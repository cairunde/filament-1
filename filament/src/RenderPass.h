/*
 * Copyright (C) 2017 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef TNT_FILAMENT_RENDERPASS_H
#define TNT_FILAMENT_RENDERPASS_H

#include "Allocators.h"

#include "details/Camera.h"
#include "details/Scene.h"

#include "private/filament/Variant.h"
#include "utils/BitmaskEnum.h"

#include <backend/DriverEnums.h>
#include <backend/Handle.h>

#include <utils/Allocator.h>
#include <utils/Range.h>
#include <utils/Slice.h>
#include <utils/architecture.h>
#include <utils/debug.h>

#include <math/mathfwd.h>

#include <functional>
#include <limits>
#include <optional>
#include <type_traits>
#include <tuple>
#include <vector>

#include <stddef.h>
#include <stdint.h>

namespace filament {

namespace backend {
class CommandBufferQueue;
}

class FMaterialInstance;
class FRenderPrimitive;
class RenderPassBuilder;

class RenderPass {
public:
    /*
     *   Command key encoding
     *   --------------------
     *
     *   CC    = Channel
     *   PP    = Pass
     *   a     = alpha masking
     *   ppp   = priority
     *   t     = two-pass transparency ordering
     *   0     = reserved, must be zero
     *
     *
     * TODO: we need to add a "primitive id" in the low-bits of material-id, so that
     *       auto-instancing can work better
     *
     *   DEPTH command (b00)
     *   |  |  | 2| 2| 2|1| 3 | 2|  6   |   10     |               32               |
     *   +--+--+--+--+--+-+---+--+------+----------+--------------------------------+
     *   |CC|00|00|01|00|0|ppp|00|000000| Z-bucket |          material-id           |
     *   +--+--+--+--+--+-+---+--+------+----------+--------------------------------+
     *   | correctness        |      optimizations (truncation allowed)             |
     *
     *
     *   COLOR (b01) and REFRACT (b10) commands
     *   |  | 2| 2| 2| 2|1| 3 | 2|  6   |   10     |               32               |
     *   +--+--+--+--+--+-+---+--+------+----------+--------------------------------+
     *   |CC|00|01|01|00|a|ppp|00|000000| Z-bucket |          material-id           |
     *   |CC|00|10|01|00|a|ppp|00|000000| Z-bucket |          material-id           | refraction
     *   +--+--+--+--+--+-+---+--+------+----------+--------------------------------+
     *   | correctness        |      optimizations (truncation allowed)             |
     *
     *
     *   BLENDED command (b11)
     *   | 2| 2| 2| 2| 2|1| 3 | 2|              32                |         15    |1|
     *   +--+--+--+--+--+-+---+--+--------------------------------+---------------+-+
     *   |CC|00|11|01|00|0|ppp|00|         ~distanceBits          |   blendOrder  |t|
     *   +--+--+--+--+--+-+---+--+--------------------------------+---------------+-+
     *   | correctness                                                              |
     *
     *
     *   pre-CUSTOM command
     *   | 2| 2| 2| 2| 2|         22           |               32               |
     *   +--+--+--+--+--+----------------------+--------------------------------+
     *   |CC|00|PP|00|00|        order         |      custom command index      |
     *   +--+--+--+--+--+----------------------+--------------------------------+
     *   | correctness                                                          |
     *
     *
     *   post-CUSTOM command
     *   | 2| 2| 2| 2| 2|         22           |               32               |
     *   +--+--+--+--+--+----------------------+--------------------------------+
     *   |CC|00|PP|11|00|        order         |      custom command index      |
     *   +--+--+--+--+--+----------------------+--------------------------------+
     *   | correctness                                                          |
     *
     *
     *   SENTINEL command
     *   |                                   64                                  |
     *   +--------.--------.--------.--------.--------.--------.--------.--------+
     *   |11111111 11111111 11111111 11111111 11111111 11111111 11111111 11111111|
     *   +-----------------------------------------------------------------------+
     */
    //using CommandKey = uint64_t;

    //公有的channel commandType passType 
    //alphaMasking,priority  Z-bucket,material-id
    //priority               order,customCommandIndex
    //t                      distanceBits,blendOrder

    struct ColorDepthRefractKey {
        uint32_t priority : 3;
        uint32_t alphaMasking : 1;
        uint32_t zBucket : 10;
        uint32_t reserved : 18;

        uint16_t secondOrder;
        uint16_t secondLayer;

        uint32_t materialId;
    }; 

    static_assert(sizeof(ColorDepthRefractKey) == 12);

    struct BlendedKey {
        uint32_t priority : 3;
        uint32_t blenderOrder : 15;
        uint32_t twoPassOrder : 1;
        uint32_t reserved : 13;

        uint16_t secondOrder;
        uint16_t secondLayer;

        uint32_t distanceBits;
    }; 

    static_assert(sizeof(BlendedKey) == 12);

    struct CustomKey {
        uint32_t order : 22;
        uint32_t reserved : 10;
        uint32_t commandIndex;

        uint32_t reserved2;
    }; 

    static_assert(sizeof(CustomKey) == 12);

    struct CommandKey {
        uint16_t channel;
        uint16_t commandType;
        uint16_t passType;
        uint16_t renderQueue;
        uint16_t order;
        uint16_t layar;

        union {
            ColorDepthRefractKey colorDepthRefractKey;
            BlendedKey blendedKey;
            CustomKey customKey;
        };
    };

    static_assert(sizeof(CommandKey) == 24);

    static constexpr CommandKey DEFAULT_KEY = {
        .channel = 0,
        .commandType = 0,
        .passType = 0,
        .renderQueue = 0,
        .order = 0,
        .layar = 0,
        .colorDepthRefractKey = {
            .alphaMasking = 0,
            .priority = 0,
            .zBucket = 0,
            .materialId = 0,
            .reserved = 0,
            .secondLayer = 0,
            .secondOrder = 0
        }
    };

    static constexpr uint64_t BLEND_ORDER_MASK              = 0xFFFEllu;
    static constexpr unsigned BLEND_ORDER_SHIFT             = 1;

    static constexpr uint64_t BLEND_TWO_PASS_MASK           = 0x1llu;
    static constexpr unsigned BLEND_TWO_PASS_SHIFT          = 0;

    static constexpr uint64_t MATERIAL_INSTANCE_ID_MASK     = 0x00000FFFllu;
    static constexpr unsigned MATERIAL_INSTANCE_ID_SHIFT    = 0;

    static constexpr uint64_t MATERIAL_VARIANT_KEY_MASK     = 0x000FF000llu;
    static constexpr unsigned MATERIAL_VARIANT_KEY_SHIFT    = 12;

    static constexpr uint64_t MATERIAL_ID_MASK              = 0xFFF00000llu;
    static constexpr unsigned MATERIAL_ID_SHIFT             = 20;

    static constexpr uint64_t BLEND_DISTANCE_MASK           = 0xFFFFFFFF0000llu;
    static constexpr unsigned BLEND_DISTANCE_SHIFT          = 16;

    static constexpr uint64_t MATERIAL_MASK                 = 0xFFFFFFFFllu;
    static constexpr unsigned MATERIAL_SHIFT                = 0;

    static constexpr uint64_t Z_BUCKET_MASK                 = 0x3FF00000000llu;
    static constexpr unsigned Z_BUCKET_SHIFT                = 32;

    static constexpr uint64_t PRIORITY_MASK                 = 0x001C000000000000llu;
    static constexpr unsigned PRIORITY_SHIFT                = 50;

    static constexpr uint64_t BLENDING_MASK                 = 0x0020000000000000llu;
    static constexpr unsigned BLENDING_SHIFT                = 53;

    static constexpr uint64_t CUSTOM_MASK                   = 0x0300000000000000llu;
    static constexpr unsigned CUSTOM_SHIFT                  = 56;

    static constexpr uint64_t PASS_MASK                     = 0x0C00000000000000llu;
    static constexpr unsigned PASS_SHIFT                    = 58;

    static constexpr uint64_t CHANNEL_MASK                  = 0xC000000000000000llu;
    static constexpr unsigned CHANNEL_SHIFT                 = 62;

    static constexpr uint64_t CUSTOM_ORDER_MASK             = 0x003FFFFF00000000llu;
    static constexpr unsigned CUSTOM_ORDER_SHIFT            = 32;

    static constexpr uint64_t CUSTOM_INDEX_MASK             = 0x00000000FFFFFFFFllu;
    static constexpr unsigned CUSTOM_INDEX_SHIFT            = 0;

    // we assume Variant fits in 8-bits.
    static_assert(sizeof(Variant::type_t) == 1);

    enum class Pass : uint16_t {    // 6-bits max
        DEPTH    = uint16_t(0x0000),
        COLOR    = uint16_t(0x0001),
        REFRACT  = uint16_t(0x0002),
        BLENDED  = uint16_t(0x0003),
        SENTINEL = uint16_t(0xFFFF)
    };

    enum class CustomCommand : uint16_t {    // 2-bits max
        PROLOG  = uint16_t(0x0),
        PASS    = uint16_t(0x1),
        EPILOG  = uint16_t(0x2)
    };

    enum CommandSortingCriteria : uint32_t{
        SORT_NONE = 0,
        SORT_ORDER = (1 << 0),
        SORT_LAYER = (1 << 1),
        SORT_RENDER_QUEUE = (1 << 2),
        SORT_CHANNEL = (1 << 3),
        SORT_COMMAND_TYPE = (1 << 4),
        SORT_PASS_TYPE = (1 << 5),

        SORT_ALPHA_MASKING = (1 << 6),
        SORT_PRIORITY = (1 << 7),
        SORT_ZBUCKET = (1 << 8),
        SORT_SECOND_ORDER = (1 << 9),
        SORT_SECOND_LAYAR = (1 << 10),
        SORT_MATERIAL_ID = (1 << 11),

        SORT_BLENDER_ORDER = (1 << 12),
        SORT_TWO_PASS_ORDER = (1 << 13),
        SORT_DISTANCE_BITS = (1 << 14),

        SORT_CUSTOM_ORDER = (1 << 15),
        SORT_COMMAN_INDEX = (1 << 16),

        SORT_COMMON = SORT_ORDER | SORT_LAYER | SORT_RENDER_QUEUE | SORT_CHANNEL | SORT_COMMAND_TYPE | SORT_PASS_TYPE,
        SORT_COLOR_DEPTH_REFRACT_CMD = SORT_COMMON | SORT_ALPHA_MASKING | SORT_PRIORITY | SORT_ZBUCKET | SORT_SECOND_ORDER | SORT_SECOND_LAYAR | SORT_MATERIAL_ID,
        SORT_BLENDER_CMD = SORT_COMMON | SORT_PRIORITY | SORT_BLENDER_ORDER | SORT_TWO_PASS_ORDER | SORT_SECOND_ORDER | SORT_SECOND_LAYAR | SORT_DISTANCE_BITS,
        SORT_CUSTOM_CMD = SORT_COMMON | SORT_CUSTOM_ORDER | SORT_COMMAN_INDEX
    };

    static bool HasFlag(const CommandSortingCriteria flags, const CommandSortingCriteria flagToTest) { 
        return ((uint32_t)flags & (uint32_t)flagToTest) != 0;
    }

    enum class CommandTypeFlags : uint32_t {
        COLOR = 0x1,    // generate the color pass only
        DEPTH = 0x2,    // generate the depth pass only ( e.g. shadowmap)

        // shadow-casters are rendered in the depth buffer, regardless of blending (or alpha masking)
        DEPTH_CONTAINS_SHADOW_CASTERS = 0x4,
        // alpha-tested objects are not rendered in the depth buffer
        DEPTH_FILTER_ALPHA_MASKED_OBJECTS = 0x08,

        // alpha-blended objects are not rendered in the depth buffer
        FILTER_TRANSLUCENT_OBJECTS = 0x10,

        // generate commands for shadow map
        SHADOW = DEPTH | DEPTH_CONTAINS_SHADOW_CASTERS,
        // generate commands for SSAO
        SSAO = DEPTH | FILTER_TRANSLUCENT_OBJECTS,
        // generate commands for screen-space reflections
        SCREEN_SPACE_REFLECTIONS = COLOR | FILTER_TRANSLUCENT_OBJECTS
    };

    /*
     * The sorting material key is 32 bits and encoded as:
     *
     * |     12     |   8    |     12     |
     * +------------+--------+------------+
     * |  material  |variant |  instance  |
     * +------------+--------+------------+
     *
     * The variant is inserted while building the commands, because we don't know it before that
     */
    static uint64_t makeMaterialSortingKey(uint32_t materialId, uint32_t instanceId) noexcept {
        uint64_t const key = ((materialId << MATERIAL_ID_SHIFT) & MATERIAL_ID_MASK) |
                         ((instanceId << MATERIAL_INSTANCE_ID_SHIFT) & MATERIAL_INSTANCE_ID_MASK);
        return (key << MATERIAL_SHIFT) & MATERIAL_MASK;
    }

    template<typename T>
    static uint64_t makeField(T value, uint64_t mask, unsigned shift) noexcept {
        assert_invariant(!((uint64_t(value) << shift) & ~mask));
        return uint64_t(value) << shift;
    }

    template<typename T>
    static uint64_t select(T boolish) noexcept {
        return boolish ? std::numeric_limits<uint64_t>::max() : uint64_t(0);
    }

    template<typename T>
    static uint64_t select(T boolish, uint64_t value) noexcept {
        return boolish ? value : uint64_t(0);
    }

    template<typename T>
    static uint16_t select16(T boolish) noexcept {
        return boolish ? std::numeric_limits<uint16_t>::max() : uint16_t(0);
    }

    struct PrimitiveInfo { // 56 bytes
        union {
            FRenderPrimitive const* primitive;                          // 8 bytes;
            uint64_t padding = {}; // ensures primitive is 8 bytes on all archs
        };                                                              // 8 bytes
        uint64_t rfu0;                                                  // 8 bytes
        backend::RasterState rasterState;                               // 4 bytes
        backend::Handle<backend::HwBufferObject> skinningHandle;        // 4 bytes
        backend::Handle<backend::HwSamplerGroup> skinningTexture;       // 4 bytes
        backend::Handle<backend::HwBufferObject> morphWeightBuffer;     // 4 bytes
        backend::Handle<backend::HwSamplerGroup> morphTargetBuffer;     // 4 bytes
        backend::Handle<backend::HwBufferObject> instanceBufferHandle;  // 4 bytes
        uint32_t index = 0;                                             // 4 bytes
        uint32_t skinningOffset = 0;                                    // 4 bytes
        uint16_t instanceCount;                                         // 2 bytes [MSb: user]
        Variant materialVariant;                                        // 1 byte
        uint8_t rfu1;                                                   // 1 byte
        uint32_t rfu2;                                                  // 4 byte

        static const uint16_t USER_INSTANCE_MASK = 0x8000u;
        static const uint16_t INSTANCE_COUNT_MASK = 0x7fffu;
    };
    static_assert(sizeof(PrimitiveInfo) == 56);

    struct alignas(8) Command {     // 64 bytes
        CommandKey key = DEFAULT_KEY;
        CommandSortingCriteria sortingCriteria;
        //CommandKey key = 0;         //  8 bytes
        PrimitiveInfo primitive;    // 56 bytes
        //bool operator < (Command const& rhs) const noexcept { return key < rhs.key; }
        //crd
        bool operator < (Command const& rhs) const noexcept {
            CommandSortingCriteria criteria = static_cast<CommandSortingCriteria>((uint32_t)sortingCriteria & (uint32_t)rhs.sortingCriteria);

            if(HasFlag(criteria, SORT_ORDER)) { 
                if(key.order != rhs.key.order) {
                    return key.order < rhs.key.order;
                }
            }

            if(HasFlag(criteria, SORT_LAYER)) {
                if(key.layar != rhs.key.layar) {
                    return key.layar < rhs.key.layar;
                }
            }

            if(HasFlag(criteria, SORT_RENDER_QUEUE)) {
                if(key.renderQueue != rhs.key.renderQueue) {
                    return key.renderQueue < rhs.key.renderQueue;
                }
            }

            if(HasFlag(criteria, SORT_CHANNEL)) {
                if(key.channel != rhs.key.channel) {
                    return key.channel < rhs.key.channel;
                }
            }

            if(HasFlag(criteria, SORT_PASS_TYPE)) {
                if(key.passType != rhs.key.passType) {
                    return key.passType < rhs.key.passType;
                }
            }

            if(HasFlag(criteria, SORT_COMMAND_TYPE)) {
                if(key.commandType != rhs.key.commandType) {
                    return key.commandType < rhs.key.commandType;
                }
            }

            if(key.commandType != uint16_t(CustomCommand::PASS)) {
                if(key.customKey.order != rhs.key.customKey.order) { 
                    return key.customKey.order < rhs.key.customKey.order;
                }

                return key.customKey.commandIndex < rhs.key.customKey.commandIndex;
            }
            else {
                if(key.blendedKey.priority != rhs.key.blendedKey.priority) {
                    return key.blendedKey.priority < rhs.key.blendedKey.priority;
                }

                if(key.blendedKey.secondOrder != rhs.key.blendedKey.secondOrder) {
                    return key.blendedKey.secondOrder < rhs.key.blendedKey.secondOrder;
                }

                if(key.blendedKey.secondLayer != rhs.key.blendedKey.secondLayer) {
                    return key.blendedKey.secondLayer < rhs.key.blendedKey.secondLayer;
                }

                if(key.passType == uint16_t(Pass::BLENDED)) {
                    if(key.blendedKey.distanceBits != rhs.key.blendedKey.distanceBits) {
                        return key.blendedKey.distanceBits < rhs.key.blendedKey.distanceBits;
                    }

                    if(key.blendedKey.blenderOrder != rhs.key.blendedKey.blenderOrder) {
                        return key.blendedKey.blenderOrder < rhs.key.blendedKey.blenderOrder;
                    }

                    return key.blendedKey.twoPassOrder < rhs.key.blendedKey.twoPassOrder;
                }
                else {
                    if(key.colorDepthRefractKey.zBucket != rhs.key.colorDepthRefractKey.zBucket) {
                        return key.colorDepthRefractKey.zBucket < rhs.key.colorDepthRefractKey.zBucket;
                    }

                    return key.colorDepthRefractKey.materialId < rhs.key.colorDepthRefractKey.materialId;
                }
            }
        }
        
        // placement new declared as "throw" to avoid the compiler's null-check
        inline void* operator new (size_t, void* ptr) {
            assert_invariant(ptr);
            return ptr;
        }
    };
    static_assert(sizeof(Command) == 88); 
    static_assert(std::is_trivially_destructible_v<Command>,
            "Command isn't trivially destructible");

    using RenderFlags = uint8_t;
    static constexpr RenderFlags HAS_SHADOWING             = 0x01;
    static constexpr RenderFlags HAS_INVERSE_FRONT_FACES   = 0x02;
    static constexpr RenderFlags IS_INSTANCED_STEREOSCOPIC = 0x04;

    // Arena used for commands
    using Arena = utils::Arena<
            utils::LinearAllocatorWithFallback,
            utils::LockingPolicy::NoLock,
            utils::TrackingPolicy::HighWatermark,
            utils::AreaPolicy::StaticArea>;

    // RenderPass can only be moved
    RenderPass(RenderPass&& rhs) = default;

    // RenderPass can't be copied
    RenderPass(RenderPass const& rhs) = delete;
    RenderPass& operator=(RenderPass const& rhs) = delete;
    RenderPass& operator=(RenderPass&& rhs) = delete;

    // allocated commands ARE NOT freed, they're owned by the Arena
    ~RenderPass() noexcept;

    Command const* begin() const noexcept { return mCommandBegin; }
    Command const* end() const noexcept { return mCommandEnd; }
    bool empty() const noexcept { return begin() == end(); }

    // Helper to execute all the commands generated by this RenderPass
    static void execute(RenderPass const& pass,
            FEngine& engine, const char* name,
            backend::Handle<backend::HwRenderTarget> renderTarget,
            backend::RenderPassParams params) noexcept;

    /*
     * Executor holds the range of commands to execute for a given pass
     */
    class Executor {
        using CustomCommandFn = std::function<void()>;
        friend class RenderPass;
        friend class RenderPassBuilder;

        // these fields are constant after creation
        utils::Slice<Command> mCommands;
        utils::Slice<CustomCommandFn> mCustomCommands;
        backend::Handle<backend::HwBufferObject> mUboHandle;
        backend::Handle<backend::HwBufferObject> mInstancedUboHandle;
        backend::Viewport mScissorViewport;

        backend::Viewport mScissor{};            // value of scissor override
        backend::PolygonOffset mPolygonOffset{}; // value of the override
        bool mPolygonOffsetOverride : 1;         // whether to override the polygon offset setting
        bool mScissorOverride : 1;               // whether to override the polygon offset setting

        Executor(RenderPass const* pass, Command const* b, Command const* e) noexcept;

        void execute(FEngine& engine, const Command* first, const Command* last) const noexcept;

        static backend::Viewport applyScissorViewport(
                backend::Viewport const& scissorViewport,
                backend::Viewport const& scissor) noexcept;

    public:
        Executor() = default;
        Executor(Executor const& rhs);
        Executor& operator=(Executor const& rhs) = default;
        ~Executor() noexcept;

        // if non-null, overrides the material's polygon offset
        void overridePolygonOffset(backend::PolygonOffset const* polygonOffset) noexcept;

        // if non-null, overrides the material's scissor
        void overrideScissor(backend::Viewport const* scissor) noexcept;
        void overrideScissor(backend::Viewport const& scissor) noexcept;

        void execute(FEngine& engine, const char* name) const noexcept;
    };

    // returns a new executor for this pass
    Executor getExecutor() const {
        return { this, mCommandBegin, mCommandEnd };
    }

    Executor getExecutor(Command const* b, Command const* e) const {
        return { this, b, e };
    }

private:
    friend class FRenderer;
    friend class RenderPassBuilder;
    RenderPass(FEngine& engine, RenderPassBuilder const& builder) noexcept;

    // This is the main function of this class, this appends commands to the pass using
    // the current camera, geometry and flags set. This can be called multiple times if needed.
    void appendCommands(FEngine& engine,
            utils::Slice<Command> commands, CommandTypeFlags commandTypeFlags) noexcept;

    // Appends a custom command.
    void appendCustomCommand(Command* commands,
            uint8_t channel, Pass pass, CustomCommand custom, uint32_t order,
            Executor::CustomCommandFn command);

    void resize(Arena& arena, size_t count) noexcept;

    // sorts commands then trims sentinels
    void sortCommands(Arena& arena) noexcept;

    // instanceify commands then trims sentinels
    void instanceify(FEngine& engine, Arena& arena) noexcept;

    // We choose the command count per job to minimize JobSystem overhead.
    // On a Pixel 4, 2048 commands is about half a millisecond of processing.
    static constexpr size_t JOBS_PARALLEL_FOR_COMMANDS_COUNT = 2048;
    static constexpr size_t JOBS_PARALLEL_FOR_COMMANDS_SIZE  =
            sizeof(Command) * JOBS_PARALLEL_FOR_COMMANDS_COUNT;

    static_assert(JOBS_PARALLEL_FOR_COMMANDS_SIZE % utils::CACHELINE_SIZE == 0,
            "Size of Commands jobs must be multiple of a cache-line size");

    static inline void generateCommands(CommandTypeFlags commandTypeFlags, Command* commands,
            FScene::RenderableSoa const& soa, utils::Range<uint32_t> range,
            Variant variant, RenderFlags renderFlags,
            FScene::VisibleMaskType visibilityMask,
            math::float3 cameraPosition, math::float3 cameraForward,
            uint8_t instancedStereoEyeCount) noexcept;

    template<RenderPass::CommandTypeFlags commandTypeFlags>
    static inline Command* generateCommandsImpl(RenderPass::CommandTypeFlags extraFlags, Command* curr,
            FScene::RenderableSoa const& soa, utils::Range<uint32_t> range,
            Variant variant, RenderFlags renderFlags, FScene::VisibleMaskType visibilityMask,
            math::float3 cameraPosition, math::float3 cameraForward,
            uint8_t instancedStereoEyeCount) noexcept;

    static void setupColorCommand(Command& cmdDraw, Variant variant,
            FMaterialInstance const* mi, bool inverseFrontFaces) noexcept;

    static void updateSummedPrimitiveCounts(
            FScene::RenderableSoa& renderableData, utils::Range<uint32_t> vr) noexcept;


    FScene::RenderableSoa const& mRenderableSoa;
    utils::Range<uint32_t> const mVisibleRenderables;
    backend::Handle<backend::HwBufferObject> const mUboHandle;
    math::float3 const mCameraPosition;
    math::float3 const mCameraForwardVector;
    RenderFlags const mFlags;
    Variant const mVariant;
    FScene::VisibleMaskType const mVisibilityMask;
    backend::Viewport const mScissorViewport;

    // Pointer to the first command
    Command* mCommandBegin = nullptr;
    // Pointer to one past the last command
    Command* mCommandEnd = nullptr;
    // a UBO for instanced primitives
    backend::Handle<backend::HwBufferObject> mInstancedUboHandle;
    // a vector for our custom commands
    using CustomCommandVector = std::vector<Executor::CustomCommandFn,
            utils::STLAllocator<Executor::CustomCommandFn, LinearAllocatorArena>>;
    mutable CustomCommandVector mCustomCommands;
};

class RenderPassBuilder {
    friend class RenderPass;

    RenderPass::Arena& mArena;
    RenderPass::CommandTypeFlags mCommandTypeFlags{};
    backend::Viewport mScissorViewport{ 0, 0, INT32_MAX, INT32_MAX };
    FScene::RenderableSoa const* mRenderableSoa = nullptr;
    utils::Range<uint32_t> mVisibleRenderables{};
    backend::Handle<backend::HwBufferObject> mUboHandle;
    math::float3 mCameraPosition{};
    math::float3 mCameraForwardVector{};
    RenderPass::RenderFlags mFlags{};
    Variant mVariant{};
    FScene::VisibleMaskType mVisibilityMask = std::numeric_limits<FScene::VisibleMaskType>::max();

    using CustomCommandRecord = std::tuple<
            uint8_t,
            RenderPass::Pass,
            RenderPass::CustomCommand,
            uint32_t,
            RenderPass::Executor::CustomCommandFn>;

    using CustomCommandContainer = std::vector<CustomCommandRecord,
            utils::STLAllocator<CustomCommandRecord, LinearAllocatorArena>>;

    // we make this optional because it's not used often, and we don't want to have
    // to construct it by default.
    std::optional<CustomCommandContainer> mCustomCommands;

public:
    explicit RenderPassBuilder(RenderPass::Arena& arena) : mArena(arena) { }

    RenderPassBuilder& commandTypeFlags(RenderPass::CommandTypeFlags commandTypeFlags) noexcept {
        mCommandTypeFlags = commandTypeFlags;
        return *this;
    }

    RenderPassBuilder& scissorViewport(backend::Viewport viewport) noexcept {
        mScissorViewport = viewport;
        return *this;
    }

    // specifies the geometry to generate commands for
    RenderPassBuilder& geometry(FScene::RenderableSoa const& soa, utils::Range<uint32_t> vr,
            backend::Handle<backend::HwBufferObject> uboHandle) noexcept {
        mRenderableSoa = &soa;
        mVisibleRenderables = vr;
        mUboHandle = uboHandle;
        return *this;
    }

    // Specifies camera information (e.g. used for sorting commands)
    RenderPassBuilder& camera(const CameraInfo& camera) noexcept {
        mCameraPosition = camera.getPosition();
        mCameraForwardVector = camera.getForwardVector();
        return *this;
    }

    //  flags controlling how commands are generated
    RenderPassBuilder& renderFlags(RenderPass::RenderFlags flags) noexcept {
        mFlags = flags;
        return *this;
    }

    // like above but allows to set specific flags
    RenderPassBuilder& renderFlags(
            RenderPass::RenderFlags mask, RenderPass::RenderFlags value) noexcept {
        mFlags = (mFlags & mask) | (value & mask);
        return *this;
    }

    // variant to use
    RenderPassBuilder& variant(Variant variant) noexcept {
        mVariant = variant;
        return *this;
    }

    // Sets the visibility mask, which is AND-ed against each Renderable's VISIBLE_MASK to
    // determine if the renderable is visible for this pass.
    // Defaults to all 1's, which means all renderables in this render pass will be rendered.
    RenderPassBuilder& visibilityMask(FScene::VisibleMaskType mask) noexcept {
        mVisibilityMask = mask;
        return *this;
    }

    RenderPassBuilder& customCommand(FEngine& engine,
            uint8_t channel,
            RenderPass::Pass pass,
            RenderPass::CustomCommand custom,
            uint32_t order,
            const RenderPass::Executor::CustomCommandFn& command);

    RenderPass build(FEngine& engine);
};


} // namespace filament

template<> struct utils::EnableBitMaskOperators<filament::RenderPass::CommandTypeFlags>
        : public std::true_type {};

#endif // TNT_FILAMENT_RENDERPASS_H
