/*
 * Copyright (C) 2024 The Android Open Source Project
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

#ifndef TNT_FILAMENT_BACKEND_CACHING_VULKANDESCRIPTORSET_H
#define TNT_FILAMENT_BACKEND_CACHING_VULKANDESCRIPTORSET_H

#include <vulkan/VulkanResourceAllocator.h>
#include <vulkan/VulkanTexture.h>
#include <vulkan/VulkanUtility.h>

#include <backend/DriverEnums.h>
#include <backend/Program.h>
#include <backend/TargetBufferInfo.h>

#include <utils/bitset.h>

#include <bluevk/BlueVK.h>
#include <tsl/robin_map.h>

namespace filament::backend {

using namespace descset;

// [UPCOMING CHANGE]: As of 03/20/24, the Filament frontend is planning to introduce descriptor set.
// This PR will arrive before that change is complete. As such, some of the methods introduced here
// will be obsolete, and certain logic will be filled out more correctly.

// Abstraction over the pool and the layout cache.
class VulkanDescriptorSetManager {
public:
    // TODO: right now we consider at most three descriptor sets defined in the shader.  This will
    // be modified after [UPCOMING_CHANGE]
    static constexpr uint8_t UNIQUE_DESCRIPTOR_SET_COUNT =
            VulkanDescriptorSetLayout::UNIQUE_DESCRIPTOR_SET_COUNT;
    using GetPipelineLayoutFunction =
            std::function<VkPipelineLayout(VulkanDescriptorSetLayoutList const&)>;

    VulkanDescriptorSetManager(VkDevice device, VulkanResourceAllocator* resourceAllocator);

    void terminate() noexcept;

    // TODO: This will be obsolete once [UPCOMING CHANGE] arrives.
    // This will write/update/bind all of the descriptor set.
    VkPipelineLayout bind(VulkanCommandBuffer* commands,
            VulkanProgram* program,
            GetPipelineLayoutFunction& getPipelineLayoutFn);

    // TODO: This will be obsolete once [UPCOMING CHANGE] arrives.
    // This is to "dynamically" bind UBOs that might have offsets changed between pipeline binding
    // and the draw call. We do this because uniforms for primitives that are part of the same
    // renderable can be stored within one uniform buffer. This can be a no-op if there were no
    // range changes between the pipeline bind and the draw call. We will re-use applicable states
    // provided within the bind() call, including the UBO descriptor set layout.
    // TODO: make it a proper dynamic binding when Filament-side descriptor changes are completed.
    void dynamicBind(VulkanCommandBuffer* commands, Handle<VulkanDescriptorSetLayout> uboLayout);

    Handle<VulkanDescriptorSetLayout> createLayout(descset::DescriptorSetLayout const& layout);

    void destroyLayout(Handle<VulkanDescriptorSetLayout> layout);

    void updateBuffer(Handle<VulkanDescriptorSet> set, uint8_t binding,
            VulkanBufferObject* bufferObject, VkDeviceSize offset, VkDeviceSize size) noexcept;

    void updateSampler(Handle<VulkanDescriptorSet> set, uint8_t binding,
            VulkanTexture* texture, VkSampler sampler) noexcept;

    void updateInputAttachment(Handle<VulkanDescriptorSet> set, VulkanAttachment attachment) noexcept;

    void clearBuffer(uint32_t bindingIndex);

    void setPlaceHolders(VkSampler sampler, VulkanTexture* texture,
            VulkanBufferObject* bufferObject) noexcept;

    void clearState() noexcept;

    // TODO: This will be filled out once [UPCOMING CHANGE] arrives.
    Handle<VulkanDescriptorSet> createSet(Handle<VulkanDescriptorSetLayout> layout) {
        return Handle<VulkanDescriptorSet>();
    }

    // TODO: This will be filled out once [UPCOMING CHANGE] arrives.
    void destroySet(Handle<VulkanDescriptorSet> set) {}

private:
    class Impl;
    Impl* mImpl;
};

}// namespace filament::backend

#endif// TNT_FILAMENT_BACKEND_CACHING_VULKANDESCRIPTORSET_H
