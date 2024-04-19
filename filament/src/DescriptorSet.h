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

#ifndef TNT_FILAMENT_DETAILS_DESCRIPTORSET_H
#define TNT_FILAMENT_DETAILS_DESCRIPTORSET_H

#include <backend/DriverApiForward.h>
#include <backend/DriverEnums.h>
#include <backend/Handle.h>

#include <utils/compiler.h>
#include <utils/bitset.h>
#include <utils/FixedCapacityVector.h>

#include "DescriptorSetLayout.h"

#include <stdint.h>

namespace filament {

class DescriptorSet {
public:
    explicit DescriptorSet(DescriptorSetLayout const& descriptorSetLayout) noexcept;

    DescriptorSet(DescriptorSet const&) = delete;
    DescriptorSet(DescriptorSet&& rhs) noexcept;
    DescriptorSet& operator=(DescriptorSet const&) = delete;
    DescriptorSet& operator=(DescriptorSet&& rhs) = delete;

    void terminate(backend::DriverApi& driver) noexcept;

    // update the descriptors if needed
    void commit(backend::DriverApi& driver) noexcept {
        if (UTILS_UNLIKELY(mDirty.any())) {
            commitSlow(driver);
        }
    }

    // bind the descriptor set
    void bind(backend::DriverApi& driver, backend::descriptor_set_t set) const noexcept;

    void bind(backend::DriverApi& driver, backend::descriptor_set_t set,
            utils::FixedCapacityVector<uint32_t> dynamicOffsets) const noexcept;

    // sets a ubo/ssbo descriptor
    void setBuffer(backend::descriptor_binding_t binding,
            backend::Handle<backend::HwBufferObject> boh,
            uint32_t offset) noexcept;

    // sets a sampler descriptor
    void setSampler(backend::descriptor_binding_t binding,
            backend::Handle<backend::HwTexture> th,
            backend::SamplerParams params) noexcept;

private:
    void commitSlow(backend::DriverApi& driver) noexcept;

    DescriptorSetLayout const& mDescriptorSetLayout;
    backend::Handle<backend::HwDescriptorSet> mDescriptorSetHandle;

    struct Desc {
        Desc() noexcept { }
        union {
            struct {
                backend::Handle<backend::HwBufferObject> boh;
                uint32_t offset;
            } buffer{};
            struct {
                backend::Handle<backend::HwTexture> th;
                backend::SamplerParams params;
            } texture;
        };
    };

    utils::FixedCapacityVector<Desc> mDescriptors;
    mutable utils::bitset32 mDirty;
};

} // namespace filament

#endif //TNT_FILAMENT_DETAILS_DESCRIPTORSET_H
