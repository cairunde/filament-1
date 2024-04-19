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

#include "DescriptorSetLayout.h"

#include "details/Engine.h"

#include <backend/DriverEnums.h>

#include <utils/BitmaskEnum.h>

#include <utility>

namespace filament {

DescriptorSetLayout::DescriptorSetLayout() noexcept = default;

DescriptorSetLayout::DescriptorSetLayout(backend::DriverApi& driver,
        backend::DescriptorSetLayout descriptorSetLayout) noexcept  {
    mDescriptorCount = descriptorSetLayout.bindings.size();
    for (auto&& desc : descriptorSetLayout.bindings) {
        if (any(desc.flags & backend::DescriptorFlags::DYNAMIC_OFFSET)) {
            mDynamicBuffers.set(desc.binding);
            mDynamicBufferCount++;
        }
        if (desc.type == backend::DescriptorType::SAMPLER) {
            mSamplerCount++;
        } else {
            mBufferCount++;
        }
    }

    mDescriptorSetLayoutHandle = driver.createDescriptorSetLayout(
            std::move(descriptorSetLayout));
}

void DescriptorSetLayout::terminate(backend::DriverApi& driver) noexcept {
    if (mDescriptorSetLayoutHandle) {
        driver.destroyDescriptorSetLayout(mDescriptorSetLayoutHandle);
    }
}

DescriptorSetLayout::DescriptorSetLayout(DescriptorSetLayout&& rhs) noexcept
    : mDescriptorSetLayoutHandle(rhs.mDescriptorSetLayoutHandle),
      mDynamicBufferCount(rhs.getDynamicBufferCount()),
      mBufferCount(rhs.mBufferCount),
      mSamplerCount(rhs.mSamplerCount),
      mDescriptorCount(rhs.mDescriptorCount) {
    rhs.mDescriptorSetLayoutHandle.clear();
}

DescriptorSetLayout& DescriptorSetLayout::operator=(DescriptorSetLayout&& rhs) noexcept {
    if (this != &rhs) {
        std::swap(mDescriptorSetLayoutHandle, rhs.mDescriptorSetLayoutHandle);
        std::swap(mDynamicBuffers, rhs.mDynamicBuffers);
        std::swap(mDynamicBufferCount, rhs.mDynamicBufferCount);
        std::swap(mBufferCount, rhs.mBufferCount);
        std::swap(mSamplerCount, rhs.mSamplerCount);
        std::swap(mDescriptorCount, rhs.mDescriptorCount);
    }
    return *this;
}

} // namespace filament
