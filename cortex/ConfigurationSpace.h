/*
hitagimon
Copyright (c) 2020-2022, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
//
// Created by jwscoggins on 7/13/22.
//

#ifndef HITAGIMON_CONFIGURATIONSPACE_H
#define HITAGIMON_CONFIGURATIONSPACE_H
#include <stdint.h>
#include "ChipsetInteract.h"
#include "ModernCpp.h"
namespace cortex
{
    namespace ConfigurationSpace
    {
/**
 * @brief A standard page that the chipset exposes to provide standard descriptions of devices in an unordred fashion
 */
        class Page
        {
        public:
            constexpr uint32_t getBaseAddress() const noexcept { return baseAddress_; }
            void setBaseAddress(uint32_t address) noexcept { baseAddress_ = address; }
            constexpr uint32_t size() const noexcept { return size_; }
            constexpr uint64_t getKind() const noexcept { return kind_; }
            constexpr uint64_t getFlags() const noexcept { return flags_; }
            constexpr bool valid() const noexcept { return valid_ != 0; }
        private:
            union
            {
                volatile uint8_t pageBytes_[256];
                volatile uint64_t pageLongWords_[256 / sizeof(uint64_t)];
                volatile uint64_t pageWords_[256 / sizeof(uint32_t)];
                struct
                {
                    volatile uint64_t kind_;
                    union
                    {
                        volatile uint64_t flags_;
                        struct
                        {
                            volatile uint64_t valid_: 1;
                        };
                    };
                    volatile uint32_t baseAddress_;
                    volatile uint32_t size_;
                };

            };
        } __attribute__((packed));
    } // end namespace ConfigurationSpace
    typedef ConfigurationSpace::Page ConfigurationSpacePage;
} // end namespace cortex
#endif //HITAGIMON_CONFIGURATIONSPACE_H
