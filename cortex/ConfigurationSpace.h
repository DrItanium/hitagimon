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
            constexpr uint8_t pageSize() const noexcept { return flags_.pageSize_; }
            constexpr uint8_t group() const noexcept { return kind_.group; }
            constexpr uint8_t subGroup() const noexcept { return kind_.subGroup; }
            constexpr uint64_t getEntireFlags() const noexcept { return flags_.raw; }
            constexpr bool valid() const noexcept { return kind_.group != 0; }
            const volatile uint32_t& getWord(uint8_t index) const noexcept { return pageWords_[index << 2]; }
            volatile uint32_t& getWord(uint8_t index) noexcept { return pageWords_[index << 2]; }
            volatile uint32_t& operator[](uint8_t index) noexcept { return getWord(index); }
            const volatile uint32_t& operator[](uint8_t index) const noexcept {return getWord(index); }

        private:
            union
            {
                volatile uint32_t pageWords_[256 / sizeof(uint32_t)];
                struct
                {
                    union
                    {
                        uint32_t raw;
                        struct
                        {
                            uint32_t group: 8;
                            uint32_t subGroup: 8;
                            uint32_t tagBits : 16;
                        };
                    } kind_;
                    union {
                        uint32_t raw;
                        struct {

                        } bits;
                    } flags_;
                    volatile uint32_t baseAddress_;
                };
            };
        } __attribute__((packed));
    } // end namespace ConfigurationSpace
    typedef ConfigurationSpace::Page ConfigurationSpacePage;
} // end namespace cortex
#endif //HITAGIMON_CONFIGURATIONSPACE_H
