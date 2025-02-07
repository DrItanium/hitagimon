/*
hitagimon
Copyright (c) 2020-2024, Joshua Scoggins
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
// Created by jwscoggins on 3/1/24.
//

#ifndef HITAGIMON_SOFTWAREDMA_H
#define HITAGIMON_SOFTWAREDMA_H
#include <stdint.h>
#include <cortex/ModernCpp.h>
namespace cortex {
    /**
     * Use interrupts to transfer some amount of 32-bit words per interrupt from one location to another
     */
    class DMARequest {
        uint8_t* source;
        uint8_t* destination;
        uint32_t totalBytesToCopy;
        uint32_t numberOfPacketsToTransferPerOperation;
        uint32_t mask;
    public:
        DMARequest(uint8_t* src, uint8_t* dest, uint32_t tbc, uint32_t npackets, uint32_t m) : source(src), destination(dest), totalBytesToCopy(tbc), numberOfPacketsToTransferPerOperation(npackets), mask(m) { }
        constexpr uint8_t* getSource() const noexcept { return source; }
        constexpr uint8_t* getDestination() const noexcept { return destination; }
        constexpr uint32_t getTotalBytesToCopy() const noexcept { return totalBytesToCopy; }
        constexpr uint32_t getNumberOfPacketsToTransferPerOperation() const noexcept { return numberOfPacketsToTransferPerOperation; }
        constexpr uint32_t getMask() const noexcept { return mask; }
    public:
    };
} // end namespace cortex
#endif //HITAGIMON_SOFTWAREDMA_H
