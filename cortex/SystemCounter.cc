/*
i960SxChipset
Copyright (c) 2020-2023, Joshua Scoggins
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
// Created by jwscoggins on 12/23/22.
//
#include <cortex/SystemCounter.h>
#include <cortex/IODevice.h>
#include <cortex/builtins.h>
namespace cortex {
    namespace {
        uint64_t coreSystemCounter_ = 0;
    } // end namespace
    void
    clearSystemCounter() noexcept {
        coreSystemCounter_ = 0;
    }
    uint64_t
    getSystemCounter() noexcept {
        return coreSystemCounter_;
    }
    void
    enableSystemCounter(uint16_t compare, uint8_t prescalar) noexcept {
    }
    void
    disableSystemCounter() noexcept {
    }
} // end namespace cortex

void
IncrementSystemCounter() {
    ++cortex::coreSystemCounter_;
}

ISR(INT0) {
    IncrementSystemCounter();
}
