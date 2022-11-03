/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
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
// Created by jwscoggins on 5/3/21.
//

#ifndef I960SXCHIPSET_IODEVICE_H
#define I960SXCHIPSET_IODEVICE_H
#include <stdint.h>
#include <unistd.h>
#include <string>
#include "ChipsetInteract.h"
namespace cortex
{
    class BuiltinIOBaseDevice
    {
    public:
        BuiltinIOBaseDevice(uint32_t offset);
        uint32_t getBaseAddress() const { return baseAddress_; }
        uint32_t getOffset() const { return offset_; }
    protected:
        uint32_t offset_;
        uint32_t baseAddress_;
    };

/**
 * @brief Manages the builtin led provided by the chipset
 */
    class ChipsetBasicFunctions : public BuiltinIOBaseDevice
    {
    public:
        ChipsetBasicFunctions();
        ~ChipsetBasicFunctions();
        void flush();
        uint16_t read() const;
        void write(uint16_t value);
        void write(char c);
        void write(const char *ptr);
        void writeLine();
        void writeLine(const char *ptr);
        /**
         * @brief sequential read from the console into the provided buffer
         * @param buffer the buffer to save to
         * @param nbyte the maximum number of bytes to read
         * @return number of bytes read
         */
        ssize_t read(char *buffer, size_t nbyte);
        /**
         * @brief Sequential write to the console into the provided buffer
         * @param buffer the buffer to write into
         * @param nbyte the maximum number of bytes to write
         * @return the number of bytes written
         */
        ssize_t write(char *buffer, size_t nbyte);

    private:
        uint16_t waitForLegalCharacter();
    };

    ChipsetBasicFunctions & getBasicChipsetInterface();
    inline uint32_t
    makeOrdinal(uint16_t lower, uint16_t upper) {
        return static_cast<uint32_t>(lower) | (static_cast<uint32_t>(upper) << 16);
    }
    inline uint64_t
    makeLongOrdinal(uint32_t lower, uint32_t upper) {
        return static_cast<uint64_t>(lower) | (static_cast<uint64_t>(upper) << 32);
    }
    inline uint64_t
    makeLongOrdinal(uint16_t a, uint16_t b, uint16_t c, uint16_t d) {
        return makeLongOrdinal(makeOrdinal(a, b),
                               makeOrdinal(c, d));
    }
}
#endif //I960SXCHIPSET_IODEVICE_H
