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
#include "IAC.h"
namespace cortex {
    namespace ChipsetBasicFunctions {
        namespace Console {
            void flush();
            uint16_t read();
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
        } // end namespace Console
        namespace Timer {
            /**
             * @brief Get the unixtime of the system
             * @return the unixtime as an unsigned 32-bit number
             */
            uint32_t unixtime() noexcept;
            /**
             * @brief Return the amount of time elapsed since
             */
            uint32_t secondstime() noexcept;
            uint32_t millis() noexcept;
            uint32_t micros() noexcept;
            float getRTCTemperature() noexcept;

        }
        namespace Info {
            uint32_t getCPUClockSpeed() noexcept;
            uint32_t getChipsetClockSpeed() noexcept;
        } // end namespace Info
        namespace OLED {
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2);
            void command(uint16_t cmd, uint16_t arg0);
        }
        namespace Random {
            uint32_t getHardwareRandomNumber() noexcept;
        }
        void begin() noexcept;
    } // end namespace ChipsetBasicFunctions
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
    struct IOMemoryBlock {
        IOMemoryBlock(uint8_t* address, uint16_t capacity) : _address(address), _capacity(capacity), _mask(capacity - 1) { }
        uint16_t capacity() const noexcept { return _capacity; }
        uint8_t read(uint16_t address) const noexcept { return _address[address & _mask]; }
        void write(uint16_t address, uint8_t value) noexcept { _address[address & _mask] = value; }
        uint8_t* data() const noexcept { return _address; }
    private:
        uint8_t* _address;
        uint16_t _capacity;
        uint16_t _mask;
    };
    IOMemoryBlock& EEPROM() noexcept;
    IOMemoryBlock& SRAM() noexcept;

}
#endif //I960SXCHIPSET_IODEVICE_H
