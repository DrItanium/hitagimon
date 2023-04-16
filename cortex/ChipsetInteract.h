/*
hitagimon
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
// Created by jwscoggins on 5/2/21.
//

#ifndef I960SXCHIPSET_PERIPHERALS_H
#define I960SXCHIPSET_PERIPHERALS_H
#include <stdint.h>
#include "ModernCpp.h"
#include "builtins.h"
namespace cortex
{
    template<typename T>
    inline volatile T &
    memory(const uint32_t address) { return *reinterpret_cast<T *>(address); }

    namespace Devices {
        enum {
            Info = 0,
            Serial,
            Timer,
        };
    }
    namespace Groups {
        enum {
            Group0 = 0,
        };
    }

    /**
     * @brief 28-bit opcode that is converted to an address and sent to the microcontroller to be interepreted as an action
     */
    union Opcode {
        Opcode(uint32_t whole = 0) : full_(whole) { }
        Opcode(uint8_t group, uint8_t function, uint16_t subminor) : subminor(subminor), function(function), group(group) { }
        Opcode(uint8_t group, uint8_t device, uint8_t operation, uint8_t subminor) : lowest(subminor), deviceOperation(operation),
        targetDevice(device), deviceGroup(group) { }
        uint32_t full_ : 28;
        struct {
            uint32_t subminor : 16;
            uint32_t function : 8;
            uint32_t group : 4;
        };
        struct {
            uint32_t lowest : 8;
            uint32_t deviceOperation : 8;
            uint32_t targetDevice : 8;
            uint32_t deviceGroup : 4;
        };

        uint32_t makeFullAddress() const {
            return 0xF0000000 | full_;
        }
        template<typename T>
        inline volatile T& memory() {
            return *reinterpret_cast<T*>(makeFullAddress());
        }

        inline void write8(uint8_t value) { memory<uint8_t>() = value; }
        inline void write16(uint16_t value) { memory<uint16_t>() = value; }
        inline void write32(uint32_t value) { memory<uint32_t>() = value; }
        inline void write64(uint64_t value) { memory<uint64_t>() = value; }
        inline uint8_t read8() { return memory<uint8_t>(); }
        inline uint16_t read16() { return memory<uint16_t>(); }
        inline uint32_t read32() { return memory<uint32_t>(); }
        inline uint64_t read64() { return memory<uint64_t>(); }
    };

#define DefRegister(name, group, device, function, subminor) const Opcode Register_ ## name ( group, device, function, subminor)
#define Group0Register(name, device, function, subminor) DefRegister(name, Groups::Group0, device, function, subminor)
#define Group0AvailableRegister(name, device) Group0Register(name ## Available, device, 0, 0)
#define Group0SizeRegister(name, device) Group0Register(name ## Size, device, 1, 0)
#define Group0BuiltinDeviceRegisters(name, device)  \
    Group0AvailableRegister(name, device);    \
    Group0SizeRegister(name, device)
    Group0BuiltinDeviceRegisters(Info, Devices::Info);
    Group0BuiltinDeviceRegisters(Serial, Devices::Serial);
    Group0BuiltinDeviceRegisters(Timer, Devices::Timer);
#undef Group0BuiltinDeviceRegisters
#undef Group0SizeRegister
#undef Group0AvailableRegister
#undef Group0Register
#undef DefRegister
}
#endif //I960SXCHIPSET_PERIPHERALS_H
