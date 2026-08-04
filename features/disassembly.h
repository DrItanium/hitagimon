/*
i960SxChipset
Copyright (c) 2020-2026, Joshua Scoggins
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

#ifndef HITAGIMON_FEATURES_DIASSEMBLY_H__
#define HITAGIMON_FEATURES_DIASSEMBLY_H__

#include <string>
namespace Machine {
    /**
     * @brief do we need the second word since this is a MEMB instruction and uses optional displacement?
     * @param lo The primary instruction
     */
    bool needSecondWord(uint32_t lo) noexcept;
    /**
     * @brief return a string representation of the given encoded instruction
     * @return string representation of the given encoded instruction
     * @param lo The primary component of the opcode
     * @param hi The optional displacement
     */
    std::string disassemble(uint32_t lo, int32_t disp = 0) noexcept;

    enum class ArchitectureLevel {
        Invalid,
        Core,
        Numerics,
        Protected,
        Extended,
        NewCore,
        IAC,
        Cx,
        Decimal,
    };

    enum class Opcode : uint16_t {
#define X(code, representation, arch) Opcode_ ## representation = code , 
#include "features/opcodes.def"
#undef X
        Unknown = 0x0000,
    };
    constexpr bool valid(Opcode value) noexcept {
        switch (value) {
#define X(code, representation, arch) case Opcode:: Opcode_ ## representation :
#include "features/opcodes.def"
#undef X
            return true;
        default:
            return false;
        }
    }
    std::string toString(Opcode value) noexcept;
    ArchitectureLevel getArchitectureLevel(Opcode value) noexcept;
    constexpr bool valid(ArchitectureLevel level) noexcept {
        switch (level) {
            case ArchitectureLevel::Core:
            case ArchitectureLevel::Numerics:
            case ArchitectureLevel::Protected:
            case ArchitectureLevel::Extended:
            case ArchitectureLevel::NewCore:
            case ArchitectureLevel::IAC:
            case ArchitectureLevel::Cx:
                return true;
            default:
                return false;
        }
    }
}

#endif // end !defined HITAGIMON_FEATURES_DIASSEMBLY_H__
