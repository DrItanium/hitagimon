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
    enum InstructionClass {
#define X(name, pattern) name = pattern ,
#include "classes.def"
#undef X
    };
    struct Opcode {
    public:
        explicit Opcode(uint16_t opcode, const std::string& name, InstructionClass ic) noexcept : 
            _opcode(opcode), 
            _name(name), 
            _class(ic) { }
        constexpr auto getOpcode() const noexcept { return _opcode; }
        constexpr const std::string& getName() const noexcept { return _name; }
        constexpr auto getClass() const noexcept { return _class; }
    private:
        uint16_t _opcode;
        std::string _name;
        InstructionClass _class;
    };
#if 0
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
#define X(name, group) name ,
#include "levels.def"
#undef X

    };
    constexpr ArchitectureLevel translateToGroup(ArchitectureLevel level) noexcept {
        switch (level) {
#define X(name, group) case ArchitectureLevel :: name : return ArchitectureLevel :: group ;
#include "levels.def"
#undef X
            default:
                return ArchitectureLevel::Invalid;
        }
    }
    constexpr bool isNewCore(ArchitectureLevel level) noexcept {
        return translateToGroup(level) == ArchitectureLevel::NewCore;
    }
    constexpr bool isCore(ArchitectureLevel level) noexcept {
        return translateToGroup(level) == ArchitectureLevel::Core;
    }
    constexpr bool isNumerics(ArchitectureLevel level) noexcept {
        return translateToGroup(level) == ArchitectureLevel::Numerics;
    }
    constexpr bool valid(ArchitectureLevel level) noexcept {
        switch (level) {
#define X(name, group) case ArchitectureLevel :: name :
#include "levels.def"
#undef X
            return true;
            default: 
            return false;
        }
    }
    enum class TreatSrcDestAs {
        NotApplicable,
        Src,
        Dest,
        SrcDest,
    };

    enum class Opcode : uint16_t {
#define X(code, representation, arch, group) Opcode_ ## representation = code , 
#include "features/opcodes.def"
#undef X
        Unknown = 0x0000,
    };
    constexpr bool valid(Opcode value) noexcept {
        switch (value) {
#define X(code, representation, arch, group) case Opcode:: Opcode_ ## representation :
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
        return Machine::valid(level);
    }
#endif
}

#endif // end !defined HITAGIMON_FEATURES_DIASSEMBLY_H__
