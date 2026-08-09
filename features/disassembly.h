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
    enum class InstructionClass {
#define X(name, pattern) name = pattern ,
#include "classes.def"
#undef X
    };
    enum class InstructionFormat {
        CTRL,
        COBR,
        REG,
        MEM1,
        MEM2,
        MEM4,
        MEM8,
        MEM12,
        MEM16,
        FBRA,
        CALLJ,
    };
    enum ModeMasks {
        M1 = 0x0800,
        M2 = 0x1000,
        M3 = 0x2000,
    };
    enum class CoreInstructionKind {
        CTRL,
        COBR,
        REG,
        MEM,
    };
    constexpr auto decode(uint8_t value) noexcept {
        if (value >= 0x80) {
            return CoreInstructionKind::MEM;
        } else if (value >= 0x40) {
            return CoreInstructionKind::REG;
        } else if (value >= 0x20) {
            return CoreInstructionKind::COBR;
        } else {
            return CoreInstructionKind::CTRL;
        }
    }
    union OperandDescriptor {
    public:
        constexpr OperandDescriptor(uint8_t align, bool lit, bool fp, bool sfr) : _align(align), _lit(lit), _fp(fp), _sfr(sfr), _unused(0), _treatAsFields(1) { }
        constexpr OperandDescriptor(uint8_t value) : _raw(value) { }
        constexpr auto getValue() const noexcept { return _raw; }
        constexpr auto getAlignment() const noexcept { return _align; }
        constexpr auto literalsAllowed() const noexcept { return _lit; }
        constexpr auto floatingPointRegistersAllowed() const noexcept { return _fp; }
        constexpr auto specialFunctionRegistersAllowed() const noexcept { return _sfr; }
        constexpr auto fieldsAreMeaningful() const noexcept { return _treatAsFields; }
        constexpr auto enabled() const noexcept { return fieldsAreMeaningful() || _raw != 0; }
        explicit constexpr operator bool() const noexcept { return enabled(); }
    private:
        uint8_t _raw;
        struct {
            uint8_t _align : 2;
            uint8_t _lit : 1;
            uint8_t _fp : 1;
            uint8_t _sfr : 1;
            uint8_t _unused : 2;
            uint8_t _treatAsFields : 1;
        };
    };
    constexpr OperandDescriptor UnusedField { 0 };
    constexpr OperandDescriptor Memory { 0x7f };
    constexpr OperandDescriptor R { 0, false, false, false };
    constexpr OperandDescriptor RS { 0, false, false, true };
    constexpr OperandDescriptor RL { 0, true, false, true };
    constexpr OperandDescriptor RSL { 0, true, false, true };
    constexpr OperandDescriptor F { 0, 0, true, 0 };
    constexpr OperandDescriptor FL { 0, true, true, 0 };
    constexpr OperandDescriptor R2 { 1, false, false, false };
    constexpr OperandDescriptor RL2 { 1, true, false, false };
    constexpr OperandDescriptor F2 { 1, false, true, false };
    constexpr OperandDescriptor FL2 { 1, true, true, false };
    constexpr OperandDescriptor R4 { 3, false, false, false };
    constexpr OperandDescriptor RL4 { 3, true, false, false };
    constexpr OperandDescriptor F4 { 3, false, true, false };
    constexpr OperandDescriptor FL4 { 3, true, true, false };
    struct Opcode {
    public:
        explicit Opcode(uint32_t opcode, const std::string& name, InstructionClass ic, InstructionFormat format, uint8_t operandCount, OperandDescriptor src1 = R, OperandDescriptor src2 = R, OperandDescriptor srcDest = R) noexcept : 
            _opcode(opcode), 
            _name(name), 
            _class(ic),
            _format(format),
            _operandCount(operandCount),
            _operands{src1, src2, srcDest},
            _kind(decode(getPrimaryOpcode()))
            {
            }
        constexpr auto getOpcode() const noexcept { return _opcode; }
        constexpr auto getPrimaryOpcode() const noexcept -> uint8_t { return static_cast<uint8_t>(_opcode >> 24); }
        constexpr const std::string& getName() const noexcept { return _name; }
        constexpr auto getClass() const noexcept { return _class; }
        constexpr auto getInstructionKind() const noexcept { return _kind; }
        constexpr auto getInstructionFormat() const noexcept { return _format; }
        constexpr auto getOperandCount() const noexcept { return _operandCount; }
        constexpr auto getSrc1Configuration() const noexcept { return _operands[0]; }
        constexpr auto getSrc2Configuration() const noexcept { return _operands[1]; }
        constexpr auto getSrcDestConfiguration() const noexcept { return _operands[2]; }
    private:
        uint32_t _opcode;
        std::string _name;
        InstructionClass _class;
        InstructionFormat _format;
        uint8_t _operandCount;
        OperandDescriptor _operands[3];
        CoreInstructionKind _kind;
    };
    enum class DecodedOpcode {
        Invalid,
// X(opcode, name, str, class, format, argCount, src1, src2, src3)
#define X(opcode, name, str, c, format, argCount, src1, src2, src3) Opcode_ ## name , 
#include "features/opcodes.def"
#undef X
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
