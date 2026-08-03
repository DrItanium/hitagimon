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

#include "features/disassembly.h"
#include <string>

namespace Machine {
    namespace {
        using Ordinal = uint32_t;
        using LongOrdinal = uint64_t;
        using Integer = int32_t;
        enum class InstructionKind : uint8_t {
            REG,
            COBR,
            CTRL,
            MEM,
        };
        enum class AddressingMode : uint8_t {
            Offset = 0b0000,
            AbasePlusOffset = 0b1000,
            Abase = 0b0100,
            IPPlusDisplacement = 0b0101, 
            Invalid = 0b0110, // manual states it is "reserved"
            AbasePlusScaledIndex = 0b0111,
            Displacement = 0b1100,
            AbasePlusDisplacement = 0b1101,
            ScaledIndexPlusDisplacement = 0b1110,
            AbasePlusScaledIndexPlusDisplacement = 0b1111,
        };
        union DecodedInstruction {
            DecodedInstruction(Ordinal lo, Ordinal displacement = 0) : primary(lo), optionalDisplacement(displacement) { }
            struct {
                Ordinal primary;
                Integer optionalDisplacement;
            };
            struct {
                Ordinal lo24 : 24;
                uint8_t opcode;
            } generic;
            constexpr uint8_t getPrimaryOpcode() const noexcept {
                return generic.opcode;
            }
            struct {
                Ordinal src1 : 5;
                Ordinal unused0 : 2;
                Ordinal opcode2 : 4;
                Ordinal m1 : 1;
                Ordinal m2 : 1;
                Ordinal m3 : 1;
                Ordinal src2 : 5;
                Ordinal srcDest : 5;
                uint8_t opcode; 
                Integer getDisplacement() const noexcept { return 0; }
            } reg;
            struct {
                Integer b0 : 1;
                Integer b1 : 1;
                Integer displacement : 11;
                Ordinal m1 : 1;
                Ordinal src2 : 5;
                Ordinal src1 : 5;
                uint8_t opcode;
                Integer getDisplacement() const noexcept { return displacement << 2; }
            } cobr;
            struct {
                union {
                    Integer value : 24;
                    struct {
                        Integer b0 : 1;
                        Integer b1 : 1;
                    } flags;
                } displacement;
                uint8_t opcode;
                Integer getDisplacement() const noexcept { return displacement.value & (~0b11); }
            } ctrl;
            struct {
                Ordinal offset : 12;
                Ordinal differentiation : 1;
                Ordinal mode : 1;
                Ordinal abase : 5;
                Ordinal srcDest : 5;
                uint8_t opcode;
            } mema;
            struct {
                Ordinal index : 5;
                Ordinal unused : 2;
                Ordinal scale : 3;
                Ordinal mode : 4;
                Ordinal abase : 5;
                Ordinal srcDest : 5;
                uint8_t opcode;
                Integer optionalDisplacement;
            } memb;
            constexpr InstructionKind getInstructionKind() const noexcept {
                switch (getPrimaryOpcode()) {
                    case 0x00 ... 0x1F:
                        return InstructionKind::CTRL;
                    case 0x20 ... 0x3F:
                        return InstructionKind::COBR;
                    case 0x40 ... 0x7F:
                        return InstructionKind::REG;
                    case 0x80 ... 0xFF:
                        return InstructionKind::MEM;
                }
            }
            constexpr bool isMEMB() const noexcept {
                return getInstructionKind() == InstructionKind::MEM && mema.differentiation == 1;
            }
            constexpr bool isMEMA() const noexcept {
                return getInstructionKind() == InstructionKind::MEM && mema.differentiation == 0;
            }
            constexpr bool isREG() const noexcept {
                return getInstructionKind() == InstructionKind::REG;
            }
            constexpr bool usesOptionalDisplacement() const noexcept {
                switch (getAddressingMode()) {
                    case AddressingMode::IPPlusDisplacement:
                    case AddressingMode::Displacement:
                    case AddressingMode::AbasePlusDisplacement:
                    case AddressingMode::ScaledIndexPlusDisplacement:
                    case AddressingMode::AbasePlusScaledIndexPlusDisplacement:
                        return true;
                    default:
                        return false;
                }
            }
            constexpr AddressingMode getAddressingMode() const noexcept {
                if (isMEMB()) {
                    return static_cast<AddressingMode>(memb.mode);
                } else if (isMEMA()) {
                    return mema.mode ? AddressingMode::AbasePlusOffset : AddressingMode::Offset;
                } else {
                    return AddressingMode::Invalid;
                }
            }
            constexpr uint16_t getOpcodeValue() const noexcept {
                if (uint16_t primaryOpcode = getPrimaryOpcode(); isREG()) {
                    uint16_t secondaryOpcode = reg.opcode2 & 0xF;
                    primaryOpcode <<= 4;
                    return primaryOpcode | secondaryOpcode;
                } else {
                    return primaryOpcode;
                }
            }
            std::string getOpcodeMnemonic() const noexcept;

        };
        
    }
    bool needSecondWord(uint32_t lo) noexcept {
        return DecodedInstruction{lo}.usesOptionalDisplacement();
    }


    namespace {
        std::string
        DecodedInstruction::getOpcodeMnemonic() const noexcept {
            switch (getOpcodeValue()) {
#define X(opcode, str) case opcode : return #str
                X(0x08, b);
                X(0x09, call);
                X(0x0a, ret);
                X(0x0b, bal);
                X(0x10, bno);
                X(0x11, bg);
                X(0x12, be);
                X(0x13, bge);
                X(0x14, bl);
                X(0x15, bne);
                X(0x16, ble);
                X(0x17, bo);
                X(0x18, faultno);
                X(0x19, faultg);
                X(0x1a, faulte);
                X(0x1b, faultge);
                X(0x1c, faultl);
                X(0x1d, faultne);
                X(0x1e, faultle);
                X(0x1f, faulto);
                X(0x20, testno);
                X(0x21, testg);
                X(0x22, teste);
                X(0x23, testge);
                X(0x24, testl);
                X(0x25, testne);
                X(0x26, testle);
                X(0x27, testo);
                X(0x30, bbc);
                X(0x31, cmpobg);
                X(0x32, cmpobe);
                X(0x33, cmpobge);
                X(0x34, cmpobl);
                X(0x35, cmpobne);
                X(0x36, cmpoble);
                X(0x37, bbs);
                X(0x38, cmpibno);
                X(0x39, cmpibg);
                X(0x3a, cmpibe);
                X(0x3b, cmpibge);
                X(0x3c, cmpibl);
                X(0x3d, cmpibne);
                X(0x3e, cmpible);
                X(0x3f, cmpibo);
                X(0x580, notbit);
                X(0x581, and);
                X(0x582, andnot);
                X(0x583, setbit);
                X(0x584, notand);
                X(0x80, ldob);
                X(0x81, ldvob);
                X(0x82, stob);
                X(0x83, stvob);
                X(0x84, bx);
                X(0x85, balx);
                X(0x86, callx);
                X(0x88, ldos);
                X(0x89, ldvos);
                X(0x8a, stos);
                X(0x8b, stvos);
                X(0x8c, lda);
#undef X
                default:
                    return "???";
            }
        }
    }
}
