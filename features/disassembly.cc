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
#include <map>
#include <tuple>
#include <sstream>

namespace Machine {
#if 0
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
        void injectFloatingPointValue(uint8_t index, std::ostream& out, bool isDestination = false) noexcept {
            switch (index) {
                case 0: 
                    out << "fp0"; 
                    break;
                case 1: 
                    out << "fp1"; 
                    break;
                case 2: 
                    out << "fp2"; 
                    break;
                case 3: 
                    out << "fp3"; 
                    break;
                case 0b10000: 
                    out << "+0.0";
                    if (isDestination) {
                        out << "!!!";
                    }
                    break;
                case 0b10110:
                    out << "+1.0";
                    if (isDestination) {
                        out << "!!!";
                    }
                    break;
                default:
                    out << "reserved";
                    break;
            }
        }
        void injectRegister(uint8_t index, std::ostream& out) noexcept {
            switch (index) {
#define X(index, name) case index : out << #name ; break
#define R(index) X(index , r ## index)
#define G(index) X(index + 16, g ## index)
                X(0, pfp); 
                X(1, sp); 
                X(2, rip); 
                R(3); 
                R(4); 
                R(5); 
                R(6); 
                R(7);
                R(8); 
                R(9); 
                R(10); 
                R(11); 
                R(12); 
                R(13); 
                R(14);
                R(15);
                G(0);
                G(1);
                G(2);
                G(3);
                G(4);
                G(5);
                G(6);
                G(7);
                G(8);
                G(9);
                G(10);
                G(11);
                G(12);
                G(13);
                G(14);
                X(31, fp);
#undef G
#undef R
#undef X
                default:
                    break;
            }
        }
        union DecodedInstruction {
            DecodedInstruction(Ordinal lo, Integer displacement = 0) : primary(lo), optionalDisplacement(displacement) { }
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

                constexpr uint16_t getOpcodeValue() const noexcept {
                    uint16_t secondaryOpcode = opcode2 & 0xF;
                    uint16_t primaryOpcode = (static_cast<uint16_t>(opcode) << 4) & 0x0FF0;
                    return primaryOpcode | secondaryOpcode;
                }
                constexpr Opcode getOpcode() const noexcept {
                    return static_cast<Opcode>(getOpcodeValue());
                }
                constexpr bool isFloatingPointOperation() const noexcept {
                    return getArchitectureLevel(getOpcode()) == ArchitectureLevel::Numerics;
                }
                bool treatSrc1AsLiteral() const noexcept { return m1 != 0; }
                bool treatSrc2AsLiteral() const noexcept { return m2 != 0; }
                private:
                void injectGenericRegister(std::ostream& stream, Ordinal regValue, bool treatAsLiteral, bool isDestination = false) const noexcept {
                    if (treatAsLiteral) {
                        if (isFloatingPointOperation()) {
                            injectFloatingPointValue(regValue, stream, isDestination);
                        } else {
                            stream << std::dec << regValue;
                            if (isDestination) {
                                stream << "!!!";
                            }
                        }
                    } else {
                        injectRegister(regValue, stream);
                    }
                }
                void injectSrc1(std::ostream& stream) const noexcept {
                    injectGenericRegister(stream, src1, treatSrc1AsLiteral());
                }
                void injectSrc2(std::ostream& stream) const noexcept {
                    injectGenericRegister(stream, src2, treatSrc2AsLiteral());
                }
                void injectSrcDest(std::ostream& stream) const noexcept {

                }
                public:
                void disassemble(std::ostream& stream) const noexcept {
                    injectSrc1(stream);
                    stream << ", ";
                    injectSrc2(stream);
                    stream << ", ";
                }
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
                bool treatSrc1AsLiteral() const noexcept { return m1 != 0; }
                void injectSrc1(std::ostream& stream) const noexcept {
                    if (treatSrc1AsLiteral()) {
                        stream << std::dec << src1;
                    } else {
                        injectRegister(src1, stream);
                    }
                }
                void injectSrc2(std::ostream& stream) const noexcept {
                    injectRegister(src2, stream);
                }
                void disassemble(std::ostream& stream) const noexcept {
                    injectSrc1(stream);
                    stream << ", ";
                    injectSrc2(stream);
                    stream << ", " << std::dec << getDisplacement();
                }
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
                void disassemble(std::ostream& stream) const noexcept {
                    stream << std::dec << getDisplacement();
                }
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
                if (isREG()) {
                    return reg.getOpcodeValue();
                } else {
                    return getPrimaryOpcode();
                }
            }
            constexpr Opcode getOpcode() const noexcept {
                if (auto result = static_cast<Opcode>(getOpcodeValue()); valid(result)) {
                    return result;
                } else {
                    return Opcode::Unknown;
                }
            }
            std::string getOpcodeMnemonic() const noexcept { return toString(getOpcode()); }
            constexpr bool isFloatingPointInstruction() const noexcept {
                return isREG() && (getArchitectureLevel(getOpcode()) == ArchitectureLevel::Numerics);
            }
            void disassemble(std::ostream& out) const noexcept {
                out << toString(getOpcode()) << " ";
                switch (getInstructionKind()) {
                    case InstructionKind::CTRL: 
                        ctrl.disassemble(out);
                        break;
                    case InstructionKind::COBR:
                        cobr.disassemble(out);
                        break;
                    case InstructionKind::REG:
                        reg.disassemble(out);
                        break;
                    default:
                        out << "TODO: FINISH";
                        break;
                }
            }
        };
        
    }
    bool needSecondWord(uint32_t lo) noexcept {
        return DecodedInstruction{lo}.usesOptionalDisplacement();
    }
    using InstructionInfo = std::tuple<Opcode, std::string, ArchitectureLevel>;
    static const inline std::map<Opcode, InstructionInfo> opcodeData {
#define X(opcode, str, arch, group) { Opcode:: Opcode_ ## str , { Opcode:: Opcode_ ## str , #str, ArchitectureLevel:: arch } },
#include "features/opcodes.def"
#undef X
    };

    std::string
    toString(Opcode opcode) noexcept {
        if (auto data = opcodeData.find(opcode); data != opcodeData.end()) {
            return std::get<std::string>(data->second);
        } else {
            return "???";
        }
    }
    ArchitectureLevel
    getArchitectureLevel(Opcode opcode) noexcept {
        if (auto data = opcodeData.find(opcode); data != opcodeData.end()) {
            return std::get<ArchitectureLevel>(data->second);
        } else {
            return ArchitectureLevel::Invalid;
        }
    }
    void
    disassembleREG(DecodedInstruction& inst, std::ostream& os) noexcept {
    }
    std::string
    disassemble(uint32_t lo, int32_t displacement) noexcept {
        DecodedInstruction inst{lo, displacement};
        std::stringstream ss;
        inst.disassemble(ss);
        std::string result = ss.str();
        return result;
    }
#endif
}
