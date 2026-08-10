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
#include <cstdint>

namespace Machine {
    namespace {
        //explicit Opcode(uint32_t opcode, const std::string& name, InstructionClass ic, InstructionFormat format, uint8_t operandCount, OperandDescriptor src1 = R, OperandDescriptor src2 = R, OperandDescriptor srcDest = R) noexcept : 
        static inline const std::map<EncodedOpcode, Opcode> encodedOpcodes {
#define X(opcode, encodedOpcode, name, str, c, format, argCount, src1, src2, src3) \
            { EncodedOpcode :: Opcode_ ## name , Opcode { encodedOpcode, str, InstructionClass :: c, InstructionFormat :: format, argCount, src1, src2, src3 } },
#include "features/opcodes.def"
#undef X
        };
        static inline const std::map<DecodedOpcode, Opcode> decodedOpcodes {
#define X(opcode, encodedOpcode, name, str, c, format, argCount, src1, src2, src3) \
            { DecodedOpcode :: Opcode_ ## name , Opcode { encodedOpcode, str, InstructionClass :: c, InstructionFormat :: format, argCount, src1, src2, src3 } },
#include "features/opcodes.def"
#undef X
        };
    }
    [[gnu::used]]
    std::optional<Opcode>
    translate(EncodedOpcode opcode) noexcept {
        if (auto result = encodedOpcodes.find(opcode); result != encodedOpcodes.end()) {
            return std::make_optional(result->second);
        } else {
            return std::nullopt;
        }
    }
    [[gnu::used]]
    std::optional<Opcode>
    translate(DecodedOpcode opcode) noexcept {
        if (auto result = decodedOpcodes.find(opcode); result != decodedOpcodes.end()) {
            return std::make_optional(result->second);
        } else {
            return std::nullopt;
        }
    }
    struct TemporaryInstruction {
        uint64_t full;
        uint32_t halves[2];
        struct {
            uint32_t instruction;
            int32_t displacement;
        };
        struct {
           uint32_t src1 : 5;
           uint32_t unused0 : 2;
           uint32_t secondaryOpcode : 4;
           uint32_t m1 : 1;
           uint32_t m2 : 1;
           uint32_t m3 : 1;
           uint32_t src2 : 5;
           uint32_t srcDest : 5;
           uint32_t primaryOpcode : 8;
           constexpr uint16_t getOpcodeValue() const noexcept {
                return (primaryOpcode << 4) | secondaryOpcode;
           }
        } reg;
        struct {
            int32_t displacement : 13;
            uint32_t m1 : 1;
            uint32_t src2 : 5;
            uint32_t src1 : 5;
            uint32_t opcode : 8;
            constexpr int32_t getDisplacement() const noexcept {
                return displacement & 0xFFFF'FFFC;
            }
            constexpr uint16_t getOpcodeValue() const noexcept {
                return opcode;
            }
        } cobr;
        struct {
            int32_t displacement : 24;
            uint32_t opcode : 8;
            constexpr int32_t getDisplacement() const noexcept {
                return displacement & 0xFFFF'FFFC;
            }
            constexpr uint16_t getOpcodeValue() const noexcept {
                return opcode;
            }
        } ctrl;
        struct {
            union {
                enum class Mode {
                    AbsoluteOffset = 0b0000,
                    RegisterIndirectWithOffset = 0b1000,
                    RegisterIndirect = 0b0100,
                    IPWithDisplacement = 0b0101,
                    Invalid = 0b0110,
                    RegisterIndirectWithIndex = 0b0111,
                    AbsoluteDisplacement = 0b1100,
                    RegisterIndirectWithDisplacement = 0b1101,
                    IndexWithDisplacement = 0b1110,
                    RegisterIndirectWithIndexAndDisplacement = 0b1111,
                };
                static constexpr bool valid(Mode mode) noexcept {
                    switch(mode) {
                        case Mode :: AbsoluteOffset: 
                        case Mode :: RegisterIndirectWithOffset:
                        case Mode :: RegisterIndirect:
                        case Mode :: IPWithDisplacement:
                        case Mode :: RegisterIndirectWithIndex:
                        case Mode :: AbsoluteDisplacement:
                        case Mode :: RegisterIndirectWithDisplacement:
                        case Mode :: IndexWithDisplacement:
                        case Mode :: RegisterIndirectWithIndexAndDisplacement:
                            return true;
                        default:
                            return false;
                    }
                }
                uint32_t value : 14;
                struct {
                    uint32_t : 12;
                    uint32_t modeChoice : 1;
                    uint32_t : 1;
                } discriminator;
                struct {
                    uint32_t offset : 12;
                    uint32_t : 1;
                    uint32_t mode : 1;
                } atype;
                struct {
                    uint32_t index : 5;
                    uint32_t unused : 2;
                    uint32_t scale : 3;
                    uint32_t mode : 4;
                } btype;
                constexpr bool isAType() const noexcept {
                    return discriminator.modeChoice == 0;
                }
                constexpr bool isBType() const noexcept {
                    return discriminator.modeChoice == 1;
                }
                constexpr Mode getMode() const noexcept {
                    if (isAType()) {
                        if (atype.mode) {
                            return Mode::RegisterIndirectWithOffset;
                        } else {
                            return Mode::AbsoluteOffset;
                        }
                    } else {
                        if (Mode m = static_cast<Mode>(btype.mode); valid(m)) {
                            return m;
                        } else {
                            return Mode::Invalid;
                        }
                    }
                }
            } encoding;
            uint32_t abase : 5;
            uint32_t srcDest : 5;
            uint32_t opcode : 8;
            constexpr uint16_t getOpcodeValue() const noexcept {
                return opcode;
            }
        } mem;


    };
    int 
    disassemble(uint64_t full, std::ostream& stream) noexcept {
        TemporaryInstruction tmp;
        tmp.full = full;
        return 4;
    }
}
