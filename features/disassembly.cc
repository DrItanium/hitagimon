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
    void
    begin() noexcept {
        static bool initialized = false;
        if (!initialized) {
            initialized = true;
        }
    }
}
