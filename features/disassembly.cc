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
        union DecodedInstruction {
            DecodedInstruction(Ordinal lo, Ordinal displacement = 0) : primary(lo), optionalDisplacement(displacement) { }
            struct {
                Ordinal primary;
                Integer optionalDisplacement;
            };
            struct {
                Ordinal src1 : 5;
                Ordinal unused0 : 2;
                Ordinal opcode2 : 4;
                Ordinal m1 : 1;
                Ordinal m2 : 1;
                Ordinal m3 : 1;
                Ordinal src2 : 5;
                Ordinal srcDest : 5;
                Ordinal opcode : 8;
                Integer getDisplacement() const noexcept { return 0; }
            } reg;
            struct {
                union {
                    Integer value : 12;
                    struct {
                        // lowest two bits are reserved
                        Integer b0 : 1;
                        Integer b1 : 1;
                    } flags;
                } rawDisplacement;
                Ordinal m1 : 1;
                Ordinal src2 : 5;
                Ordinal src1 : 5;
                Ordinal opcode : 8;
                Integer getDisplacement() const noexcept { return rawDisplacement.value & (~0b11); }
            } cobr;
        };

        bool isMEMB(uint32_t value) noexcept;
    }
    bool needSecondWord(uint32_t lo) noexcept {
        return isMEMB(lo);
    }
}
