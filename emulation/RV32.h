/*
hitagimon
Copyright (c) 2020-2025, Joshua Scoggins
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

#ifndef HITAGIMON_RV32_H
#define HITAGIMON_RV32_H
#include <stdint.h>
#include <cortex/Types.h>
#include <cortex/SysExamine.h>
/*
 * This contains concepts necessary for executing pre-translated rv32 assembly
 * instructions under the i960 itself. This of this more like documentation
 */
namespace rv32 {


struct ExecutionEnvironment {
    uint32_t csrs[4096];
    // Since riscv does not use register windows or an upward growing stack, we
    // need to emulate it. Thus we have actually 6 stacks to worry about at any
    // given point. Three of them are meant for the i960 hardware. The other
    // three are emulated to allow for a downward growing stack.
    //
    // Each time we change context the system state must be inspected to double
    // check that we are doing the right thing.
    uint32_t userStackAddress;
    uint32_t supervisorStackAddress;
    uint32_t kernelStackAddress;

    // registers that aren't directly mapped to i960 registers
    uint32_t gp;
    uint32_t tp;
    uint32_t s8, s9, s10, s11;

    uint32_t getCSR(uint16_t offset) const;
};

extern "C" ExecutionEnvironment* getExecutionEnvironment();


}
#ifdef __cplusplus
extern "C" {
#endif
// allow the compiler to generate instruction sequences
inline uint32_t __builtin_rv32_sltu(uint32_t rs1, uint32_t rs2) { return rs1 < rs2; }
inline uint32_t __builtin_rv32_maxu(uint32_t rs1, uint32_t rs2) { return rs1 < rs2 ? rs2 : rs1; }
inline int32_t __builtin_rv32_max(int32_t rs1, int32_t rs2) { return rs1 < rs2 ? rs2 : rs1; }
inline uint32_t __builtin_rv32_minu(uint32_t rs1, uint32_t rs2) { return rs1 > rs2 ? rs2 : rs1; }
inline int32_t __builtin_rv32_min(int32_t rs1, int32_t rs2) { return rs1 > rs2 ? rs2 : rs1; }
inline uint32_t __builtin_rv32_andn(uint32_t rs1, uint32_t rs2) { return (~rs2) & rs1; }
inline int32_t __builtin_rv32_sext_h(int16_t rs1) { return static_cast<int32_t>(rs1); }
inline int32_t __builtin_rv32_sext_b(int8_t rs1) { return static_cast<int32_t>(rs1); }
inline uint32_t __builtin_rv32_sh1add(uint32_t rs1, uint32_t rs2) { return rs2 + (rs1 << 1); }
inline uint32_t __builtin_rv32_sh2add(uint32_t rs1, uint32_t rs2) { return rs2 + (rs1 << 2); }
inline uint32_t __builtin_rv32_sh3add(uint32_t rs1, uint32_t rs2) { return rs2 + (rs1 << 3); }

#ifdef __cplusplus
}
#endif
#endif //HITAGIMON_RV32_H
