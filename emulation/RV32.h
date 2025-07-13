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

struct ControlRegisters {
    uint32_t csrs[4096];
    // up to 4096 registers
};

struct ExecutionEnvironment {
    ControlRegisters csrs;
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
};

extern "C" ExecutionEnvironment* getExecutionEnvironment();

}
#endif //HITAGIMON_RV32_H
