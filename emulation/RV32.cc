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

#include "emulation/RV32.h"


uint32_t testApplication(uint32_t a, uint32_t b) { return __builtin_rv32_maxu(a, b); }
int32_t testApplication2(int32_t a, int32_t b) { return __builtin_rv32_max(a, b); }

uint32_t testApplication3(uint32_t a, uint32_t b) {
    return __builtin_rv32_andn(a, b);
}

uint32_t testApplication4(int16_t a) {
    return __builtin_rv32_sext_h(a);
}

uint32_t testApplication5(int8_t a) {
    return __builtin_rv32_sext_b(a);
}

uint32_t testApplication6(uint32_t a, uint32_t b) { return __builtin_rv32_sh1add(a, b); }
uint32_t testApplication7(uint32_t a, uint32_t b) { return __builtin_rv32_sh2add(a, b); }
uint32_t testApplication8(uint32_t a, uint32_t b) { return __builtin_rv32_sh3add(a, b); }

uint32_t testApplication9(uint32_t a, uint32_t b) { return __builtin_rv32_minu(a, b); }
int32_t testApplication10(int32_t a, int32_t b) { return __builtin_rv32_min(a, b); }

uint64_t testApplication11(uint64_t a, uint64_t b) { return a * b; }
int64_t testApplication11a(int64_t a, int64_t b) { return a * b; }
uint64_t testApplication12(uint64_t a, uint64_t b) { return a / b; }
int64_t testApplication12a(int64_t a, int64_t b) { return a / b; }

uint32_t countLeadingZeros(uint32_t value) {
    uint32_t result = 0;
    // count leading zeros
    asm volatile (
            "scanbit %1, %0\n\t"
            "subo %0, 31, %0" : "=r" (result) : "r"(value) );

    return result;
}

uint32_t countTrailingZeros(uint32_t value) {
    if (value == 0) {
        return 32;
    } else {
        uint32_t count = 0;
        for (int i = 0; i < 32; ++i, value >>= 1) {
            if (value & 1) {
                break;
            } 
            ++count;
        }
        return count;
    }
}
uint32_t populationCount(uint32_t value) {
    uint32_t count = 0;
    for (int i = 0; i < 32; ++i, value >>= 1) {
        if ((value & 1)) {
            ++count;
        }
    }
    return count;
}



namespace rv32 {
uint32_t
ExecutionEnvironment::getCSR(uint16_t a) const {
    return csrs[a & 0xFFF];
}

}

uint32_t 
rotateLeft(uint32_t rs1, uint32_t rs2) {
    uint32_t shamt = rs2 & 0x1f;
    return (rs1 << shamt) | (rs1 >> (32 - shamt));
}

uint32_t 
rotateRight(uint32_t rs1, uint32_t rs2) {
    uint32_t shamt = rs2 & 0x1f;
    return (rs1  >> shamt) | (rs1 << (32 - shamt));
}
template<typename T>
T load(T* ptr, uint8_t offset) {
    return ptr[offset];
}
struct test0 {
    uint32_t values[4];
};

template float load(float*, uint8_t);
template double load(double *, uint8_t);
template long double load(long double *, uint8_t);
template test0 load(test0*, uint8_t);


template<typename T>
T fusedMultiplyAdd(T a, T b, T c) {
    return (a * b) + c;
}

template float fusedMultiplyAdd(float, float, float);
template double fusedMultiplyAdd(double, double, double);
template long double fusedMultiplyAdd(long double, long double, long double);

