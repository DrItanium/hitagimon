//
// Created by jwscoggins on 4/14/23.
//

#include "ModernGCC.h"
#ifndef __BUILTIN_MUL_OVERFLOW
#include "SysExamine.h"
extern "C"
bool
__builtin_mul_overflow(size_t a, size_t b, size_t* res) {
    register uint32_t save asm("r14");
    cortex::ArithmeticControls intermediate;
    do {
        asm volatile ("modac 0, 0, %0": "=r" (save));
        intermediate.raw = save;
        intermediate.integerOverflowMask = 1;
    } while (false);
    SetArithmeticControls(intermediate);
    do {
        *res = a * b;
    } while (false);
    GetArithmeticControls(intermediate);
    bool result = intermediate.integerOverflowFlag;
    asm volatile ("modac %0, %0, %0": "=&r" (save));
    return result;
}

#endif