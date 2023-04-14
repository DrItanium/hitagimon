//
// Created by jwscoggins on 4/14/23.
//

#include "ModernGCC.h"
#ifndef __BUILTIN_MUL_OVERFLOW
#include "SysExamine.h"

inline uint32_t setArithmeticControls(uint32_t mask, uint32_t src) {
    uint32_t result = 0;
    asm volatile ("modac %2, %1, %0" : "=r" (result) :  "r" (src) , "r" (mask));
    return result;
}
inline uint32_t setArithmeticControls(uint32_t value) {
    asm volatile ("modac %0, %0, %0" : "=&r" (value));
    return value;
}
inline uint32_t getArithmeticControls() {
    uint32_t result = 0;
    asm volatile ("modac 0, 0, %0" : "=r" (result));
    return result;
}
extern "C"
bool
__builtin_mul_overflow(size_t a, size_t b, size_t* res) {
    uint32_t save = setArithmeticControls(0x500, 0x400);
    do {
        *res = a * b;
    } while (false);
    uint32_t result = getArithmeticControls();
    setArithmeticControls(save);
    return (result & 0x100);
}

#endif