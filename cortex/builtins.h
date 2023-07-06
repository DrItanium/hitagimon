/*
hitagimon
Copyright (c) 2020-2023, Joshua Scoggins
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
//
// Created by jwscoggins on 4/16/23.
//

#ifndef HITAGIMON_BUILTINS_H
#define HITAGIMON_BUILTINS_H
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
#if defined(__i960MC__) || defined(__i960SB__) || defined(__i960KB__) || defined(__i960KA__) || defined(__i960XA__)
#define __HAS_PROTECTED_ARCHITECTURE_EXTENSIONS__ 1
#define __HAS_NUMERICS_ARCHITECTURE_EXTENSIONS__ 1
#endif
inline uint32_t __builtin_i960_synld(void* src) {
    uint32_t dest = 0;
    asm volatile ("synld %1, %0" : "=&r" (dest) : "r" (src) : "cc", "memory");
    return dest;
}
inline void __builtin_i960_synmov(void* dest, void* src) {
    asm volatile ("synmov %0, %1" : : "r" (dest) , "r" (src) : "cc", "memory");
}
inline void __builtin_i960_synmovl(void* dest, void* src) {
    asm volatile ("synmovl %0, %1" : : "r" (dest) , "r" (src) : "cc", "memory");
}
inline void __builtin_i960_synmovq(void* dest, void* src) {
    asm volatile ("synmovq %0, %1" : : "r" (dest) , "r" (src) : "cc", "memory");
}
inline uint32_t __builtin_i960_get_interrupt_control_reg(void) {
    return __builtin_i960_synld((void*)0xFF000004);
}
inline void __builtin_i960_set_interrupt_control_reg(uint32_t value) {
    // stash the value on the stack
    uint32_t copy = value;
    __builtin_i960_synmov((void*)0xFF000004, (void*)&copy);
}
inline void __builtin_i960_flushreg(void) { asm volatile ("flushreg" : : : "memory"); }
inline void __builtin_i960_mark(void) { asm volatile ("mark" ::: "memory"); }
inline void __builtin_i960_fmark(void) { asm volatile ("fmark" ::: "memory"); }
inline void __builtin_i960_syncf(void) { asm volatile ("syncf" ::: "memory"); }
inline uint32_t __builtin_i960_modac(uint32_t mask, uint32_t src) {
    uint32_t result = 0;
    asm volatile ("modac %2, %1, %0" : "=r" (result) :  "r" (src) , "r" (mask) : "cc", "memory");
    return result;
}
inline uint32_t __builtin_i960_setac(uint32_t value) {
    asm volatile ("modac %0, %0, %0" : "=&r" (value) : : "cc", "memory");
    return value;
}
inline uint32_t __builtin_i960_getac(void) {
    uint32_t result = 0;
    asm volatile ("modac 0, 0, %0" : "=&r" (result) :  : "memory");
    return result;
}

inline uint32_t __builtin_i960_modtc(uint32_t mask, uint32_t src) {
    uint32_t result = 0;
    asm volatile ("modtc %2, %1, %0" : "=r" (result) :  "r" (src) , "r" (mask) : "cc", "memory");
    return result;
}
inline uint32_t __builtin_i960_settc(uint32_t value) {
    asm volatile ("modtc %0, %0, %0" : "=&r" (value) : : "cc", "memory");
    return value;
}
inline uint32_t __builtin_i960_gettc(void) {
    uint32_t result = 0;
    asm volatile ("modtc 0, 0, %0" : "=&r" (result) :  : "memory");
    return result;
}

inline uint32_t __builtin_i960_modpc(uint32_t mask, uint32_t src) {
    uint32_t result = 0;
    asm volatile ("modpc %2, %1, %0" : "=r" (result) :  "r" (src) , "r" (mask) : "cc", "memory");
    return result;
}
inline uint32_t __builtin_i960_setpc(uint32_t value) {
    asm volatile ("modpc %0, %0, %0" : "=&r" (value) : : "cc", "memory");
    return value;
}
inline uint32_t __builtin_i960_getpc(void) {
    uint32_t result = 0;
    asm volatile ("modpc 0, 0, %0" : "=&r" (result) :  : "memory");
    return result;
}
#define __BUILTIN_I960_SYNLD 1
#define __BUILTIN_I960_SYNMOV 1
#define __BUILTIN_I960_SYNMOVL 1
#define __BUILTIN_I960_SYNMOVQ 1
#define __BUILTIN_I960_GET_INTERRUPT_CONTROL_REG 1
#define __BUILTIN_I960_SET_INTERRUPT_CONTROL_REG 1
#define __BUILTIN_I960_FLUSHREG 1
#define __BUILTIN_I960_MARK 1
#define __BUILTIN_I960_FMARK 1
#define __BUILTIN_I960_SYNCF 1
#define __BUILTIN_I960_GETAC 1
#define __BUILTIN_I960_SETAC 1
#define __BUILTIN_I960_MODAC 1
#define __BUILTIN_I960_MODTC 1
#define __BUILTIN_I960_SETTC 1
#define __BUILTIN_I960_GETTC 1
#define __BUILTIN_I960_MODPC 1
#define __BUILTIN_I960_SETPC 1
#define __BUILTIN_I960_GETPC 1
#ifdef __HAS_PROTECTED_ARCHITECTURE_EXTENSIONS__
#define __BUILTIN_I960_MOVSTR 1
inline void __builtin_i960_movstr(char* srcBytes, char* destBytes, uint32_t length) {
    asm volatile ("movstr %0, %1, %2" : : "r" (destBytes) , "r" (srcBytes), "r" (length) : "cc", "memory");
}
#define __BUILTIN_I960_MOVQSTR 1
inline void __builtin_i960_movqstr(char* srcBytes, char* destBytes, uint32_t length) {
    asm volatile ("movqstr %0, %1, %2" : : "r" (destBytes) , "r" (srcBytes), "r" (length) : "cc", "memory");
}

#define __BUILTIN_I960_INSPACC 1
inline uint32_t __builtin_i960_inspacc(void* src) {
    uint32_t dest = 0;
    asm volatile ("inspacc %1, %0" : "=&r" (dest) : "r" (src) : "cc", "memory");
    return dest;
}

#define __BUILTIN_I960_CONDREC 1
inline uint32_t __builtin_i960_condrec(uint32_t srcSS) {
    uint32_t destSS = 0;
    asm volatile ("condrec %1, %0" : "=&r" (destSS) : "r" (srcSS) : "cc", "memory");
    return destSS;
}

#define __BUILTIN_I960_LDPHY 1
inline uint32_t __builtin_i960_ldphy(char* src) {
    uint32_t dest = 0;
    asm volatile ("ldphy %1, %0" : "=&r" (dest) : "r" (src) : "cc", "memory");
    return dest;
}

#define __BUILTIN_I960_SAVEPRCS 1
inline void __builtin_i960_saveprcs(void) {
    asm volatile ("saveprcs" ::: "memory" );
}
#define __BUILTIN_I960_WAIT 1
inline void __builtin_i960_wait(void) {
    asm volatile ("wait" ::: "memory" );
}
#endif // end defined(__HAS_PROTECTED_ARCHITECTURE_EXTENSIONS__)
#ifdef __HAS_NUMERICS_ARCHITECTURE_EXTENSIONS__
#endif // end defined(__HAS_NUMERICS_ARCHITECTURE_EXTENSIONS__)


#ifdef __cplusplus
}
#endif
#endif //HITAGIMON_BUILTINS_H
