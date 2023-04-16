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
//#define SetArithmeticControls(ac) asm volatile ("modac %0, %0, %0" : "=&r" (ac.raw))
#define __builtin_i960_synld(dest, src) asm volatile ("synld %1, %0" : "=&r" (dest) : "r" (src) : "cc")
#define __builtin_i960_synmov(dest, src) asm volatile ("synmov %0, %1" : "=&r" (dest) : "r" (src) : "cc", "memory")
#define __builtin_i960_synmovl(dest, src) asm volatile ("synmovl %0, %1" : "=&r" (dest) : "r" (src) : "cc", "memory")
#define __builtin_i960_synmovq(dest, src) asm volatile ("synmovq %0, %1" : "=&r" (dest) : "r" (src) : "cc", "memory")
#define __builtin_i960_syncf asm volatile ("syncf" : : "memory")
#define __builtin_i960_mark asm volatile ("mark" : : "memory")
#define __builtin_i960_fmark asm volatile ("fmark" : : "memory")
#define __builtin_i960_flushreg asm volatile ("flushreg" : : "memory")

#endif //HITAGIMON_BUILTINS_H
