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
// Created by jwscoggins on 12/23/22.
//

#include <sys/stat.h>
#include <sys/time.h>
#include <stdint.h>
#include <stdio.h>

// all of the system linkage goes in this file
// these are symbols which are handled by other implementations
#if 0
extern "C" int _sys_fstat (int file, struct stat* st);
extern "C"
int
fstat (int file, struct stat* st) {
    return _sys_fstat(file, st);
}

extern "C" int _sys_getpid ();
extern "C"
int
getpid () {
    return _sys_getpid();
}
extern "C" int _sys_kill (int pid, int signal);
extern "C"
int
kill (int pid, int signal) {
    _sys_kill(pid, signal);
}
#endif
extern "C" int _sys_gettimeofday(struct timeval*, void*);
extern "C"
int
gettimeofday(struct timeval* tv, void* tz) {
    return _sys_gettimeofday(tv, tz);
}
extern "C" int _sys_setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue);
extern "C"
int
setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) {
    return _sys_setitimer(which, newValue, oldValue);
}
