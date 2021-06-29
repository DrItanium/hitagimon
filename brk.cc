/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
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
// Created by jwscoggins on 5/2/21.
//
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <stdlib.h>
#include "IORoutines.h"
#include "LowLevelInterface.h"
static char* heapEnd = 0;

extern "C"
void*
sbrk(intptr_t increment) {
    if (heapEnd == 0) {
        heapEnd = reinterpret_cast<char*>(&end);
    }
    char* prevHeapEnd = heapEnd;
    heapEnd += increment;
    return prevHeapEnd;
}

extern "C"
int
fstat (int file, struct stat* st) {
    st->st_mode = S_IFCHR;
    return 0;
}

extern "C"
int
isatty (int file) {
    return file < 3;
}

extern "C"
off_t
lseek(int fd, off_t offset, int whence) {
    return sys_lseek(fd, offset, whence);
}

extern "C"
int
setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) {
    return sys_setitimer(which, newValue, oldValue);
}

extern "C"
int access(const char* pathName, int mode) {
    return sys_access(pathName, mode);
}

extern "C"
int getpid () {
    return -1;
}

extern "C"
int
write (int fd, const void* buf, size_t sz) {
    int numWritten = 0;
    int r = sys_write(fd, buf, sz, numWritten);
    if (r != 0) {
        errno = r;
        return -1;
    }
    return numWritten;
}
extern "C"
int
read (int fd, void* buf, size_t sz) {
    int nread = 0;
    int r = sys_read (fd, buf, sz, nread);
    if (r != 0)
    {
        errno = r;
        return -1;
    }
    return nread;
}

extern "C"
int
gettimeofday(struct timeval* tv, void* tz) {
    return sys_gettimeofday(tv, tz);
}

extern "C"
int
close(int fd) {
    return sys_close(fd);
}

extern "C"
void
_exit(int status) {
    sys_exit(status);
    // make sure that this function does not return
    while (true);
}

extern "C"
int
kill (int pid, int signal) {
    exit (signal);
}