/*
hitagimon
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
#include <sys/time.h>
#include <stdlib.h>
#include "IORoutines.h"
#include "IODevice.h"

namespace
{
    int
    sys_write(int fd, const void *buf, size_t sz, int &nwrite) {
        nwrite = 0;
        if (fd >= 3) {
            if (fd < (getSDCardInterface().getMaximumNumberOfOpenFiles() + 3)) {
                nwrite = getSDCardInterface().writeFile(fd - 3, buf, sz);
                return 0;
            } else {
                return EBADF;
            }
        } else {
            // builtin files
            switch (fd) {
                case STDOUT_FILENO:
                case STDERR_FILENO:
                    nwrite = getBasicChipsetInterface().write(reinterpret_cast<char *>(const_cast<void *>(buf)), sz);
                    break;
                default:
                    return EBADF;
            }
            return 0;
        }
    }
    int
    sys_read(int fd, void *buf, size_t sz, int &nread) {
        //char* theBuf = reinterpret_cast<char*>(buf);
        nread = 0;
        if (fd >= 3) {
            if (fd < (getSDCardInterface().getMaximumNumberOfOpenFiles() + 3)) {
                nread = getSDCardInterface().readFile(fd - 3, buf, sz);
                return 0;
            } else {
                return EBADF;
            }
        } else {
            // builtin files
            switch (fd) {
                case STDIN_FILENO:
                    nread = getBasicChipsetInterface().read(reinterpret_cast<char *>(buf), sz);
                    break;
                default:
                    return EBADF;
            }
        }
    }
}
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
setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) {
    printf("setitimer(%d, %x, %x)\n", which, newValue, oldValue);
    /// @todo use arduino timers to satisfy this, we use the interrupts to trigger timers
    return 0;
}

extern "C"
int access(const char* pathName, int mode) {
    //printf("access(\"%s\", %d)\n", pathName, mode);
    // the sd card interface does not have the concept of permission bits but we can easily do
    if (mode == R_OK) {
        if (getSDCardInterface().fileExists(pathName)) {
            return 0;
        } else {
            errno = EACCES;
            return -1;
        }
    } else {
        /// @todo check user's permissions for a file, this will be found on the SD Card. so this path needs to be passed to the 1284p
        errno = EACCES;
        return -1;
    }
}

extern "C"
int getpid () {
    return -1;
}

extern "C"
int
read (int fd, void* buf, size_t sz) {
    //printf("read(%d, 0x%x, %ld)\n", fd, buf, sz);
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
    printf("gettimeofday(%x, %x);\n", tv, tz);
    return 0;
}

extern "C"
int
close(int fd) {
    //printf("close(%d);\n", fd);
    if (fd >= 3) {
        return getSDCardInterface().closeFile(fd - 3);
    } else {
        errno = EBADF;
        return -1;
    }
}

extern "C"
void
_exit(int status) {
    printf("exit(%d)\n", status);
    while (true) {
       // just hang here
    }
}

extern "C"
int
kill (int pid, int signal) {
    printf("KILLING PROCESS!\n");
    exit (signal);
}
namespace {
}
extern "C"
int
open (char* file, int flags) {
    //printf("open(\"%s\", %d)\n", file, flags);
    int result = getSDCardInterface().openFile(file, flags);
    if (result != -1) {
        result =+ 3; // skip past the stdin/stderr/stdout ids
    }
    return result;
}