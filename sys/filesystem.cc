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

#include <unistd.h>
#include <errno.h>
#include <sys/stat.h>

extern "C"
int hitagi_access(const char* pathName, int mode) {
    //printf("access(\"%s\", %d)\n", pathName, mode);
    if (mode == R_OK) {
        errno = EACCES;
        return -1;
    } else {
        /// @todo check user's permissions for a file, this will be found on the SD Card. so this path needs to be passed to the 1284p
        errno = EACCES;
        return -1;
    }
}

extern "C"
int
hitagi_close(int fd) {
    if (fd > 2) {
#if 0
        if (cortex::getBasicChipsetInterface().closeFile(fd - 3)) {
            return 0;
        } else {
#endif
        errno = EBADF;
        return -1;
#if 0
        }
#endif
    } else {
        errno = EBADF;
        return -1;
    }
}
extern "C"
int
hitagi_fstat (int file, struct stat* st) {
    st->st_mode = S_IFCHR;
    return 0;
}

extern "C"
int
hitagi_isatty(int file) {
    return file < 3;
}
// Linkage for the system calls that are used by the C library
extern "C" int _sys_access(const char*, int);
extern "C" int access(const char* pathName, int mode) { return _sys_access(pathName, mode); }
