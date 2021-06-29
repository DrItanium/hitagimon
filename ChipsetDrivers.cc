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
// Created by jwscoggins on 6/28/21.
//

#include "ChipsetDrivers.h"
#include "IODevice.h"
#include <errno.h>
int
performSysWrite(int fd, const void *buf, size_t sz, int &nwrite) {
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
performSysRead(int fd, void *buf, size_t sz, int &nread) {
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
int
performSysLseek(int fd, off_t offset, int whence) {
    //printf("lseek(%d, %ld, %d)\n", fd, offset, whence);
    /// @todo implement this using an SD Card interface
    if (fd >= 3) {
        if (fd < (getSDCardInterface().getMaximumNumberOfOpenFiles() + 3)) {
            return getSDCardInterface().seek(fd - 3, offset, whence);
        } else {
            errno = EBADF;
            return -1;
        }
    } else {
        // builtin files
        switch (fd) {
            case STDIN_FILENO:
                return 0;
            default:
                errno = EBADF;
                return -1;
        }
    }
}
void
performSysExit(int signal) {
    while (true) {
        // just hang here
    }
}
int
performSysClose(int fd) {
    //printf("close(%d);\n", fd);
    if (fd >= 3) {
        return getSDCardInterface().closeFile(fd - 3);
    } else {
        errno = EBADF;
        return -1;
    }
}
int
performSysOpen(const char *file, int flags, int mode) {
    int result = getSDCardInterface().openFile(file, flags);
    if (result != -1) {
        result =+ 3; // skip past the stdin/stderr/stdout ids
    }
    return result;
}

void
performLedToggle() {
    getBasicChipsetInterface().toggleLED();
}

int
performSysAccess(const char *pathName, int mode) {
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
