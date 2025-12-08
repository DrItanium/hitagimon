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
#include <cortex/IODevice.h>
#include <cortex/FilesystemInterface.h>

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
    cortex::File& targetFile = cortex::Filesystem::getFile(fd);
    if (targetFile) {
        targetFile.close();
        return 0;
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
    return cortex::Filesystem::getFile(file).isatty();
}
extern "C"
int
hitagi_unlink(const char* path) {
    if (cortex::Filesystem::unlinkFile(path)) {
        return 0;
    } else {
        errno = ENOSYS;
        return -1;
    }
}

extern "C"
int
hitagi_open (char* file, int flags, int mode) {
    // if we can't open for whatever reason a null file type will come back
    // the uid will always be -1 in that case
    return cortex::Filesystem::openFile(file, flags, mode).uid();
}

extern "C"
int
hitagi_link (const char* path1, const char* path2) {
    if (cortex::Filesystem::linkFile(path1, path2)) {
        return 0;
    } else {
        errno = ENOSYS;
        return -1;
    }
}

extern "C"
off_t
hitagi_lseek(int fd, off_t offset, int whence) {
    cortex::File& targetFile = cortex::Filesystem::getFile(fd);
    if (targetFile) {
        if (!targetFile.canSeek()) {
            errno = EBADF;
        }
    } else {
        errno = EBADF;
    }
    // just always call it since it always has an implementation
    return targetFile.seek(offset, whence);
}

extern "C"
int
hitagi_read(int fd, void *buf, size_t sz, int *nread) {
    *nread = 0;
    cortex::File& targetFile = cortex::Filesystem::getFile(fd);
    if (!targetFile) {
       return EBADF;
    } else {
        *nread = targetFile.read(reinterpret_cast<char*>(buf), sz);
        return 0;
    }
}
extern "C"
int
hitagi_write(int fd, const void *buf, size_t sz, int *nwrite) {
    *nwrite = 0;
    cortex::File& targetFile = cortex::Filesystem::getFile(fd);
    if (!targetFile) {
       return EBADF;
    } else {
        *nwrite = targetFile.write(reinterpret_cast<const char*>(buf), sz);
        return 0;
    }
}



// Linkage for the system calls that are used by the C library
extern "C" int _sys_read(int fd, void* buf, size_t sz, int* nread);
extern "C" int _sys_write(int fd, const void* buf, size_t sz, int* nwrite);
extern "C" off_t _sys_lseek(int fd, off_t offset, int whence);
extern "C" int _sys_open(const char* pathName, int flags, int mode);
extern "C" int _sys_access(const char*, int);
extern "C" int _sys_unlink(const char* path);
extern "C" int _sys_fstat(int file, struct stat* st);
extern "C" int _sys_link(const char* path1, const char* path2);
extern "C" int _sys_isatty(int file);
extern "C" int _sys_close(int fd);

extern "C" int access(const char* pathName, int mode) { return _sys_access(pathName, mode); }
extern "C" int unlink(const char* path) { return _sys_unlink(path); }
extern "C" int open(const char* pathName, int flags, int mode) { return _sys_open(pathName, flags, mode); }
extern "C" int fstat(int file, struct stat* st) { return _sys_fstat(file, st); }
extern "C" int link(const char* path1, const char* path2) { return _sys_link(path1, path2); }
extern "C" int isatty(int file) { return _sys_isatty(file); }
extern "C" off_t lseek(int fd, off_t offset, int whence) { return _sys_lseek(fd, offset, whence); }
extern "C" int read(int fd, void* buf, size_t sz) {
    int nread = 0;
    int r = _sys_read(fd, buf, sz, &nread);
    if (r != 0) {
        errno = r;
        return -1;
    }
    return nread;
}

extern "C" int write(int fd, const void* buf, size_t sz) {
    int nwrite = 0;
    int r = _sys_write(fd, buf, sz, &nwrite);
    if (r != 0) {
        errno = r;
        return -1;
    }
    return nwrite;
}
extern "C" int close(int fd) { return _sys_close(fd); }