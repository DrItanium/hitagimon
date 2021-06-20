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
#include "IODevice.h"

namespace
{
    int
    sys_write(int fd, const void *buf, size_t sz, int &nwrite) {
        nwrite = 0;
        switch (fd) {
            case STDOUT_FILENO:
            case STDERR_FILENO:
                nwrite = getBasicChipsetInterface().write(reinterpret_cast<char *>(const_cast<void *>(buf)), sz);
                break;
            case STDIN_FILENO:
            default:
                return EBADF;
        }
        return 0;
    }
    int
    sys_read(int fd, void *buf, size_t sz, int &nread) {
        //char* theBuf = reinterpret_cast<char*>(buf);
        nread = 0;
        switch (fd) {
            case STDIN_FILENO:
                nread = getBasicChipsetInterface().read(reinterpret_cast<char *>(buf), sz);
                break;
            default:
                return EBADF;
        }
        return 0;
    }
}
const size_t RamSize = 0x20000000;
const size_t RamStart = 0x80000000;
const size_t RamEnd = RamStart + RamSize;
static char* heapEnd = 0;
#if 0
extern "C"
int brk(void* ptr) {
    heapEnd = reinterpret_cast<char*>(ptr);
    return 0;
}
#endif
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

namespace {
    off_t
    sys_lseek(int fd, off_t offset, int type) {
        return 0;
    }
}
extern "C"
off_t
lseek(int fd, off_t offset, int type) {
    /// @todo implement this using an SD Card interface
    return 0;
}

extern "C"
int
setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) {
    /// @todo use arduino timers to satisfy this, we use the interrupts to trigger timers
    return 0;
}

extern "C"
int access(const char* pathName, int mode) {
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
write (int fd, const void* buf, size_t sz) {
    int numWritten = 0;
    /// @todo Implement _sys_write equivalent
    int r = sys_write(fd, buf, sz, numWritten);
    if (r != 0) {
        errno = r;
        return -1;
    }
    return numWritten;
}
extern "C"
int
read (int fd, void* buf, size_t sz)
{
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
    return 0;
}

extern "C"
int
close(int fd) {
    /// @todo implement closing files on the SD Card
    return 0;
}

extern "C"
void
_exit(int status) {
    while (true) {
       // just hang here
    }
}

extern "C"
int
kill (int pid, int signal) {
    getBasicChipsetInterface().writeLine("KILLING PROCESS!!!!");
    exit (signal);
}
namespace {
}
extern "C"
int
open (char* file, int flags) {
    /// @todo interface this function with the SDCard
    /// @todo write the _sys_open function
    int result = getSDCardInterface().openFile(file, flags);
    if (result != -1) {
        result =+ 3; // skip past the stdin/stderr/stdout ids
    }
    return result;
}