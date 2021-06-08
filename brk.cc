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

extern "C"
int
_sys_write(int fd, const void* buf, size_t sz, int* nwrite) {
    switch (fd) {
        case STDIN_FILENO:
            break;
        case STDOUT_FILENO:
            break;
        case STDERR_FILENO:
            break;
        default:
            errno = EBADF;
            return -1;
    }
    *nwrite = 0;
    return 0;
}
const size_t RamSize = 0x20000000;
const size_t RamStart = 0x80000000;
const size_t RamEnd = RamStart + RamSize;
static char* heapEnd = 0;
extern "C"
int brk(void* ptr) {
    heapEnd = reinterpret_cast<char*>(ptr);
    return 0;
}
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
    /// @todo check user's permissions for a file, this will be found on the SD Card. so this path needs to be passed to the 1284p
    return 0;
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
    int r = _sys_write(fd, buf, sz, &numWritten);
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
    int r = 0;
#if 0
    r = _sys_read (fd, buf, sz, &nread);
#else
    r = 0;
#endif
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
    exit (signal);
}

extern "C"
int
open (char* file, int mode, int perms) {
    /// @todo interface this function with the SDCard
    return 0;
}