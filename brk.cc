//
// Created by jwscoggins on 5/2/21.
//
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include <sys/stat.h>
#include "IORoutines.h"

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
#if 0
    int r = _sys_write(fd, buf, sz, &numWritten);
#else
    int r = 0;
#endif
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
