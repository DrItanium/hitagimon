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