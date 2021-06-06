//
// Created by jwscoggins on 5/2/21.
//
#include <unistd.h>
#include <errno.h>
#include <stdint.h>
#include "IORoutines.h"

const size_t RamSize = 0x20000000;
const size_t RamStart = 0x80000000;
const size_t RamEnd = RamStart + RamSize;
static char* currentBreak = 0;
extern "C"
int brk(void* addr) {
#if 0
    if ((addr < reinterpret_cast<char*>(&end)) || (addr >= getStackStart())) {
        // ERROR
        return -1;
    }
    currentBreak = reinterpret_cast<char*>(addr);
#else
    return -1;
#endif
}
extern "C"
void* sbrk(intptr_t increment) {
#if 0
    char* newBreak = 0;
    char* oldBreak = 0;
    char* junk = 0;
    if (currentBreak == 0) {
        currentBreak = reinterpret_cast<char*>(&end);
    }
    newBreak = currentBreak + increment;

    if ((newBreak < reinterpret_cast<char*>(&end)) || (newBreak >= stack_start)) {
        return reinterpret_cast<void*>(-1);
    }

    for (char* junk = currentBreak; junk < newBreak; ++junk) {
        *junk = 0;
    }
    oldBreak = currentBreak;
    currentBreak = newBreak;
    return oldBreak;
#else
    return reinterpret_cast<void*>(-1);
#endif
}
