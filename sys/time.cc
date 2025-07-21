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
#include <sys/time.h>
extern "C" {
    #include <sys/resource.h>
}
#include <stdint.h>
#include <cortex/SystemCounter.h>
#include <cortex/IODevice.h>

extern "C"
int
hitagi_getrusage(int who, struct rusage* usage) {
    //uint64_t counterAmounts = cortex::ChipsetBasicFunctions::Timer::millis();
    uint64_t counterAmounts = cortex::ChipsetBasicFunctions::Timer::micros();
    // this gives us the number of milliseconds overall
    uint64_t seconds = counterAmounts / 1000000; // 1 million microseconds in a second
    // dividing by 1000 will give us the number of seconds
    uint64_t remainder = counterAmounts % 1000000;
    // the remainder is in milliseconds
    // now we multiply that by 1000 to get the number of microseconds
    //uint64_t counterAmounts = cortex::getSystemCounter() * 10; // this is at a frequency of every 10 msec so we multiply it by 10 first
    //uint64_t seconds = counterAmounts / 1000;
    //uint64_t remainder = counterAmounts % 1000;
    //remainder *= 1000;
    usage->ru_utime.tv_sec = seconds;
    usage->ru_utime.tv_usec = remainder;
    usage->ru_stime.tv_sec = 0;
    usage->ru_stime.tv_usec = 0;
    return 0;
}

extern "C"
int
hitagi_setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) {
    /// @todo use arduino timers to satisfy this, we use the interrupts to trigger timers
    return 0;
}

extern "C"
int
hitagi_gettimeofday(struct timeval* tv, void*) {
    // read the unix time from the rtc connected to the microcontroller
    uint32_t theTime = cortex::ChipsetBasicFunctions::Timer::unixtime();
    tv->tv_sec = theTime;
    tv->tv_usec = theTime * 1000000;
    return 0;
}

extern "C" int _sys_getrusage(int who, struct rusage*);
extern "C" int getrusage(int who, struct rusage* usage) { return _sys_getrusage(who, usage); }
extern "C" int _sys_gettimeofday(struct timeval*, void*);
extern "C" int gettimeofday(struct timeval* tv, void* tz) { return _sys_gettimeofday(tv, tz); }
extern "C" int _sys_setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue);
extern "C" int setitimer(int which, const struct itimerval* newValue, struct itimerval* oldValue) { return _sys_setitimer(which, newValue, oldValue); }
