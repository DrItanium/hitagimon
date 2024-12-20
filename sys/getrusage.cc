//
// Created by jwscoggins on 7/23/23.
//
extern "C" {
#include <sys/resource.h>
}
#include <cortex/SystemCounter.h>
#include <cortex/IODevice.h>
extern "C" int sys_getrusage(int who, struct rusage*);
extern "C"
int
getrusage(int who, struct rusage* usage) {
    return sys_getrusage(who, usage);
}
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
