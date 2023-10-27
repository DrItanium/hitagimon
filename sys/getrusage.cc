//
// Created by jwscoggins on 7/23/23.
//
extern "C" {
#include <sys/resource.h>
}
#include <cortex/SystemCounter.h>
extern "C" int _sys_getrusage(int who, struct rusage*);
extern "C"
int
getrusage(int who, struct rusage* usage) {
    return _sys_getrusage(who, usage);
}
extern "C"
int
hitagi_getrusage(int who, struct rusage* usage) {
    uint64_t counterAmounts = cortex::getSystemCounter() * 10; // this is at a frequency of every 10 msec so we multiply it by 10 first
    // this gives us the number of milliseconds overall
    uint64_t seconds = counterAmounts / 1000;
    // dividing by 1000 will give us the number of seconds
    uint64_t remainder = counterAmounts % 1000;
    // the remainder is in milliseconds
    // now we multiply that by 1000 to get the number of microseconds
    remainder *= 1000;
    usage->ru_utime.tv_sec = seconds;
    usage->ru_utime.tv_usec = remainder;
    usage->ru_stime.tv_sec = 0;
    usage->ru_stime.tv_usec = 0;
    return 0;
}
