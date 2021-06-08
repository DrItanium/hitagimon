//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"

BuiltinLED theLED;
BuiltinTFTDisplay theDisplay;
BuiltinConsole theConsole;
extern "C" int doommain (int argc, char** argv) ;
uint64_t wait(uint64_t count) {
    volatile uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}
int main() {
    for (uint64_t i = 0; i < 0xFDED; ++i){
        theLED.toggle();
        delay(10000000);
    }
    doommain(0, 0);
    return 0;
}

