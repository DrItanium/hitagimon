//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"

extern "C" int doommain (int argc, char** argv) ;
uint64_t delay(uint64_t count) {
    volatile uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}
int main() {
    BuiltinLED theLED;
    BuiltinTFTDisplay theDisplay;
    BuiltinConsole theConsole;
    theDisplay.clearScreen();
    volatile uint64_t count = 0;
    theLED.toggle();
    while (true) {
        theLED.toggle();
        count += delay(1000);
    }
    //doommain(0, 0);
    return count;
}

