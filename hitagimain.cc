//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <newlib.h>
#include <iostream>
// incompatible with libstdc++!
// min and max macros must come last
#include <Arduino.h>

void
setup() {
    cortex::ChipsetBasicFunctions::Console::writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    std::cout << "NEWLIB Version: " << _NEWLIB_VERSION << std::endl;
    uint64_t start = cortex::getSystemCounter();
    printf("Staring counter: %#llx\n", start);
}
void loop() {
    uint64_t start = cortex::getSystemCounter();
    do {
        uint64_t now = cortex::getSystemCounter();
        uint64_t difference = now - start;
        if (difference >= 100) {
            printf("Counter: %#llx\n", now);
            break;
        }
    } while (true);
}

void yield() {

}

void initVariant() {

}

extern "C"
void
vect_INT0(void) {

}
extern "C"
void
vect_INT1(void) {

}
