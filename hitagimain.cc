//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include <string>
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/ModernCpp.h>
#include <cortex/SystemCounter.h>
#include <arduino/Arduino.h>
#include <arduino/WCharacter.h>
#include <newlib.h>

void setup() {
    cortex::ChipsetBasicFunctions::Console::writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("NEWLIB Version: %s\n", _NEWLIB_VERSION);
    printf("Enabling system counter....");
    cortex::enableSystemCounter(97, 0x7);
    printf("done\n");
}
void loop() {
    uint64_t start = cortex::getSystemCounter();
    do {
        uint64_t now = cortex::getSystemCounter();
        uint64_t difference = now - start;
        if (difference >= 100) {
            printf("Counter: %x%x\n", static_cast<unsigned int>(now >> 32), static_cast<unsigned int>(now));
            break;
        }
    } while (true);
}

void yield() {

}

void initVariant() {

}