//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <arduino/Arduino.h>
#include <newlib.h>
uint16_t colors[256];
void setup() {
    cortex::ChipsetBasicFunctions::Console::writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("NEWLIB Version: %s\n", _NEWLIB_VERSION);
    printf("Staring counter: %#llx\n", cortex::getSystemCounter());
    for (int i = 0; i < 256; ++i) {
        colors[i] = cortex::ChipsetBasicFunctions::Display::color565(
                static_cast<uint8_t>(random()),
                static_cast<uint8_t>(random()),
                static_cast<uint8_t>(random()));
    }
    for (int16_t x = 0; x < 240; ++x) {
        cortex::ChipsetBasicFunctions::Display::startWrite();
        for (int16_t y = 0; y < 320; ++y) {
            cortex::ChipsetBasicFunctions::Display::writePixel( x, y, colors[static_cast<uint8_t>(random())]);
        }
        cortex::ChipsetBasicFunctions::Display::endWrite();
    }
    for (int16_t x = 0; x < 240; ++x) {
        for (int16_t y = 0; y < 320; ++y) {
            cortex::ChipsetBasicFunctions::Display::startWrite();
            for (int16_t h = 0; h < 320; ++h) {
                for (int16_t w = 0; w < 240; ++w) {
                    cortex::ChipsetBasicFunctions::Display::writeFillRect(x, y, w, h, colors[static_cast<uint8_t>(random())]);
                }
            }
            cortex::ChipsetBasicFunctions::Display::endWrite();
        }
    }
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
