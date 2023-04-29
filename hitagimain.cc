//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <arduino/Arduino.h>
#include <newlib.h>
#include <math.h>
uint16_t colors[256];
uint16_t colorBlack = 0;
void screenFillTest() noexcept {
    cortex::ChipsetBasicFunctions::Console::writeLine("screenFillTest");
    for (int i = 0; i < 256; ++i) {
        cortex::ChipsetBasicFunctions::Display::fillScreen(colors[i]);
    }
    cortex::ChipsetBasicFunctions::Display::fillScreen(colorBlack);
}
void
pixelFillTest() noexcept {
    cortex::ChipsetBasicFunctions::Console::writeLine("pixelFillTest");
    int16_t width = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayWidth());
    int16_t height = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayHeight());
    for (int16_t x = 0; x < width; ++x) {
        cortex::ChipsetBasicFunctions::Display::startWrite();
        for (int16_t y = 0; y < height; ++y) {
            cortex::ChipsetBasicFunctions::Display::writePixel( x, y, colors[static_cast<uint8_t>(random())]);
        }
        cortex::ChipsetBasicFunctions::Display::endWrite();
    }
}
void
squareFillTest() noexcept {
    cortex::ChipsetBasicFunctions::Console::writeLine("squareFillTest");
    int16_t width = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayWidth());
    int16_t height = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayHeight());
    for (int16_t x = 0; x < width; x += 64) {
        for (int16_t y = 0; y < height; y += 64) {
            for (int16_t h = 0; h < height; h += 64) {
                cortex::ChipsetBasicFunctions::Display::startWrite();
                for (int16_t w = 0; w < width; w += 64) {
                    cortex::ChipsetBasicFunctions::Display::writeFillRect(x, y, w, h, colors[static_cast<uint8_t>(random())]);
                }
                cortex::ChipsetBasicFunctions::Display::endWrite();
            }
        }
    }
}
void
roundRectTest() noexcept {
    cortex::ChipsetBasicFunctions::Console::writeLine("roundRectTest");
    int16_t width = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayWidth());
    int16_t height = static_cast<int16_t>(cortex::ChipsetBasicFunctions::Display::getDisplayHeight());
    int cx = width / 2 - 1;
    int cy = height / 2 - 1;
    cortex::ChipsetBasicFunctions::Display::fillScreen(colorBlack);
    int w = min(width, height);
    for (int i = 0; i < w; i+= 6) {
        int i2 = i / 2;
        cortex::ChipsetBasicFunctions::Display::drawRoundRect(
                cx - i2,
                cy - i2,
                i, i, i / 8,
                cortex::ChipsetBasicFunctions::Display::color565(i, 0, 0) );
    }
}
void setup() {
    cortex::ChipsetBasicFunctions::Console::writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("NEWLIB Version: %s\n", _NEWLIB_VERSION);
    printf("Staring counter: %#llx\n", cortex::getSystemCounter());
    colorBlack = cortex::ChipsetBasicFunctions::Display::color565(0,0,0);
    for (int i = 0; i < 256; ++i) {
        colors[i] = cortex::ChipsetBasicFunctions::Display::color565(
                static_cast<uint8_t>(random()),
                static_cast<uint8_t>(random()),
                static_cast<uint8_t>(random()));
    }
    screenFillTest();
    pixelFillTest();
    squareFillTest();
    roundRectTest();
    cortex::ChipsetBasicFunctions::Console::writeLine("done");

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
