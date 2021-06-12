//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"
#include <string>

BuiltinLED theLED;
extern "C" int doommain (int argc, char** argv) ;
uint64_t delay(uint64_t count) {
    uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}
int main() {
    BuiltinTFTDisplay theDisplay;
    BuiltinConsole theConsole;
    BuiltinChipsetDebugInterface debugInterface;
    theConsole.write('d');
    theConsole.write('o');
    theConsole.write('n');
    theConsole.write('u');
    theConsole.write('t');
    theConsole.write('s');
    theConsole.write(' ');
    theConsole.writeLine("are very tasty!");
    theConsole.flush();
    theDisplay.clearScreen();
    volatile uint64_t count = 0;
    volatile uint16_t prevColor = 0xFFFF;
    theLED.toggle();
    while (true) {
        for (uint32_t color = 0; color <= 0x1000000; ++color) {
            uint16_t curr = theDisplay.color565(color);
            if (curr != prevColor) {
                if (color & 0x100) {
                    theLED.toggle();
                }
                theDisplay.fillScreen(curr);
                prevColor = curr;
            }
        }
        count += delay(1000);
    }
    //doommain(0, 0);
    return count;
}

