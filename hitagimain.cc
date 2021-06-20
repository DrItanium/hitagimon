//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"
#include <string>
#include <iostream>

extern "C" int doommain (int argc, char** argv) ;
uint64_t delay(uint64_t count) {
    uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}

char* args[] = {
    "@DOOMARGS.TXT"
};
int main() {
    ChipsetBasicFunctions& theChipset = getBasicChipsetInterface();
    BuiltinTFTDisplay& theDisplay = getDisplay();
    theChipset.writeLine("Entered main!");
    std::string str("the string states \"donuts are very tasty!\"");
    theChipset.writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    theChipset.writeLine("donuts are very tasty!");
    theChipset.writeLine(str.c_str());
    theDisplay.clearScreen();
    //volatile uint64_t count = 0;
    //volatile uint16_t prevColor = 0xFFFF;
    theChipset.toggleLED();
    //while (true) {
    //    for (uint32_t color = 0; color <= 0x1000000; ++color) {
    //        uint16_t curr = theDisplay.color565(color);
    //        if (curr != prevColor) {
    //            if (color & 0x100) {
    //                theChipset.toggleLED();
    //            }
    //            theDisplay.fillScreen(curr);
    //            prevColor = curr;
    //        }
    //    }
    //    count += delay(1000);
    //}
    doommain(1, args);
    return 0;
}

