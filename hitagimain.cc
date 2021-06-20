//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"
#include <string>
#include "EnvironmentInterface.h"

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
void setupEnvironmentVariables() {
    EnvironmentInterface::set("HOME", "/");
}
int main() {
    ChipsetBasicFunctions& theChipset = getBasicChipsetInterface();
    BuiltinTFTDisplay& theDisplay = getDisplay();
    theChipset.writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("Setting up Environment Variables....{\n");
    setupEnvironmentVariables();
    printf("}...Done\n");
    printf("STARTING UP DOOM!!!\n");
    theDisplay.clearScreen();
    theChipset.toggleLED();
    doommain(1, args);
    return 0;
}

