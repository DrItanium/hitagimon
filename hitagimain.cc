//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "chipset/IODevice.h"
#include <string>
#include "cortex/EnvironmentInterface.h"

//extern "C" int doommain (int argc, char** argv) ;
extern "C" int clipsMain(int argc, char *argv[]);
uint64_t delay(uint64_t count) {
    uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}
union TestStorage {
    uint32_t value;
    struct {
        uint8_t low;
        uint16_t mid;
        uint8_t high;
    }__attribute__((packed));
} __attribute__((packed));
char* args[] = { };
void setupEnvironmentVariables() {
    cortex::EnvironmentInterface::set("HOME", "/home");
    cortex::EnvironmentInterface::set("DOOMWADDIR", "/home/wads");
}
int main() {
    ChipsetBasicFunctions& theChipset = getBasicChipsetInterface();
    BuiltinTFTDisplay& theDisplay = getDisplay();
    theChipset.writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("Sizeof(int) = %d\n", sizeof(int));
    printf("Setting up Environment Variables....{\n");
    setupEnvironmentVariables();
    printf("}...Done\n");
    printf("Testing out unaligned accesses!\n");
    volatile TestStorage tu;
    tu.value = 0xFDED1234;
    printf("tu.value = 0x%x\n", tu.value);
    printf("tu.mid = 0x%x\n", tu.mid);
    printf("tu.high = 0x%x\n", tu.high);
    printf("tu.low = 0x%x\n", tu.low);
    printf("------\n") ;
    tu.mid = 0xFDED;
    printf("tu.value = 0x%x\n", tu.value);
    printf("tu.mid = 0x%x\n", tu.mid);
    printf("tu.high = 0x%x\n", tu.high);
    printf("tu.low = 0x%x\n", tu.low);
    printf("-----\n");
    volatile uint64_t theValue64 = 0xABCDEF0123456789ull;
    printf("Testing out 64-bit reads and writes!\n");
    printf("theValue64 = 0x%llux but expecting 0xABCD'EF01'2345'6789\n", theValue64);
    theValue64 = 0x9876543210ABCDEFull;
    printf("theValue64 = 0x%llux but expecting 0x9876'5432'10AB'CDEF\n", theValue64);
    theDisplay.clearScreen();
    clipsMain(0, args);
    return 0;
}

