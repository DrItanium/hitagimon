//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include <string>
#include <cortex/IODevice.h>
#include <cortex/EnvironmentInterface.h>
#include <cortex/Interrupts.h>
#include <cortex/IAC.h>
#include <cortex/ModernCpp.h>
#include <cortex/SystemCounter.h>
#include <newlib.h>
#include <time.h>
//extern "C" int clipsMain(int argc, char *argv[]);
union TestStorage {
    uint32_t value;
    struct {
        uint8_t low;
        uint16_t mid;
        uint8_t high;
    }__attribute__((packed));
} __attribute__((packed));
void banner() noexcept {
    cortex::ChipsetBasicFunctions::Console::writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("NEWLIB Version: %s\n", _NEWLIB_VERSION);
    printf("Enabling system counter....");
    cortex::enableSystemCounter(97, 0x7);
    printf("done\n");
}
void doTestDiagnostics() noexcept {
    printf("Sizeof(int) = %lu\n", sizeof(int));
    printf("Testing out unaligned accesses!\n");
    volatile TestStorage tu;
    tu.value = 0xFDED1234;
    printf("tu.value = 0x%lx\n", tu.value);
    printf("tu.mid = 0x%x\n", tu.mid);
    printf("tu.high = 0x%x\n", tu.high);
    printf("tu.low = 0x%x\n", tu.low);
    printf("------\n") ;
    tu.mid = 0xFDED;
    printf("tu.value = 0x%lx\n", tu.value);
    printf("tu.mid = 0x%x\n", tu.mid);
    printf("tu.high = 0x%x\n", tu.high);
    printf("tu.low = 0x%x\n", tu.low);
    printf("-----\n");
    volatile uint64_t theValue64 = 0xABCDEF0123456789ull;
    printf("Testing out 64-bit reads and writes!\n");
    printf("OK? theValue64 = 0x%x%x but expecting 0xABCD'EF01'2345'6789\n", static_cast<unsigned>(theValue64), static_cast<unsigned>(theValue64 >> 32));
    theValue64 = 0x9876543210ABCDEFull;
    printf("OK? theValue64 = 0x%x%x but expecting 0x9876'5432'10AB'CDEF\n", static_cast<unsigned>(theValue64), static_cast<unsigned>(theValue64 >> 32));
}
int main() noexcept {
    banner();
    doTestDiagnostics();
    while (true) {
        uint64_t start = cortex::getSystemCounter();
        printf("Counter: %x%x\n", static_cast<unsigned int>(start), static_cast<unsigned int>(start >> 32));
        do {
            uint64_t difference = cortex::getSystemCounter() - start;
            if (difference >= 1000) {
                break;
            }
        } while (true);
    }
    //printf("Starting up CLIPS!\n");
    //clipsMain(0, args);
    exit(0);
    return 0;
}

