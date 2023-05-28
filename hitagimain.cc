//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <cortex/builtins.h>
#include <newlib.h>
#include <iostream>
#include <cortex/ModernGCC.h>
#include <cortex/ChipsetInteract.h>

void
init()
{
    // first setup the interrupt control registers
    // it is int0 is lowest byte and so on
    __builtin_i960_set_interrupt_control_reg(0xFCFDFEFF);
    // setup the chipset basic functions as one of the first things we actually do
    cortex::ChipsetBasicFunctions::begin();
    // then setup the system clock to be 200 hz so that we trigger every 10 ms
    // we have a 16-bit counter so the prescalar is 8 (0b010) and the compare is 624
    // this is directly configuring timer 1 on the 2560 acting as chipset
    cortex::clearSystemCounter();
    cortex::enableSystemCounter(6249, 0x2);
}
void
setup() {
    std::cout << "HITAGIMON" << std::endl
              << "Built on " << __DATE__ << " at " << __TIME__ << std::endl
              << "--------------------------------------------" << std::endl << std::endl << std::endl << std::endl
              << "NEWLIB Version: " << _NEWLIB_VERSION << std::endl
              << "Starting counter: 0x" << std::hex << cortex::getSystemCounter() << std::endl;
}
template<bool specialSpaceIdentification>
void loop() {
    if (specialSpaceIdentification) {
        __builtin_i960_syncf();
        cortex::memory<uint32_t>(0xE0000000) = 0xABCDEF01;
    } else {
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
}

int main(void) {
    init();
    setup();
    for (;;) {
        loop<false>();
    }
    return 0;
}

extern "C"
void
vect_INT0(void) {

}
extern "C"
void
vect_INT1(void) {

}
