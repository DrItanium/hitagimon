//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
#include <cortex/IAC.h>
#include <cortex/SystemCounter.h>
#include <cortex/builtins.h>
#include <newlib.h>
#include <iostream>

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
    //volatile cortex::Timer16& t1 = cortex::ChipsetBasicFunctions::Timer::getTimer1();
    //volatile cortex::Timer16& t2 = cortex::ChipsetBasicFunctions::Timer::getTimer2();
    //volatile cortex::Timer16& t3 = cortex::ChipsetBasicFunctions::Timer::getTimer3();
    //t0.begin();
    //t1.begin();
    //t2.begin();
    //t3.begin();
    cortex::clearSystemCounter();
    cortex::enableSystemCounter(6249, 0x2);
    // setup the prescalar values (6249, 0x2)
}
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

int main(void) {
    init();
    setup();
    for (;;) {
        loop();
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
