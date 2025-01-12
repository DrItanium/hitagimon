//
// Created by jwscoggins on 6/7/21.
//
#include <cortex/IODevice.h>
extern "C" void setupInterruptHandler();
extern "C" void initFP();
extern "C" void _init();
void
init()
{
    // these routines must be first!
    initFP();
    setupInterruptHandler();
    _init(); // must be first!
    // setup the chipset basic functions as one of the first things we actually do
    cortex::ChipsetBasicFunctions::begin();
    // make sure that we configured the c runtime to not buffer the inputs and outputs
    // this should allow each character to be printed out
}
void
setup() {
}
void loop() {
}


int main(int argc, char** argv) {
    init();
    setup();
    for (;; ) {
        loop();
    }
    return 0;
}
