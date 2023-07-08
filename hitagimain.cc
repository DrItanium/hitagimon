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
#include <third-party/tinyscheme-1.42/scheme.h>

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
printSegmentDescriptor(std::ostream& out, cortex::SegmentDescriptor& curr) {
    out << "\t\t\tValid: " << std::boolalpha << curr.operator bool() << std::endl;
    if (curr) {
        out << "\t\t\tKind: ";
        if (curr.isSmallSegmentTable()) {
            out << "small segment" << std::endl;
        } else if (curr.isLargeSegmentTable()) {
            out << "large segment" << std::endl;
        } else if (curr.isSimpleRegion()) {
            out << "simple region" << std::endl;
        } else if (curr.isProcedureTable()) {
            out << "procedure table" << std::endl;
        } else {
            out << "unknown" << std::endl;
        }
    }
    out << "\t\t\tData:" << std::endl;
    for (int j = 0; j < 4; ++j) {
        out << "\t\t\t\t" << std::dec << j << ": 0x" << std::hex << curr.backingStorage[j] << std::endl;
    }
}
int
computeSegmentSelector(int index) noexcept {
    return static_cast<int>((index << 6) | 0x3f);
}
void
printBaseSegmentTable(std::ostream& out, cortex::SegmentTable& segTable, int count) {
    out << "\tKind: ";
    if (segTable.isSmallSegmentTable()) {
        out << "Small" << std::endl;
    } else if (segTable.isLargeSegmentTable()) {
        out << "Large" << std::endl;
    } else {
        out << "Unknown!" << std::endl;
    }
    // print out the first eight entries
    out << "\tFirst " << std::dec << count << " Entries:" << std::endl;
    for (int i = 0; i < count; ++i) {
        out << "\t\t" << std::dec << i << " (0x" << std::hex << computeSegmentSelector(i) << "):" << std::endl;
        printSegmentDescriptor(out, segTable.getDescriptor(i));
    }
}
void
setup() {
    std::cout << "HITAGIMON" << std::endl
              << "Built on " << __DATE__ << " at " << __TIME__ << std::endl
              << "--------------------------------------------" << std::endl << std::endl << std::endl << std::endl
              << "NEWLIB Version: " << _NEWLIB_VERSION << std::endl
              << "Starting counter: 0x" << std::hex << cortex::getSystemCounter() << std::endl;

    std::cout << "Boot Words information: " << std::endl;
    const volatile cortex::BootWords& theWords = cortex::getBootWords();
    std::cout << "\tSegment Table Start: " << std::hex << theWords.sat << std::endl;
    std::cout << "\tBoot PRCB Start: " << std::hex << theWords.thePRCB << std::endl;
    std::cout << "\tFirst Instruction starts at: 0x" << std::hex << reinterpret_cast<uintptr_t>(theWords.firstInstruction) << std::endl;
    cortex::SegmentTable& segTable = *theWords.sat;
    std::cout << "Boot Segment Table Information: " << std::endl;
    printBaseSegmentTable(std::cout, segTable, 12);
    cortex::SegmentTable& currTable = *cortex::getSystemAddressTable();
    std::cout << "Current Segment Table Information: " << std::endl;
    printBaseSegmentTable(std::cout, currTable, 12);
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

char* argv[] = { "hitagimon", };
int main(void) {
    init();
    setup();
    int argc = 1;
    int result = scheme_main(argc, argv);
    for (;;) {
        loop<false>();
    }
    return result;
}

extern "C"
void
vect_INT0(void) {

}
extern "C"
void
vect_INT1(void) {

}
