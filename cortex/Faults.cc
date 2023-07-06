//
// Created by jwscoggins on 8/28/21.
//

#include "ModernGCC.h"
#include "Faults.h"
#include "ChipsetInteract.h"
#include <stdio.h>
#include <stdint.h>
namespace cortex
{
    void
    FaultData::display() {
        uint8_t ftype = faultInfo.type;
        uint8_t fsubtype = faultInfo.subtype;

        printf("Fault Type: %x\n", ftype);
        switch (getFaultKind()) {
#define X(kind, index, locase, hicase) case UserFaultKind:: kind : printf ("\t" #kind " Fault\n" ); break;
#include "cortex/Faults.def"
#undef X
            default:
                break;
        }
        printf("Fault Subtype: %x\n", fsubtype);
        if (getFaultKind() == UserFaultKind::Operation) {
            // load and display the operation in question
            if (fsubtype == 1) {
                printf("\tInvalid Opcode\n");
            } else if (fsubtype == 4) {
                printf("\tInvalid Operand\n");
            }
            volatile uint32_t* ptr = addressOfFaultingInstruction;
            unsigned int container[8] = { 0 };
            for (int i = 0;i < 8; ++i, ++ptr) {
                container[i] = *ptr;
            }
            printf("\t\tContents: \n");
            for (int i = 0; i < 8; ++i) {
               printf("\t\t\t0x%x\n", container[i]);
            }
        }
        printf("Faulting Address: %p\n", addressOfFaultingInstruction);
        printf("PC: %#lx\n", pc.raw);
        printf("AC: %#lx\n", ac.raw);
    }
#define X(kind, index, locase, hicase) \
namespace { FaultHandler user ## kind ## _ ; } \
FaultHandler getUser ## kind ## FaultHandler () { return user ## kind ## _ ; } \
void setUser ## kind ## FaultHandler (FaultHandler handler) { user ## kind ## _ = handler; }
#include "cortex/Faults.def"
#undef X
}