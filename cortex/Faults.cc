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
        uint8_t ftype = static_cast<uint8_t>(record.type >> 16);
        uint8_t fsubtype = static_cast<uint8_t>(record.type) ;

        printf("Fault Type: %x\n", ftype);
        switch (ftype) {
            case 0: printf("\tOverride Fault\n"); break;
            case 1: printf("\tTrace Fault\n"); break;
            case 2: printf("\tOperation Fault\n"); break;
            case 3: printf("\tArithmetic Fault\n"); break;
            case 4: printf("\tFloating-Point Fault\n"); break;
            case 5: printf("\tConstraint Fault\n"); break;
            case 6: printf("\tVirtual Memory Fault\n"); break;
            case 7: printf("\tProtection Fault\n"); break;
            case 8: printf("\tMachine Fault\n"); break;
            case 9: printf("\tStructural Fault\n"); break;
            case 0xa: printf("\tType Fault\n"); break;
            case 0xc: printf("\tProcess Fault\n"); break;
            case 0xd: printf("\tDescriptor Fault\n"); break;
            case 0xe: printf("\tEvent Fault\n"); break;
            default:
                break;
        }
        printf("Fault Subtype: %x\n", fsubtype);
        if (ftype == 2) {
            // load and display the operation in question
            if (fsubtype == 1) {
                printf("\tInvalid Opcode\n");
            } else if (fsubtype == 4) {
                printf("\tInvalid Operand\n");
            }
            volatile uint32_t* ptr = record.addr;
            unsigned int container[8] = { 0 };
            for (int i = 0;i < 8; ++i, ++ptr) {
                container[i] = *ptr;
            }
            printf("\t\tContents: \n");
            for (int i = 0; i < 8; ++i) {
               printf("\t\t\t0x%x\n", container[i]);
            }
        }
        printf("Faulting Address: %p\n", record.addr);
        printf("PC: %#lx\n", record.pc);
        printf("AC: %#lx\n", record.ac);
    }
#define X(kind) \
namespace {     \
    FaultHandler user ## kind ## _ ; \
}               \
FaultHandler getUser ## kind ## FaultHandler () { return user ## kind ## _ ; } \
void setUser ## kind ## FaultHandler (FaultHandler handler) { user ## kind ## _ = handler; }
    X(Override);
    X(Trace);
    X(Operation);
    X(Arithmetic);
    X(FloatingPoint);
    X(Constraint);
    X(VirtualMemory);
    X(Protection);
    X(Machine);
    X(Structural);
    X(Type);
    X(Process);
    X(Descriptor);
    X(Event);
#undef X
}