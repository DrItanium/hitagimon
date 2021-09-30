//
// Created by jwscoggins on 8/28/21.
//

#include "Faults.h"
#include <stdio.h>
namespace cortex
{
    void
    FaultData::display() {
        printf("Fault Type: %x\n", ftype);
        switch (ftype) {
            case 1: printf("\tTrace Fault"); break;
            case 2: printf("\tOperation Fault"); break;
            case 3: printf("\tArithmetic Fault"); break;
            case 4: printf("\tFloating-Point Fault"); break;
            case 5: printf("\tConstraint Fault"); break;
            case 7: printf("\tProtection Fault"); break;
            case 0xa: printf("\tType Fault"); break;
            default:
                break;
        }
        printf("Fault Subtype: %x\n", fsubtype);
        if (ftype == 2) {
            if (fsubtype == 1) {
                printf("\tInvalid Opcode\n");
            } else if (fsubtype == 4) {
                printf("\tInvalid Operand\n");
            }
        }
        printf("Faulting Address: %p\n", faddress);
        printf("PC: %x\n", pc);
        printf("AC: %x\n", ac);
    }
}