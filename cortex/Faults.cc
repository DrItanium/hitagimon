//
// Created by jwscoggins on 8/28/21.
//

#include "Faults.h"
#include <stdio.h>
namespace cortex
{
    void
    FaultData::display() {
        printf("PC: %x\n", pc);
    }
}