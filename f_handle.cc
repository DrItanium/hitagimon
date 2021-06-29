/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
 * Fault handler routines
 */
#include "chipset/IODevice.h"
#include <stdio.h>
struct fault_data {
    volatile unsigned reserved;
    volatile unsigned override[3];
    volatile unsigned fdata[3];
    volatile unsigned override_data;
    volatile unsigned pc;
    volatile unsigned ac;
    volatile unsigned int	fsubtype:8,
            freserved:8,
            ftype:8,
            fflags:8;
    volatile unsigned int *faddress;
};
void
displayFaultData(fault_data* record) {
    printf("PC: %x\n", record->pc);
}
extern "C"
void
user_reserved(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER RESERVED FAULT RAISED!");
    displayFaultData(record);
}

extern "C"
void
user_trace(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER TRACE FAULT RAISED!");
    displayFaultData(record);
}

extern "C"
void
user_operation(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER OPERATION FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_arithmetic(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER ARITHMETIC FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_real_arithmetic(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER REAL ARITHMETIC FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_constraint(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER CONSTRAINT FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_protection(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER PROTECTION FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_machine(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER MACHINE FAULT RAISED!");
    displayFaultData(record);
}
extern "C"
void
user_type(fault_data* record) {
    TemporaryRWLoggingDisabler disable(getBasicChipsetInterface());
    getBasicChipsetInterface().writeLine("USER TYPE FAULT RAISED!");
    displayFaultData(record);
}
