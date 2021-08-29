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
#include "../chipset/IODevice.h"
#include "../cortex/Faults.h"
extern "C"
void
user_reserved(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER RESERVED FAULT RAISED!");
    record->display();
}

extern "C"
void
user_trace(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER TRACE FAULT RAISED!");
    record->display();
}

extern "C"
void
user_operation(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER OPERATION FAULT RAISED!");
    record->display();
}
extern "C"
void
user_arithmetic(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER ARITHMETIC FAULT RAISED!");
    record->display();
}
extern "C"
void
user_real_arithmetic(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER REAL ARITHMETIC FAULT RAISED!");
    record->display();
}
extern "C"
void
user_constraint(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER CONSTRAINT FAULT RAISED!");
    record->display();
}
extern "C"
void
user_protection(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER PROTECTION FAULT RAISED!");
    record->display();
}
extern "C"
void
user_machine(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER MACHINE FAULT RAISED!");
    record->display();
}
extern "C"
void
user_type(cortex::FaultData* record) {
    getBasicChipsetInterface().writeLine("USER TYPE FAULT RAISED!");
    record->display();
}
