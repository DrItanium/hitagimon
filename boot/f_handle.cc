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
#include <string>
void
basicDisplay(const std::string& kind, cortex::FaultData* record) {
    getBasicChipsetInterface().write(kind.c_str());
    getBasicChipsetInterface().writeLine(" FAULT RAISED!");
    record->display();
}

extern "C"
void
user_reserved(cortex::FaultData* record) {
    basicDisplay("USER RESERVED", record);
}

extern "C"
void
user_trace(cortex::FaultData* record) {
    basicDisplay("USER TRACE", record);
}

extern "C"
void
user_operation(cortex::FaultData* record) {
    basicDisplay("USER OPERATION", record);
}
extern "C"
void
user_arithmetic(cortex::FaultData* record) {
    basicDisplay("USER ARITHMETIC", record);
}
extern "C"
void
user_real_arithmetic(cortex::FaultData* record) {
    basicDisplay("USER REAL ARITHMETIC", record);
}
extern "C"
void
user_constraint(cortex::FaultData* record) {
    basicDisplay("USER CONSTRAINT", record);
}
extern "C"
void
user_protection(cortex::FaultData* record) {
    basicDisplay("USER PROTECTION", record);
}
extern "C"
void
user_machine(cortex::FaultData* record) {
    basicDisplay("USER MACHINE", record);
}
extern "C"
void
user_type(cortex::FaultData* record) {
    basicDisplay("USER TYPE", record);
}
