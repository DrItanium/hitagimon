/*
hitagimon
Copyright (c) 2020-2022, Joshua Scoggins
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
//
// Created by jwscoggins on 7/10/22.
//

#ifndef HITAGIMON_IAC_H
#define HITAGIMON_IAC_H
#include <stdint.h>
#include "SysExamine.h"
namespace cortex {
    struct IACMessage {
        uint16_t field2;
        uint8_t field1;
        uint8_t type;
        uint32_t field3;
        uint32_t field4;
        uint32_t field5;
    } __attribute__((packed));
   struct SystemBase {
       SystemAddressTable* theSAT;
       PRCB* thePRCB;
   };
   void triggerInterrupt(uint8_t interruptVector);
   void purgeInstructionCache();
   void reinitializeProcessor(SystemAddressTable* sat, PRCB* prcb, void (*start)());
   void setBreakpointRegister(uint32_t first, uint32_t second);
    __attribute__((unused)) void storeSystemBaseAddress(SystemBase* to);
   void testPendingInterrupts();
   uint32_t readInterruptState();
   void setInterruptState(uint32_t);
    void sendIAC(uint8_t type, uint8_t field1 = 0, uint16_t field2 = 0, uint32_t field3 = 0, uint32_t field4 = 0, uint32_t field5 = 0);
}
#endif //HITAGIMON_IAC_H
