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
#include "IAC.h"
#include "ModernCpp.h"
extern "C" void sendIACCommand(uint32_t iacPort, void* theMessage);
extern "C" uint32_t hitagi_readInterruptState();
extern "C" void hitagi_writeInterruptState(uint32_t value);
namespace cortex {
    struct IACMessage {
        uint16_t field2;
        uint8_t field1;
        uint8_t type;
        uint32_t field3;
        uint32_t field4;
        uint32_t field5;
    } __attribute__((packed));
    void
    sendIAC(uint8_t type, uint8_t field1 = 0, uint16_t field2 = 0, uint32_t field3 = 0, uint32_t field4 = 0, uint32_t field5 = 0) {
        IACMessage theMessage;
        theMessage.type = type;
        theMessage.field1 = field1;
        theMessage.field2 = field2;
        theMessage.field3 = field3;
        theMessage.field4 = field4;
        theMessage.field5 = field5;
        sendIACCommand(0, &theMessage);
    }

    void
    triggerInterrupt(uint8_t interruptVector) {
        sendIAC(0x40, interruptVector);
    }
    void
    purgeInstructionCache() {
        sendIAC(0x89);
    }
    void
    reinitializeProcessor(SystemAddressTable* sat, PRCB* prcb, void (*start)()) {
        sendIAC(0x93, 0,
                      0,
                      reinterpret_cast<uint32_t>(sat),
                      reinterpret_cast<uint32_t>(prcb),
                      reinterpret_cast<uint32_t>(start));
    }
    void
    setBreakpointRegister(uint32_t first, uint32_t second) {
        sendIAC(0x8F, 0, 0, first, second);
    }
    void
    storeSystemBaseAddress(SystemBase* to) {
        sendIAC(0x80, 0, 0, reinterpret_cast<uint32_t>(to));
    }
    void testPendingInterrupts() {
        sendIAC(0x41);
    }
    uint32_t readInterruptState() {
        return hitagi_readInterruptState();
    }
    void setInterruptState(uint32_t value) {
        hitagi_writeInterruptState(value);
    }
}