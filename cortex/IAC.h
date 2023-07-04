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
#include "ModernCpp.h"
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
    void triggerInterrupt(uint8_t interruptVector) noexcept;
    void purgeInstructionCache() noexcept;
    void reinitializeProcessor(SystemAddressTable* sat, PRCB* prcb, void (*start)()) noexcept;
    void setBreakpointRegister(uint32_t first, uint32_t second) noexcept;
    void storeSystemBaseAddress(SystemBase* to) noexcept;
    SystemAddressTable* getSystemAddressTable() noexcept;
    PRCB* getPRCB() noexcept;
    SystemBase getSystemBase() noexcept;
    void testPendingInterrupts() noexcept;
    void sendIAC(uint8_t type, uint8_t field1 = 0, uint16_t field2 = 0, uint32_t field3 = 0, uint32_t field4 = 0, uint32_t field5 = 0);
    void sendIAC(IACMessage* msg) noexcept;
    inline void checkProcessNotice(uint32_t pcbSS) noexcept {
        sendIAC(0x90, 0, 0, pcbSS);
    }
    inline void continueInitialization() noexcept {
        sendIAC(0x92);
    }
    inline void flushLocalRegisters(uint32_t physicalStackPageAddress) noexcept {
        sendIAC(0x84, 0, 0, physicalStackPageAddress);
    }
    inline void flushProcess() noexcept { sendIAC(0x87); }
    inline void flushTLB() noexcept { sendIAC(0x8a); }
    inline void flushTLBPageTableEntry(uint32_t offsetFromSegmentBase, uint32_t ssOfSegmentThatContainsPage) noexcept {
        sendIAC(0x8C, 0, 0, offsetFromSegmentBase, ssOfSegmentThatContainsPage);
    }
    inline void flushTLBPhysicalPage(uint32_t basePhysicalAddressOfPage) noexcept {
        sendIAC(0x88, 0, 0, basePhysicalAddressOfPage);
    }
    inline void flushTLBSegmentEntry(uint32_t ssForSegment) noexcept {
        sendIAC(0x8B, 0, 0, ssForSegment);
    }
    inline void freeze() noexcept { sendIAC(0x91); }
    inline void modifyProcessorControls(uint32_t newProcessorControls, uint32_t mask) noexcept { sendIAC(0x8D, 0,0, newProcessorControls, mask); }
    inline void preemptProcess() noexcept { sendIAC(0x85); }
    inline void restartProcessor(uint32_t physicalAddressOfSegmentTable, uint32_t physicalAddressOfPRCB) noexcept {
        sendIAC(0x81, 0, 0, physicalAddressOfSegmentTable, physicalAddressOfPRCB);
    }
    inline void stopProcessor() noexcept { sendIAC(0x83); }
    inline void storeProcessor() noexcept { sendIAC(0x86); }
    inline void warmstartProcessor(uint32_t physicalAddressOfSegmentTable, uint32_t physicalAddressOfPRCB) noexcept {
        sendIAC(0x8e, 0, 0, physicalAddressOfSegmentTable, physicalAddressOfPRCB);
    }

}
#endif //HITAGIMON_IAC_H
