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
#include <cstdint>
#include "SysExamine.h"
#include "ModernCpp.h"
namespace cortex {
    class IACMessage {
    public:
        enum IACMessageKind {
            Interrupt = 0x40,
            TestPendingInterrupts = 0x41,
            StoreSystemBase = 0x80,
            RestartProcessor = 0x81,
            StopProcessor = 0x83,
            FlushLocalRegisters = 0x84,
            PreemptProcess = 0x85,
            StoreProcessor = 0x86,
            FlushProcess = 0x87,
            FlushTLBPhysicalPage = 0x88,
            PurgeInstructionCache = 0x89,
            FlushTLB = 0x8a,
            FlushTLBSegmentEntry = 0x8b,
            FlushTLBPageTableEntry = 0x8c,
            ModifyProcessorControls = 0x8d,
            WarmstartProcessor = 0x8e,
            SetBreakpointRegister = 0x8f,
            CheckProcessNotice = 0x90,
            Freeze = 0x91,
            ContinueInitialization = 0x92,
            ReinitializeProcessor = 0x93,
        };
    public:
        IACMessage(uint8_t t = 0, uint8_t f1 = 0, uint16_t f2 = 0, uint32_t f3 = 0, uint32_t f4 = 0, uint32_t f5 = 0) : field2(f2), field1(f1), type(t), field3(f3), field4(f4), field5(f5) { }
        inline uint8_t getType() const noexcept { return type; }
        inline uint8_t getField1() const noexcept { return field1; }
        inline uint16_t getField2() const noexcept { return field2; }
        inline uint32_t getField3() const noexcept { return field3; }
        inline uint32_t getField4() const noexcept { return field4; }
        inline uint32_t getField5() const noexcept { return field5; }
    private:
        uint16_t field2;
        uint8_t field1;
        uint8_t type;
        uint32_t field3;
        uint32_t field4;
        uint32_t field5;
    } __attribute__((packed));
    struct SystemBase {
        SegmentTable* theSAT;
        PRCB* thePRCB;
    };
    SegmentTable* getSystemAddressTable() noexcept;
    PRCB* getPRCB() noexcept;
    SystemBase getSystemBase() noexcept;
    void sendIAC(uint8_t type, uint8_t field1 = 0, uint16_t field2 = 0, uint32_t field3 = 0, uint32_t field4 = 0, uint32_t field5 = 0);
    void sendIAC(IACMessage* msg) noexcept;
    inline void triggerInterrupt(uint8_t interruptVector) noexcept {
        sendIAC(IACMessage::Interrupt, interruptVector);
    }
    inline void reinitializeProcessor(SegmentTable* sat, PRCB* prcb, void (*start)()) noexcept {
        sendIAC(IACMessage::ReinitializeProcessor,
                0,
                0,
                reinterpret_cast<uint32_t>(sat),
                reinterpret_cast<uint32_t>(prcb),
                reinterpret_cast<uint32_t>(start));
    }
    inline void setBreakpointRegister(uint32_t first, uint32_t second) noexcept {
        sendIAC(IACMessage::SetBreakpointRegister, 0, 0, first, second);
    }
    inline void storeSystemBaseAddress(SystemBase* to) noexcept {
        sendIAC(IACMessage::StoreSystemBase, 0, 0, reinterpret_cast<uint32_t>(to));
    }
    inline void purgeInstructionCache() noexcept {
        sendIAC(IACMessage::PurgeInstructionCache);
    }
    inline void testPendingInterrupts() noexcept {
        sendIAC(IACMessage::TestPendingInterrupts);
    }
    inline void checkProcessNotice(uint32_t pcbSS) noexcept {
        sendIAC(IACMessage::CheckProcessNotice, 0, 0, pcbSS);
    }
    inline void continueInitialization() noexcept {
        sendIAC(IACMessage::ContinueInitialization);
    }
    inline void flushLocalRegisters(uint32_t physicalStackPageAddress) noexcept {
        sendIAC(IACMessage::FlushLocalRegisters, 0, 0, physicalStackPageAddress);
    }
    inline void flushProcess() noexcept { sendIAC(IACMessage::FlushProcess); }
    inline void flushTLB() noexcept { sendIAC(IACMessage::FlushTLB); }
    inline void flushTLBPageTableEntry(uint32_t offsetFromSegmentBase, uint32_t ssOfSegmentThatContainsPage) noexcept {
        sendIAC(IACMessage::FlushTLBPageTableEntry, 0, 0, offsetFromSegmentBase, ssOfSegmentThatContainsPage);
    }
    inline void flushTLBPhysicalPage(uint32_t basePhysicalAddressOfPage) noexcept {
        sendIAC(IACMessage::FlushTLBPhysicalPage, 0, 0, basePhysicalAddressOfPage);
    }
    inline void flushTLBSegmentEntry(uint32_t ssForSegment) noexcept {
        sendIAC(IACMessage::FlushTLBSegmentEntry, 0, 0, ssForSegment);
    }
    inline void freeze() noexcept { sendIAC(IACMessage::Freeze); }
    inline void modifyProcessorControls(uint32_t newProcessorControls, uint32_t mask) noexcept { sendIAC(IACMessage::ModifyProcessorControls, 0,0, newProcessorControls, mask); }
    inline void preemptProcess() noexcept { sendIAC(IACMessage::PreemptProcess); }
    inline void restartProcessor(uint32_t physicalAddressOfSegmentTable, uint32_t physicalAddressOfPRCB) noexcept {
        sendIAC(IACMessage::RestartProcessor, 0, 0, physicalAddressOfSegmentTable, physicalAddressOfPRCB);
    }
    inline void stopProcessor() noexcept { sendIAC(IACMessage::StopProcessor); }
    inline void storeProcessor() noexcept { sendIAC(IACMessage::StoreProcessor); }
    inline void warmstartProcessor(uint32_t physicalAddressOfSegmentTable, uint32_t physicalAddressOfPRCB) noexcept {
        sendIAC(IACMessage::WarmstartProcessor, 0, 0, physicalAddressOfSegmentTable, physicalAddressOfPRCB);
    }

}
#endif //HITAGIMON_IAC_H
