//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_SYSEXAMINE_H
#define HITAGIMON_SYSEXAMINE_H
#include <stdint.h>
namespace cortex {
    /**
     * @brief i960 specific arithmetic controls
     */
    union ArithmeticControls {
        uint32_t raw;
        struct {
            uint32_t cc : 3;
#ifdef __i960SB__
            uint32_t arithmeticStatus : 4;
            uint32_t unused0 : 1;
#else
            uint32_t unused0 : 5;
#endif
            uint32_t integerOverflowFlag : 1;
            uint32_t unused1 : 1;
            uint32_t integerOverflowMask : 1;
            uint32_t unused2 : 2;
            uint32_t noImpreciseFaults : 1;
#ifdef __i960SB__
            uint32_t floatingOverflowFlag : 1;
            uint32_t floatingUnderflowFlag : 1;
            uint32_t floatingInvalidOpFlag : 1;
            uint32_t floatingZeroDivideFlag : 1;
            uint32_t floatingInexactFlag : 1;
            uint32_t unused3 : 3;
            uint32_t floatingOverflowMask : 1;
            uint32_t floatingUnderflowMask : 1;
            uint32_t floatingInvalidOpMask : 1;
            uint32_t floatingZeroDivideMask : 1;
            uint32_t floatingInexactMask : 1;
            uint32_t floatingPointNormalizingMode : 1;
            uint32_t floatingPointRoundingControl : 2;
#else
            uint32_t unused3 : 16;
#endif
        };
    };
    union ProcessControls
    {
        uint32_t raw;
        struct {
            uint32_t traceEnable: 1;
            uint32_t executionMode: 1;
            uint32_t unused0: 7;
            uint32_t resume: 1;
            uint32_t traceFaultPending: 1;
            uint32_t unused1: 2;
            uint32_t state: 1;
            uint32_t unused2: 2;
            uint32_t priority: 5;
            uint32_t internalState: 10;
        };
    };
    union TraceControls {
        uint32_t raw;
        struct {
            uint32_t unused0 : 1;
            uint32_t instructionTraceMode : 1;
            uint32_t branchTraceMode : 1;
            uint32_t callTraceMode : 1;
            uint32_t returnTraceMode : 1;
            uint32_t prereturnTraceMode : 1;
            uint32_t supervisorTraceMode : 1;
            uint32_t breakpointTraceMode : 1;
            uint32_t unused1 : 9;
            uint32_t instructionTraceEvent : 1;
            uint32_t branchTraceEvent : 1;
            uint32_t callTraceEvent : 1;
            uint32_t returnTraceEvent : 1;
            uint32_t prereturnTraceEvent : 1;
            uint32_t supervisorTraceEvent : 1;
            uint32_t breakpointTraceEvent : 1;
            uint32_t unused2 : 8;
        };
    };
    struct FaultRecord {
        uint32_t pc;
        uint32_t ac;
        uint32_t type;
        uint32_t* addr;
    };
    struct FaultTableEntry {
        void (*handler)();
        int32_t magicNumber;
    };
    struct FaultTable {
        FaultTableEntry entries[32];
    };
    struct InterruptTable {
        uint32_t pendingPriorities;
        uint32_t pendingInterrupts[8];
        void (*interruptProcedures[248])();
    };
    struct InterruptRecord {
        uint32_t pc;
        uint32_t ac;
        uint8_t vectorNumber;
    };
    struct SysProcTable {
        uint32_t reserved[3];
        void* supervisorStack;
        uint32_t preserved[8];
        void (*entries[260])();
    };
    struct PRCB {
        uint32_t reserved0;
        uint32_t magicNumber;
        uint32_t reserved1[3];
        InterruptTable* theInterruptTable;
        void* interruptStack;
        uint32_t reserved2;
        uint32_t magicNumber1;
        uint32_t magicNumber2;
        FaultTable* theFaultTable;
        uint32_t reserved3[32];
    };
    struct SystemAddressTable {
        SystemAddressTable* satPtr;
        PRCB* thePRCB;
        uint32_t checkWord;
        void (*firstInstruction)();
        uint32_t checkWords[4];
        uint32_t preserved0[22];
        SysProcTable* theProcTable0;
        uint32_t magicNumber0;
        uint32_t preserved1[2];
        SystemAddressTable* offset0;
        uint32_t magicNumber1;
        uint32_t preserved2[2];
        SysProcTable* theProcTable1;
        uint32_t magicNumber2;
        uint32_t preserved3[2];
        SysProcTable* traceTable;
        uint32_t magicNumber3;
    };
#define GetProcessControls(pc) asm volatile ("modpc 0, 0, %0" : "=r" (pc.raw))
#define GetArithmeticControls(ac) asm volatile ("modac 0, 0, %0" : "=r" (ac.raw))
#define GetTraceControls(tc) asm volatile ("modtc 0, 0, %0" : "=r" (tc.raw))
}
#endif //HITAGIMON_SYSEXAMINE_H
