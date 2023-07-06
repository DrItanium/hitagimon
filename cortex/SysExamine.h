//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_SYSEXAMINE_H
#define HITAGIMON_SYSEXAMINE_H
#include <stdint.h>
#include <stdio.h>
#include "ModernCpp.h"
#include "builtins.h"
namespace cortex {
    /**
     * @brief i960 specific arithmetic controls
     */
    union ArithmeticControls {
        uint32_t raw;
        struct {
            uint32_t cc : 3;
#ifdef __HAS_NUMERICS_ARCHITECTURE_EXTENSIONS__
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
#ifdef __HAS_NUMERICS_ARCHITECTURE_EXTENSIONS__
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
    } __attribute((packed));
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
    } __attribute((packed));
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
    } __attribute((packed));
    struct FaultTableEntry {
        typedef void(*FaultOperation)();
        uint32_t handlerRaw;
        int32_t magicNumber;
        inline FaultOperation getFaultFunction() const noexcept { return reinterpret_cast<FaultOperation>(handlerRaw & 0xFFFFFFFC); }
        inline uint8_t getProcedureKind() const noexcept { return static_cast<uint8_t>(handlerRaw & 0x3); }
        inline bool isLocalProcedure() const noexcept { return getProcedureKind() == 0; }
        inline bool isSystemProcedure() const noexcept { return getProcedureKind() == 0x2 && getMagicNumber() == 0x0000027F; }
        inline bool isTraceFaultHandler() const noexcept { return getProcedureKind() == 0x2 && getMagicNumber() == 0x000002BF; }
        inline int32_t getMagicNumber() const noexcept { return magicNumber; }
    } __attribute((packed));
    struct FaultTable {
        FaultTableEntry entries[32];
    } __attribute((packed));
    struct InterruptTable {
        uint32_t pendingPriorities;
        uint32_t pendingInterrupts[8];
        void (*interruptProcedures[248])();
    } __attribute((packed));
    struct InterruptRecord {
        uint32_t pc;
        uint32_t ac;
        uint8_t vectorNumber;
    } __attribute((packed));
    struct SATEntry {
        typedef void (*Executable)();
        enum Kind {
            LocalProcedure,
            Reserved0,
            SupervisorProcedure,
            Reserved1,
            Invalid = Reserved1,
        };
        uint32_t raw;
        inline Executable asFunction() const noexcept {
            // drop the lowest tag bits to make sure that it is the right location
            return reinterpret_cast<Executable>(raw & 0xFFFFFFFC);
        }
        inline Kind getKind() const noexcept {
            return static_cast<Kind>(raw & 0x3);
        }
    } __attribute((packed));
    struct SysProcTable {
        uint32_t reserved[3];
        void* supervisorStack;
        uint32_t preserved[8];
        SATEntry entries[260];
        typedef SATEntry::Executable FunctionBody;

        inline FunctionBody getEntry(uint32_t index) noexcept {
            if (index > 259) {
                return nullptr;
            } else {
                return entries[index].asFunction();
            }
        }
        inline SATEntry::Kind getEntryKind(uint32_t index) noexcept {
            if (index > 259) {
                return SATEntry::Invalid;
            } else {
                return entries[index].getKind();
            }
        }
    } __attribute((packed));
    struct PRCB {
        uint32_t reserved0;
        union {
            uint32_t raw;
            struct {
                uint32_t reserved0 : 1;
                uint32_t multiProcessorPreempt : 1;
                uint32_t state : 2;
                uint32_t reserved1 : 1;
                uint32_t nonPreemptLimit : 5;
                uint32_t addressingMode : 1;
                uint32_t checkDispatchPort : 1;
                uint32_t reserved2 : 4;
                uint32_t interimPriority : 5;
                uint32_t reserved3 : 10;
                uint32_t writeExternalPriority : 1;
            };
        } processorState;
        uint32_t reserved1;
        uint32_t currentProcess;
        uint32_t dispatchPort;
        InterruptTable* theInterruptTable;
        void* interruptStack;
        uint32_t reserved2;
        uint32_t region3SegmentSelector;
        uint32_t systemProcedureTablePointer;
        FaultTable* theFaultTable;
        uint32_t reserved3;
        uint32_t reserved4[3];
        uint64_t idleTime;
        uint32_t systemErrorFault;
        uint32_t reserved5;
        uint32_t resumptionRecord[12];
        uint32_t systemErrorFaultRecord[11];
        bool virtualMemoryEnabled() const noexcept { return processorState.addressingMode != 0; }
    } __attribute((packed));
    struct SystemAddressTable {
        /// @todo populate once I figure out how to represent it
    } __attribute((packed));
    struct BootWords {
        SystemAddressTable* sat;
        PRCB* thePRCB;
        uint32_t checkWord;
        void (*firstInstruction)();
        uint32_t checkWords[4];
    } __attribute((packed));
    inline volatile const BootWords& getBootWords() noexcept {
        return *reinterpret_cast<volatile const BootWords* >(0);
    }
    struct ProcessControlBlock {
        uint64_t queueRecord;
        uint32_t receiveMessage;
        uint32_t dispatchPortSS;
        uint32_t residualTimeSlice;
        uint32_t processControls;
        uint8_t lock;
        uint32_t processNotice : 24;
        uint32_t traceControls;
        uint32_t reserved0[4];
        uint32_t region0SS;
        uint32_t region1SS;
        uint32_t region2SS;
        uint32_t arithmeticControls;
        uint32_t reserved1;
        uint32_t nextTimeSlice;
        uint64_t executionTime;
        uint8_t resumptionRecord[44];
        uint8_t floatingPointRegisters[48]; // 12 bytes * 4
        uint32_t globalRegisters[16];
    } __attribute((packed));
}
#endif //HITAGIMON_SYSEXAMINE_H
