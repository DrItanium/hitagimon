//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_FAULTS_H
#define HITAGIMON_FAULTS_H
#include "SysExamine.h"
namespace cortex
{
    namespace UserFaultKind {
        enum UserFaultKind {
#define X(name) name ,
#include "cortex/Faults.def"
#undef X
        };
    }
    struct FaultData
    {
        union FaultRecordInfo {
            uint32_t full;
            struct {
                uint8_t subtype;
                uint8_t reserved;
                uint8_t type;
                uint8_t flags;
            };
        };
        uint32_t reserved;
        uint32_t override[3];
        uint32_t data[3];
        FaultRecordInfo overrideInfo;
        ProcessControls pc;
        ArithmeticControls ac;
        FaultRecordInfo faultInfo;
        uint32_t* addressOfFaultingInstruction;
        void display();
        inline UserFaultKind::UserFaultKind getFaultKind() const noexcept {
                switch (faultInfo.type) {
                    case 0: return UserFaultKind::Override;
                    case 1: return UserFaultKind::Trace;
                    case 2: return UserFaultKind::Operation;
                    case 3: return UserFaultKind::Arithmetic;
                    case 4: return UserFaultKind::FloatingPoint;
                    case 5: return UserFaultKind::Constraint;
                    case 6: return UserFaultKind::VirtualMemory;
                    case 7: return UserFaultKind::Protection;
                    case 8: return UserFaultKind::Machine;
                    case 9: return UserFaultKind::Structural;
                    case 0xa: return UserFaultKind::Type;
                    case 0xc: return UserFaultKind::Process;
                    case 0xd: return UserFaultKind::Descriptor;
                    case 0xe: return UserFaultKind::Event;
                    default:
                        return UserFaultKind::UserFaultKind(0xFF);
                }
        }
    } __attribute__((packed));

    typedef void (*FaultHandler)(FaultData *data);
#define X(kind) \
FaultHandler getUser ## kind ## FaultHandler (); \
                void setUser ## kind ## FaultHandler (FaultHandler);
#include "cortex/Faults.def"
#undef X

    template<UserFaultKind::UserFaultKind kind>
    inline FaultHandler getUserFaultHandler() {
        switch (kind) {
#define X(name) case UserFaultKind:: name : return getUser ## name ## FaultHandler ();
#include "cortex/Faults.def"
#undef X
            default: return nullptr;
        }
    }



    template<UserFaultKind::UserFaultKind kind>
    inline void setUserFaultHandler(FaultHandler handler) {
        switch (kind) {
#define X(name) case UserFaultKind:: name : setUser ## name ## FaultHandler (handler);
#include "cortex/Faults.def"
#undef X
            default: break;
        }
    }
}
#endif //HITAGIMON_FAULTS_H
