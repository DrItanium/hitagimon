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
#define X(name, index, locase, hicase) name = index,
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
        void display(ProcedureStackFrame& previous);
        inline UserFaultKind::UserFaultKind getFaultKind() const noexcept {
                switch (faultInfo.type) {
#define X(kind, index, locase, hicase) case index : return UserFaultKind :: kind ;
#include "cortex/Faults.def"
#undef X
                    default:
                        return UserFaultKind::UserFaultKind(0xFF);
                }
        }
    } __attribute__((packed));
    using FaultHandler = void(*)(FaultData& data, ProcedureStackFrame& pfp);
#define X(kind, index, locase, hicase) \
FaultHandler getUser ## kind ## FaultHandler (); \
                void setUser ## kind ## FaultHandler (FaultHandler);
#include "cortex/Faults.def"
#undef X

    template<UserFaultKind::UserFaultKind kind>
    inline FaultHandler getUserFaultHandler() {
        switch (kind) {
#define X(name, index, locase, hicase) case UserFaultKind:: name : return getUser ## name ## FaultHandler ();
#include "cortex/Faults.def"
#undef X
            default: return nullptr;
        }
    }



    template<UserFaultKind::UserFaultKind kind>
    inline void setUserFaultHandler(FaultHandler handler) {
        switch (kind) {
#define X(name, index, locase, hicase) case UserFaultKind:: name : setUser ## name ## FaultHandler (handler);
#include "cortex/Faults.def"
#undef X
            default: break;
        }
    }
}
#endif //HITAGIMON_FAULTS_H
