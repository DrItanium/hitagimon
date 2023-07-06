//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_FAULTS_H
#define HITAGIMON_FAULTS_H
#include "SysExamine.h"
namespace cortex
{
    struct FaultData
    {
        volatile unsigned reserved;
        volatile unsigned override[3];
        volatile unsigned fdata[3];
        volatile unsigned override_data;
        volatile FaultRecord record;
        void display();
    } __attribute__((packed));

    typedef void (*FaultHandler)(FaultData *data);
    namespace UserFaultKind {
        enum UserFaultKind {
#define X(name) name ,
#include "cortex/Faults.def"
#undef X
        };
    }
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
