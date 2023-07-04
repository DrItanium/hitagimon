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
            Reserved,
            Trace,
            Operation,
            Arithmetic,
            FloatingPoint,
            Constraint,
            VirtualMemory,
            Protection,
            Machine,
            Structural,
            Type,
            Reserved1,
            Process,
            Descriptor,
            Event,

            // aliases
            RealArithmetic = FloatingPoint,
        };
    }
#define X(kind) \
FaultHandler getUser ## kind ## FaultHandler (); \
                void setUser ## kind ## FaultHandler (FaultHandler)
    X(Reserved);
    X(Trace);
    X(Operation);
    X(Arithmetic);
    X(FloatingPoint);
    X(Constraint);
    X(Protection);
    X(VirtualMemory);
    X(Machine);
    X(Type);
    X(Process);
    X(Descriptor);
    X(Event);
    X(Structural);
#undef X

    template<UserFaultKind::UserFaultKind kind>
    inline FaultHandler getUserFaultHandler() {
        switch (kind) {
#define X(name) case UserFaultKind:: name : return getUser ## name ## FaultHandler ()
            X(Reserved);
            X(Trace);
            X(Operation);
            X(Arithmetic);
            X(FloatingPoint);
            X(Constraint);
            X(VirtualMemory);
            X(Protection);
            X(Machine);
            X(Structural);
            X(Type);
            X(Process);
            X(Descriptor);
            X(Event);
#undef X
            default: return nullptr;
        }
    }



    template<UserFaultKind::UserFaultKind kind>
    inline void setUserFaultHandler(FaultHandler handler) {
        switch (kind) {
#define X(name) case UserFaultKind:: name : setUser ## name ## FaultHandler (handler)
            X(Reserved);
            X(Trace);
            X(Operation);
            X(Arithmetic);
            X(FloatingPoint);
            X(Constraint);
            X(VirtualMemory);
            X(Protection);
            X(Machine);
            X(Structural);
            X(Type);
            X(Process);
            X(Descriptor);
            X(Event);
#undef X
            default: break;
        }
    }
}
#endif //HITAGIMON_FAULTS_H
