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
#undef X

    template<UserFaultKind::UserFaultKind kind>
    inline FaultHandler getUserFaultHandler() {
        switch (kind) {
            case UserFaultKind::Reserved: return getUserReservedFaultHandler();
            case UserFaultKind::Trace: return getUserTraceFaultHandler();
            case UserFaultKind::Operation: return getUserOperationFaultHandler();
            case UserFaultKind::Arithmetic: return getUserArithmeticFaultHandler();
            case UserFaultKind::FloatingPoint: return getUserFloatingPointFaultHandler();
            case UserFaultKind::Constraint: return getUserConstraintFaultHandler();
            case UserFaultKind::Protection: return getUserProtectionFaultHandler();
            case UserFaultKind::Machine: return getUserMachineFaultHandler();
            case UserFaultKind::Type: return getUserTypeFaultHandler();
            default: return nullptr;
        }
    }



    template<UserFaultKind::UserFaultKind kind>
    inline void setUserFaultHandler(FaultHandler handler) {
        switch (kind) {
            case UserFaultKind::Reserved: setUserReservedFaultHandler(handler); break;
            case UserFaultKind::Trace: setUserTraceFaultHandler(handler); break;
            case UserFaultKind::Operation: setUserOperationFaultHandler(handler); break;
            case UserFaultKind::Arithmetic: setUserArithmeticFaultHandler(handler); break;
            case UserFaultKind::FloatingPoint: setUserFloatingPointFaultHandler(handler); break;
            case UserFaultKind::Constraint: setUserConstraintFaultHandler(handler); break;
            case UserFaultKind::Protection: setUserProtectionFaultHandler(handler); break;
            case UserFaultKind::Machine: setUserMachineFaultHandler(handler); break;
            case UserFaultKind::Type: setUserTypeFaultHandler(handler); break;
            default: break;
        }
    }
}
#endif //HITAGIMON_FAULTS_H
