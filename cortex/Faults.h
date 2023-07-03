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

    FaultHandler getUserReservedFaultHandler();
    FaultHandler getUserTraceFaultHandler();
    FaultHandler getUserOperationFaultHandler();
    FaultHandler getUserArithmeticFaultHandler();
    FaultHandler getUserRealArithmeticFaultHandler();
    FaultHandler getUserConstraintFaultHandler();
    FaultHandler getUserProtectionFaultHandler();
    FaultHandler getUserMachineFaultHandler();
    FaultHandler getUserTypeFaultHandler();
    template<UserFaultKind::UserFaultKind kind>
    inline FaultHandler getUserFaultHandler() {
        switch (kind) {
            case UserFaultKind::Reserved: return getUserReservedFaultHandler();
            case UserFaultKind::Trace: return getUserTraceFaultHandler();
            case UserFaultKind::Operation: return getUserOperationFaultHandler();
            case UserFaultKind::Arithmetic: return getUserArithmeticFaultHandler();
            case UserFaultKind::RealArithmetic: return getUserRealArithmeticFaultHandler();
            case UserFaultKind::Constraint: return getUserConstraintFaultHandler();
            case UserFaultKind::Protection: return getUserProtectionFaultHandler();
            case UserFaultKind::Machine: return getUserMachineFaultHandler();
            case UserFaultKind::Type: return getUserTypeFaultHandler();
            default: return nullptr;
        }
    }

    void setUserReservedFaultHandler(FaultHandler);
    void setUserTraceFaultHandler(FaultHandler);
    void setUserOperationFaultHandler(FaultHandler);
    void setUserArithmeticFaultHandler(FaultHandler);
    void setUserRealArithmeticFaultHandler(FaultHandler);
    void setUserConstraintFaultHandler(FaultHandler);
    void setUserProtectionFaultHandler(FaultHandler);
    void setUserMachineFaultHandler(FaultHandler);
    void setUserTypeFaultHandler(FaultHandler);

    template<UserFaultKind::UserFaultKind kind>
    inline void setUserFaultHandler(FaultHandler handler) {
        switch (kind) {
            case UserFaultKind::Reserved: setUserReservedFaultHandler(handler); break;
            case UserFaultKind::Trace: setUserTraceFaultHandler(handler); break;
            case UserFaultKind::Operation: setUserOperationFaultHandler(handler); break;
            case UserFaultKind::Arithmetic: setUserArithmeticFaultHandler(handler); break;
            case UserFaultKind::RealArithmetic: setUserRealArithmeticFaultHandler(handler); break;
            case UserFaultKind::Constraint: setUserConstraintFaultHandler(handler); break;
            case UserFaultKind::Protection: setUserProtectionFaultHandler(handler); break;
            case UserFaultKind::Machine: setUserMachineFaultHandler(handler); break;
            case UserFaultKind::Type: setUserTypeFaultHandler(handler); break;
            default: break;
        }
    }
}
#endif //HITAGIMON_FAULTS_H
