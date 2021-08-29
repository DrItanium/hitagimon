//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_FAULTS_H
#define HITAGIMON_FAULTS_H
namespace cortex
{
    struct FaultData
    {
        volatile unsigned reserved;
        volatile unsigned override[3];
        volatile unsigned fdata[3];
        volatile unsigned override_data;
        volatile unsigned pc;
        volatile unsigned ac;
        volatile unsigned int fsubtype: 8,
                freserved: 8,
                ftype: 8,
                fflags: 8;
        volatile unsigned int *faddress;
        void display();
    } __attribute__((packed));

    typedef void (*FaultHandler)(FaultData *data);
}
#endif //HITAGIMON_FAULTS_H
