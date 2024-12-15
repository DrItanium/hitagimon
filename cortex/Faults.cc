//
// Created by jwscoggins on 8/28/21.
//

#include "Faults.h"
#include "ChipsetInteract.h"
namespace cortex
{
    void
    FaultData::display(ProcedureStackFrame* fp) {
        uint8_t ftype = faultInfo.type;
        uint8_t fsubtype = faultInfo.subtype;
        printf("FAULT INTERCEPTED!\n\n");
        printf("Fault Type: %x\n", ftype);
        switch (getFaultKind()) {
#define X(kind, index, locase, hicase) case UserFaultKind:: kind : printf ("\t" #kind " Fault\n" ); break;
#include "cortex/Faults.def"
#undef X
            default:
                break;
        }
        printf("Fault Subtype: %x\n", fsubtype);
        if (getFaultKind() == UserFaultKind::Operation) {
            // load and display the operation in question
            if (fsubtype == 1) {
                printf("\tInvalid Opcode\n");
            } else if (fsubtype == 4) {
                printf("\tInvalid Operand\n");
            }
            volatile uint32_t* ptr = addressOfFaultingInstruction;
            unsigned int container[8] = { 0 };
            for (int i = 0;i < 8; ++i, ++ptr) {
                container[i] = *ptr;
            }
            printf("\t\tContents: \n");
            for (int i = 0; i < 8; ++i) {
               printf("\t\t\t0x%x\n", container[i]);
            }
        }
        printf("Faulting Address: %p\n", addressOfFaultingInstruction);
        printf("Process Controls: %#lx\n", pc.raw);
        printf("Arithmetic Controls: %#lx\n", ac.raw);
        printf("Frame Pointer Chain:\n");
        auto frameMatchesFaultingInstruction = [this](ProcedureStackFrame* fp) { return fp->rip == reinterpret_cast<uint32_t>(addressOfFaultingInstruction); };
        auto printFP = [frameMatchesFaultingInstruction](ProcedureStackFrame* fp, int index) {
            printf("\tFrame %d @ %p\n", index, fp);
            printf("\t\tPFP: %#lx\n", fp->pfp);
            printf("\t\tSP: %#lx\n", fp->sp);
            printf("\t\tRIP: %#lx\n", fp->rip);
            if (frameMatchesFaultingInstruction(fp)) {
                printf("\t\tIs Failing Frame!\n");
            }
        };
        ProcedureStackFrame* failingFrame = fp;
        int i = 0;
        // walk back up the chain until we get to the frame which will return to the faulting frame (this allows us to ignore any number of frames we generate as part of the fault handling process
        for (; !frameMatchesFaultingInstruction(failingFrame); failingFrame = failingFrame->getParentFrame(), ++i) {
            printFP(failingFrame, i);
        }
        printFP(failingFrame, i);
        // go one more up to be able to actually figure out where we came from
        failingFrame = failingFrame->getParentFrame();
        ++i;
        printFP(failingFrame, i);
        // now that we have this address, we need to figure out where the pfp of the pfp is located
    }
#define X(kind, index, locase, hicase) \
namespace { FaultHandler user ## kind ## _ ; } \
FaultHandler getUser ## kind ## FaultHandler () { return user ## kind ## _ ; } \
void setUser ## kind ## FaultHandler (FaultHandler handler) { user ## kind ## _ = handler; }
#include "cortex/Faults.def"
#undef X
}