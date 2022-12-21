//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_INTERRUPTS_H
#define HITAGIMON_INTERRUPTS_H
#include "ModernCpp.h"
#include <stdint.h>
#ifdef __cplusplus
extern "C" {
#endif
typedef void (*InterruptFunction)(void);
InterruptFunction getNMIFunction();
InterruptFunction getISR0Function();
void setNMIFunction(InterruptFunction func);
void setISR0Function(InterruptFunction func);
typedef void (*InterruptRoutine)(void);
typedef struct __attribute__((packed)) InterruptTable {
    uint32_t pendingPriorities;
    uint32_t pendingInterrupts[8];
    union {
        uint32_t vectors[248];
        InterruptRoutine routines[248];
    };
} InterruptTable_t ;


#ifdef __cplusplus
}
#endif
#endif //HITAGIMON_INTERRUPTS_H
