//
// Created by jwscoggins on 8/28/21.
//

#ifndef HITAGIMON_SYSEXAMINE_H
#define HITAGIMON_SYSEXAMINE_H
#include <stdint.h>
namespace cortex {
    /**
     * @brief i960 specific arithmetic controls
     */
    union ArithmeticControls {
        uint32_t raw;
        struct {
            uint32_t cc : 3;
#ifdef __i960SB__
            uint32_t arithmeticStatus : 4;
            uint32_t unused0 : 1;
#else
            uint32_t unused0 : 5;
#endif
        };
    };
}
#endif //HITAGIMON_SYSEXAMINE_H
