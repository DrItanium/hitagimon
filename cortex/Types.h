//
// Created by jwscoggins on 12/23/22.
//

#ifndef HITAGIMON_TYPES_H
#define HITAGIMON_TYPES_H
#include <stdint.h>
namespace cortex {
    union __attribute((packed)) SplitWord16 {
        uint16_t base;
        uint8_t bytes[sizeof(uint16_t)/sizeof(uint8_t)];
    };
    union __attribute((packed)) SplitWord32 {
        uint32_t base;
        uint16_t halves[sizeof(uint32_t)/sizeof(uint16_t)];
        uint8_t bytes[sizeof(uint32_t)/sizeof(uint8_t)];
        SplitWord16 word16s[sizeof(uint32_t) / sizeof(SplitWord16)];
    };
    union __attribute((packed)) SplitWord64 {
        uint64_t base;
        uint32_t halves[sizeof(uint64_t)/sizeof(uint32_t)];
        uint16_t quarters[sizeof(uint64_t)/sizeof(uint16_t)];
        uint8_t bytes[sizeof(uint64_t)/sizeof(uint8_t)];
        SplitWord16 word16s[sizeof(uint64_t) / sizeof(SplitWord16)];
        SplitWord32 word32s[sizeof(uint64_t) / sizeof(SplitWord32)];
    };
}

#endif //HITAGIMON_TYPES_H
