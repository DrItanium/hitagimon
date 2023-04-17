//
// Created by jwscoggins on 12/23/22.
//

#ifndef HITAGIMON_TYPES_H
#define HITAGIMON_TYPES_H
#include <stdint.h>
#include <cortex/ModernCpp.h>
namespace cortex {
    union __attribute((packed)) SplitWord16 {
        uint16_t base;
        int16_t sbase;
        uint8_t bytes[2];
        int8_t sbytes[2];
        constexpr uint16_t getBase() const noexcept { return base; }
        constexpr int16_t getSignedBase() const noexcept { return sbase; }
    };
    static_assert(sizeof(SplitWord16) == sizeof(uint16_t), "SplitWord16 must be 2 bytes in size");
    union __attribute((packed)) SplitWord32 {
        uint32_t base;
        int32_t sbase;
        uint16_t halves[2];
        uint8_t bytes[4];
        int8_t sbytes[4];
        SplitWord16 word16s[2];
        constexpr uint32_t getBase() const noexcept { return base; }
        constexpr int32_t getSignedBase() const noexcept { return sbase; }
    };
    static_assert(sizeof(SplitWord32) == sizeof(uint32_t), "SplitWord32 must be 4 bytes in size");
    union __attribute((packed)) SplitWord64 {
        uint64_t base;
        int64_t sbase;
        uint32_t halves[2];
        uint16_t quarters[4];
        uint8_t bytes[8];
        int8_t sbytes[8];
        SplitWord16 word16s[4];
        SplitWord32 word32s[2];
        constexpr uint64_t getBase() const noexcept { return base; }
        constexpr int64_t getSignedBase() const noexcept { return sbase; }
    };
    static_assert(sizeof(SplitWord64) == sizeof(uint64_t), "SplitWord64 must be 8 bytes in size");
}

#endif //HITAGIMON_TYPES_H
