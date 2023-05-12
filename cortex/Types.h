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
        struct {
            uint32_t p0 : 1;
            uint32_t p1 : 1;
            uint32_t p2 : 1;
            uint32_t p3 : 1;
            uint32_t p4 : 1;
            uint32_t p5 : 1;
            uint32_t p6 : 1;
            uint32_t p7 : 1;
            uint32_t p8 : 1;
            uint32_t p9 : 1;
            uint32_t p10 : 1;
            uint32_t p11 : 1;
            uint32_t p12 : 1;
            uint32_t p13 : 1;
            uint32_t p14 : 1;
            uint32_t p15 : 1;
        } bits;
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
        struct {
            uint32_t p0 : 1;
            uint32_t p1 : 1;
            uint32_t p2 : 1;
            uint32_t p3 : 1;
            uint32_t p4 : 1;
            uint32_t p5 : 1;
            uint32_t p6 : 1;
            uint32_t p7 : 1;
            uint32_t p8 : 1;
            uint32_t p9 : 1;
            uint32_t p10 : 1;
            uint32_t p11 : 1;
            uint32_t p12 : 1;
            uint32_t p13 : 1;
            uint32_t p14 : 1;
            uint32_t p15 : 1;
            uint32_t p16 : 1;
            uint32_t p17 : 1;
            uint32_t p18 : 1;
            uint32_t p19 : 1;
            uint32_t p20 : 1;
            uint32_t p21 : 1;
            uint32_t p22 : 1;
            uint32_t p23 : 1;
            uint32_t p24 : 1;
            uint32_t p25 : 1;
            uint32_t p26 : 1;
            uint32_t p27 : 1;
            uint32_t p28 : 1;
            uint32_t p29 : 1;
            uint32_t p30 : 1;
            uint32_t p31 : 1;
        } bits;
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
        struct {
            uint32_t p0 : 1;
            uint32_t p1 : 1;
            uint32_t p2 : 1;
            uint32_t p3 : 1;
            uint32_t p4 : 1;
            uint32_t p5 : 1;
            uint32_t p6 : 1;
            uint32_t p7 : 1;
            uint32_t p8 : 1;
            uint32_t p9 : 1;
            uint32_t p10 : 1;
            uint32_t p11 : 1;
            uint32_t p12 : 1;
            uint32_t p13 : 1;
            uint32_t p14 : 1;
            uint32_t p15 : 1;
            uint32_t p16 : 1;
            uint32_t p17 : 1;
            uint32_t p18 : 1;
            uint32_t p19 : 1;
            uint32_t p20 : 1;
            uint32_t p21 : 1;
            uint32_t p22 : 1;
            uint32_t p23 : 1;
            uint32_t p24 : 1;
            uint32_t p25 : 1;
            uint32_t p26 : 1;
            uint32_t p27 : 1;
            uint32_t p28 : 1;
            uint32_t p29 : 1;
            uint32_t p30 : 1;
            uint32_t p31 : 1;
            uint32_t p32 : 1;
            uint32_t p33 : 1;
            uint32_t p34 : 1;
            uint32_t p35 : 1;
            uint32_t p36 : 1;
            uint32_t p37 : 1;
            uint32_t p38 : 1;
            uint32_t p39 : 1;
            uint32_t p40 : 1;
            uint32_t p41 : 1;
            uint32_t p42 : 1;
            uint32_t p43 : 1;
            uint32_t p44 : 1;
            uint32_t p45 : 1;
            uint32_t p46 : 1;
            uint32_t p47 : 1;
            uint32_t p48 : 1;
            uint32_t p49 : 1;
            uint32_t p50 : 1;
            uint32_t p51 : 1;
            uint32_t p52 : 1;
            uint32_t p53 : 1;
            uint32_t p54 : 1;
            uint32_t p55 : 1;
            uint32_t p56 : 1;
            uint32_t p57 : 1;
            uint32_t p58 : 1;
            uint32_t p59 : 1;
            uint32_t p60 : 1;
            uint32_t p61 : 1;
            uint32_t p62 : 1;
            uint32_t p63 : 1;
        } bits;
        constexpr uint64_t getBase() const noexcept { return base; }
        constexpr int64_t getSignedBase() const noexcept { return sbase; }
    };
    static_assert(sizeof(SplitWord64) == sizeof(uint64_t), "SplitWord64 must be 8 bytes in size");
}

#endif //HITAGIMON_TYPES_H
