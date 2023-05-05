/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
//
// Created by jwscoggins on 5/3/21.
//

#ifndef I960SXCHIPSET_IODEVICE_H
#define I960SXCHIPSET_IODEVICE_H
#include <stdint.h>
#include <unistd.h>
#include <string>
#include "ChipsetInteract.h"
#include "IAC.h"
namespace cortex
{
    struct Port8 {
        uint8_t input;
        uint8_t direction;
        uint8_t output;
    } __attribute__((packed));

    struct EEPROMInterface {
        union {
           uint8_t reg;
           struct {
               uint8_t re : 1;
               uint8_t pe : 1;
               uint8_t mpe : 1;
               uint8_t rie : 1;
               uint8_t pm : 2;
               uint8_t unused : 2;
           } bits;
        } config;
        uint8_t dataRegister;
        uint16_t addressRegister : 12;
    } __attribute__((packed));
/**
 * @brief Exposure of a chipset timer to the i960, since the chipset is an AVR timer this is a modelling of an AVR 16 bit timer
 */
    struct Timer16 {
    public:
        enum WavefrontGenerationMode {
            Normal = 0,
            PhaseCorrectPWM_8bit,
            PhaseCorrectPWM_9bit,
            PhaseCorrectPWM_10bit,
            CTC_OCRA,
            FastPWM_8bit,
            FastPWM_9bit,
            FastPWM_10bit,
            PhaseAndFrequencyCorrectPWM_ICR,
            PhaseAndFrequencyCorrectPWM_OCRA,
            PhaseCorrectPWM_ICR,
            PhaseCorrectPWM_OCRA,
            CTC_ICR,
            Reserved,
            FastPWM_ICR,
            FastPWM_OCRA,
        };
        enum CompareOutputMode {
            CompareOutputMode_NormalPortOperation = 0,
            CompareOutputMode_ToggleOutputComparePinOnMatch,
            CompareOutputMode_ClearOutputComparePinOnMatch,
            CompareOutputMode_SetOutputComparePinOnMatch,
        };
        void begin() volatile noexcept;
        inline void setCounter(uint16_t value) volatile noexcept { counter_ = value; }
        inline uint16_t getCounter() const volatile noexcept { return counter_; }
        inline void setInputCapture(uint16_t value) volatile noexcept { inputCapture_ = value; }
        inline uint16_t getInputCapture() const volatile noexcept { return inputCapture_ ; }
        inline void setOutputCompareA(uint16_t value) volatile noexcept { outputCompareA_ = value; }
        inline uint16_t getOutputCompareA() const volatile noexcept { return outputCompareA_ ; }
        inline void setOutputCompareB(uint16_t value) volatile noexcept { outputCompareB_ = value; }
        inline uint16_t getOutputCompareB() const volatile noexcept { return outputCompareB_ ; }
        inline void setOutputCompareC(uint16_t value) volatile noexcept { outputCompareC_ = value; }
        inline uint16_t getOutputCompareC() const volatile noexcept { return outputCompareC_ ; }
        inline void setWaveformGenerationMode(WavefrontGenerationMode mode) volatile noexcept {
            ctl_.bits.wgm0 = (mode & 0x1) != 0;
            ctl_.bits.wgm1 = (mode & 0x2) != 0;
            ctl_.bits.wgm2 = (mode & 0x4) != 0;
            ctl_.bits.wgm3 = (mode & 0x8) != 0;
        }
        inline WavefrontGenerationMode getWaveformGenerationMode() const volatile noexcept {
            uint8_t result = 0;
            if (ctl_.bits.wgm0) { result |= (1 << 0); }
            if (ctl_.bits.wgm1) { result |= (1 << 1); }
            if (ctl_.bits.wgm2) { result |= (1 << 2); }
            if (ctl_.bits.wgm3) { result |= (1 << 3); }
            return static_cast<WavefrontGenerationMode>(result);
        }
        inline bool inputCaptureNoiseCancelerEnabled() const volatile noexcept { return ctl_.bits.icnc != 0; }
        inline bool inputCaptureEdgeSelectOnFallingEdge() const volatile noexcept { return ctl_.bits.ices == 0; }
        inline bool inputCaptureEdgeSelectOnRisingEdge() const volatile noexcept { return ctl_.bits.ices != 0; }
        inline void setInputCaptureEdgeSelect(bool onRisingEdge) volatile noexcept { ctl_.bits.ices = onRisingEdge; }
        inline void setInputCaptureNoiseCanceler(bool enabled) volatile noexcept { ctl_.bits.icnc = enabled; }
        inline void forceOutputCompareOnChannelA(bool enabled) volatile noexcept { ctl_.bits.foc_a = enabled; }
        inline void forceOutputCompareOnChannelB(bool enabled) volatile noexcept { ctl_.bits.foc_b = enabled; }
        inline void forceOutputCompareOnChannelC(bool enabled) volatile noexcept { ctl_.bits.foc_c = enabled; }
        inline void setConfiguration(uint32_t value) volatile noexcept { ctl_.whole = value; }
        inline uint32_t getConfiguration() const volatile noexcept { return ctl_.whole; }
        inline void setClockPrescaler(uint8_t value) volatile noexcept {
            ctl_.bits.cs = value;
        }
        inline uint8_t getClockPrescaler() const volatile noexcept { return ctl_.bits.cs; }
        inline void stop() volatile noexcept { setClockPrescaler(0); }
        inline void start(uint8_t prescalar) volatile noexcept {
            setClockPrescaler(prescalar);
        }
        inline void setCompareOutputModeA(CompareOutputMode mode) volatile noexcept {
            ctl_.bits.coma0 = mode & 0x1;
            ctl_.bits.coma1 = (mode & 0x2) != 0;
        }
        inline void setCompareOutputModeB(CompareOutputMode mode) volatile noexcept {
            ctl_.bits.comb0 = mode & 0x1;
            ctl_.bits.comb1 = (mode & 0x2) != 0;
        }
        inline void setCompareOutputModeC(CompareOutputMode mode) volatile noexcept {
            ctl_.bits.comc0 = mode & 0x1;
            ctl_.bits.comc1 = (mode & 0x2) != 0;
        }
    private:
        volatile union {
            volatile uint32_t whole;
            struct {
                volatile uint8_t wgm0 : 1;
                volatile uint8_t wgm1 : 1;
                volatile uint8_t comc0 : 1;
                volatile uint8_t comc1 : 1;
                volatile uint8_t comb0 : 1;
                volatile uint8_t comb1 : 1;
                volatile uint8_t coma0 : 1;
                volatile uint8_t coma1 : 1;
                volatile uint8_t cs : 3;
                volatile uint8_t wgm2 : 1;
                volatile uint8_t wgm3 : 1;
                volatile uint8_t unused0 : 1;
                volatile uint8_t ices : 1;
                volatile uint8_t icnc : 1;
                volatile uint8_t unused1 : 5;
                volatile uint8_t foc_c : 1;
                volatile uint8_t foc_b : 1;
                volatile uint8_t foc_a : 1;
                volatile uint8_t unused2 : 8;
            } bits;
        } ctl_;
        volatile uint16_t counter_;
        volatile uint16_t inputCapture_;
        volatile uint16_t outputCompareA_;
        volatile uint16_t outputCompareB_;
        volatile uint16_t outputCompareC_;
        volatile uint16_t unused_;
    } __attribute__((packed));
    namespace ChipsetBasicFunctions {

        namespace Console {
            void flush();
            uint16_t read();
            void write(uint16_t value);
            void write(char c);
            void write(const char *ptr);
            void writeLine();
            void writeLine(const char *ptr);
            /**
             * @brief sequential read from the console into the provided buffer
             * @param buffer the buffer to save to
             * @param nbyte the maximum number of bytes to read
             * @return number of bytes read
             */
            ssize_t read(char *buffer, size_t nbyte);
            /**
             * @brief Sequential write to the console into the provided buffer
             * @param buffer the buffer to write into
             * @param nbyte the maximum number of bytes to write
             * @return the number of bytes written
             */
            ssize_t write(char *buffer, size_t nbyte);
        } // end namespace Console
        namespace Timer {
            volatile Timer16& getTimer0() noexcept;
            volatile Timer16& getTimer1() noexcept;
            volatile Timer16& getTimer2() noexcept;
            volatile Timer16& getTimer3() noexcept;
            /**
             * @brief Get the unixtime of the system
             * @return the unixtime as an unsigned 32-bit number
             */
            uint32_t unixtime() noexcept;


        }
        namespace Info {
            uint32_t getCPUClockSpeed() noexcept;
            uint32_t getChipsetClockSpeed() noexcept;
        } // end namespace Info
        void begin() noexcept;
    } // end namespace ChipsetBasicFunctions
    inline uint32_t
    makeOrdinal(uint16_t lower, uint16_t upper) {
        return static_cast<uint32_t>(lower) | (static_cast<uint32_t>(upper) << 16);
    }
    inline uint64_t
    makeLongOrdinal(uint32_t lower, uint32_t upper) {
        return static_cast<uint64_t>(lower) | (static_cast<uint64_t>(upper) << 32);
    }
    inline uint64_t
    makeLongOrdinal(uint16_t a, uint16_t b, uint16_t c, uint16_t d) {
        return makeLongOrdinal(makeOrdinal(a, b),
                               makeOrdinal(c, d));
    }
}
#endif //I960SXCHIPSET_IODEVICE_H
