//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>

namespace cortex
{
    struct IOSpace {
        union {
            uint8_t bytes[256];
            struct {
                uint32_t cpuClockSpeed;
                uint32_t chipsetClockSpeed;
                uint32_t SerialRW;
                uint32_t SerialFlush;
                //
                uint32_t ms;
                uint32_t us;
                uint32_t _eepromCapacity;
                uint32_t _sramCapacity;
                // rtc functions
                uint32_t _unixtime;
                uint32_t _secondstime;
                float _rtcTemperature;
                uint32_t _configure32k;
            };
        } builtin;
        uint8_t unmappedPages[7][256];
        uint8_t sramCache[2048];
        uint8_t eeprom[4096];
        inline uint16_t read() volatile noexcept { return builtin.SerialRW; }
        inline void write(uint16_t c) volatile noexcept { builtin.SerialRW = c; }
        inline void flush() volatile noexcept { builtin.SerialFlush = 0; }
        inline uint32_t millis() volatile noexcept { return builtin.ms; }
        inline uint32_t micros() volatile noexcept { return builtin.us; }
        inline uint16_t eepromCapacity() const volatile noexcept { return builtin._eepromCapacity; }
        inline uint16_t sramCacheCapacity() volatile noexcept { return builtin._sramCapacity; }
        inline uint32_t unixtime() volatile noexcept { return builtin._unixtime; }
        inline uint32_t secondstime() volatile noexcept { return builtin._secondstime; }
        inline float rtc_getTemperature() volatile noexcept { return builtin._rtcTemperature; }
        inline bool rtc_32kEnabled() volatile noexcept { return builtin._configure32k; }
        inline void rtc_enable32k() volatile noexcept { builtin._configure32k = 1; }
        inline void rtc_disable32k() volatile noexcept { builtin._configure32k = 0; }
    } __attribute__((packed));
    volatile IOSpace& getIOSpace() noexcept {
        return memory<IOSpace>(0xFE000000);
    }
    namespace ChipsetBasicFunctions {
        namespace Console {

            uint16_t
            read() {
                return getIOSpace().read();
            }

            void
            write(uint16_t c) {
                getIOSpace().write(c);
            }

            void
            write(char c) {
                getIOSpace().write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                getIOSpace().flush();
            }
            void
            write(const char *ptr) {
                write(const_cast<char *>(ptr), strlen(ptr));
            }

            void
            writeLine() {
                write('\n');
            }
            void
            writeLine(const char *ptr) {
                write(ptr);
                writeLine();
            }

            ssize_t
            write(char *buffer, size_t nbyte) {
                // unlike reading, we must be sequential in writing
                ssize_t numWritten = 0;
                for (size_t i = 0; i < nbyte; ++i) {
                    write(buffer[i]);
                    ++numWritten;
                }
                flush();
                return numWritten;
            }
            uint16_t
            waitForLegalCharacter() {
                uint16_t rawConsoleValue = read();
                while (rawConsoleValue == 0xFFFF) {
                    rawConsoleValue = read();
                }
                return rawConsoleValue;
            }
            ssize_t
            read(char *buffer, size_t nbyte) {
                ssize_t numRead = 0;
                for (size_t i = 0; i < nbyte; ++i) {
                    buffer[i] = static_cast<char>(waitForLegalCharacter());
                    ++numRead;
                    if ((buffer[i] == '\n') || (buffer[i] == '\r')) {
                        return numRead;
                    }
                }
                return numRead;
            }
        } // end namespace Console
        namespace Timer {
            uint32_t secondstime() noexcept { return getIOSpace().secondstime(); }
            uint32_t unixtime() noexcept { return getIOSpace().unixtime(); }
            uint32_t millis() noexcept { return getIOSpace().millis(); }
            uint32_t micros() noexcept { return getIOSpace().micros(); }
            float getRTCTemperature() noexcept { return getIOSpace().rtc_getTemperature(); }
        } // end namespace RTC
        namespace Info {
            uint32_t
            getCPUClockSpeed() noexcept {
                return getIOSpace().builtin.cpuClockSpeed;
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return getIOSpace().builtin.chipsetClockSpeed;
            }

        }
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
    IOMemoryBlock&
    EEPROM() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getIOSpace().eeprom), getIOSpace().eepromCapacity());
        return thing;
    }
    IOMemoryBlock&
    SRAM() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getIOSpace().sramCache), getIOSpace().sramCacheCapacity());
        return thing;
    }

} // end namespace cortex
