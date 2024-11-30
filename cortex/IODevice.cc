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
            uint8_t data[16][256];
            struct {
                uint32_t cpuClockSpeed;
                uint32_t chipsetClockSpeed;
                uint32_t SerialRW;
                uint32_t SerialFlush;
                uint32_t ms;
                uint32_t us;
                uint32_t _eepromBaseAddress;
                uint32_t _sramCacheBaseAddress;
                uint16_t _eepromCapacity;
                uint16_t _sramCapacity;
            };
        };
        inline uint16_t read() volatile noexcept { return SerialRW; }
        inline void write(uint16_t c) volatile noexcept { SerialRW = c; }
        inline void flush() volatile noexcept { SerialFlush = 0; }
        inline uint32_t millis() volatile noexcept { return ms; }
        inline uint32_t micros() volatile noexcept { return us; }
        inline uint16_t eepromCapacity() volatile noexcept { return _eepromCapacity; }
        inline uint16_t sramCacheCapacity() volatile noexcept { return _sramCapacity; }
        inline volatile uint8_t* eepromStorage() volatile noexcept { return reinterpret_cast<volatile uint8_t*>(_eepromBaseAddress); }
        inline volatile uint8_t* sramStorage() volatile noexcept { return reinterpret_cast<volatile uint8_t*>(_sramCacheBaseAddress); }
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
            uint32_t
            unixtime() noexcept {
                return 0;
            }
            uint32_t
            millis() noexcept {
                return getIOSpace().millis();
            }
            uint32_t
            micros() noexcept {
                return getIOSpace().micros();
            }
        } // end namespace RTC
        namespace Info {
            uint32_t
            getCPUClockSpeed() noexcept {
                return getIOSpace().cpuClockSpeed;
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return getIOSpace().chipsetClockSpeed;
            }

        }
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
    void
    Timer16::begin() volatile noexcept {
        ctl_.whole = 0;
        counter_ = 0;
        inputCapture_ = 0;
        outputCompareA_ = 0;
        outputCompareB_ = 0;
        outputCompareC_ = 0;
        unused_ = 0;
    }
} // end namespace cortex