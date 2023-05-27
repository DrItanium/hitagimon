//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>

namespace cortex
{
    struct IOSpace {
        uint32_t cpuClockSpeed;
        uint32_t chipsetClockSpeed;
        uint32_t SerialRW;
        uint32_t SerialFlush;
        Timer16 timer0;
        Timer16 timer1;
        Timer16 timer2;
        Timer16 timer3;
    } __attribute__((packed));
    volatile IOSpace& getIOSpace() noexcept {
        return memory<IOSpace>(0xFE000000);
    }
    namespace ChipsetBasicFunctions {
        namespace Console {

            uint16_t
            read() {
                return getIOSpace().SerialRW;
            }

            void
            write(uint16_t c) {
                getIOSpace().SerialRW = c;
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                getIOSpace().SerialFlush = 0;
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
            volatile Timer16& getTimer0() noexcept { return getIOSpace().timer0; }
            volatile Timer16& getTimer1() noexcept { return getIOSpace().timer1; }
            volatile Timer16& getTimer2() noexcept { return getIOSpace().timer2; }
            volatile Timer16& getTimer3() noexcept { return getIOSpace().timer3; }
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