//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>

namespace cortex
{
    namespace Operations {
#define X(x) ((static_cast<uint32_t>(0xFE) << 24) | ((static_cast<uint32_t>(x) << 4) & 0x00FFFFF0))
        enum {
            Code_Info_GetCPUClockSpeed = 0,
            Code_Info_GetChipsetClockSpeed,

            Code_Serial_RW = 0x10,
            Code_Serial_Flush,

            Code_Timer0 = 0x20,
            Code_Timer1,
            Code_Timer2,
            Code_Timer3,


#define Y(opcode) opcode = X( Code_ ## opcode )
            Y(Info_GetCPUClockSpeed),
            Y(Info_GetChipsetClockSpeed),
            // serial operations begin
            Y(Serial_RW),
            Y(Serial_Flush),
            // timer operations begin
            Y(Timer0),
            Y(Timer1),
            Y(Timer2),
            Y(Timer3),
        };
#undef Y
#undef X
    } // end namespace Operations
    namespace ChipsetBasicFunctions {
        namespace Console {

            uint16_t
            read() {
                return memory<uint16_t>(Operations::Serial_RW);
            }

            void
            write(uint16_t c) {
                memory<uint16_t>(Operations::Serial_RW) = c;
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                memory<uint8_t>(Operations::Serial_Flush) = 0;
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
            volatile Timer16& getTimer0() noexcept { return memory<Timer16>(Operations::Timer0); }
            volatile Timer16& getTimer1() noexcept { return memory<Timer16>(Operations::Timer1); }
            volatile Timer16& getTimer2() noexcept { return memory<Timer16>(Operations::Timer2); }
            volatile Timer16& getTimer3() noexcept { return memory<Timer16>(Operations::Timer3); }
        } // end namespace RTC
        namespace Info {
            uint32_t
            getCPUClockSpeed() noexcept {
                return memory<uint32_t>(Operations::Info_GetCPUClockSpeed);
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return memory<uint32_t>(Operations::Info_GetChipsetClockSpeed);
            }
        }
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex