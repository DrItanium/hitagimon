//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"
namespace cortex
{
    namespace ChipsetBasicFunctions {
        namespace Opcodes {
            enum {
                Available = 0,
                Size,
                RW,
                Flush,
                Baud,
            };
        }
        namespace Console {
            uint16_t
            read() {
                Opcode code(0, Devices::Serial, Opcodes::RW, 0);
                return code.read16();
            }

            void
            write(uint16_t c) {
                Opcode code(0, Devices::Serial, Opcodes::RW, 0);
                code.write16(c);
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                Opcode code(0, Devices::Serial, Opcodes::Flush, 0);
                // doesn't matter what you write as long as you write it
                code.write16(0);
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
            // this is a pre c++11 "hack" to support referencing enums by name
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    CompareValue,
                    Prescalar,
                };
            }
            bool
            available() noexcept {
                Opcode code(0, Devices::Timer, Opcodes::Available, 0);
                return code.read16();
            }
            void
            setCompareValue(uint8_t value) noexcept {
                Opcode code(0, Devices::Timer, Opcodes::CompareValue, 0);
                code.write16(value);
            }
            uint8_t
            getCompareValue() noexcept {
                Opcode code(0, Devices::Timer, Opcodes::CompareValue, 0);
                return code.read16();
            }
            void
            setPrescalar(uint8_t value) noexcept {
                Opcode code(0, Devices::Timer, Opcodes::Prescalar, 0);
                code.write16(value);
            }
            uint8_t getPrescalar() noexcept {
                Opcode code(0, Devices::Timer, Opcodes::Prescalar, 0);
                return code.read16();
            }
        } // end namespace RTC
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex