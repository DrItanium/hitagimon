//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"
namespace cortex
{
    namespace ChipsetBasicFunctions {
        namespace Console {
            uint16_t
            read() {
                Opcode code(0, 1, 2, 0);
                return code.read16();
            }

            void
            write(uint16_t c) {
                Opcode code(0, 1, 2, 0);
                code.write16(c);
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                Opcode code(0, 1, 3, 0);
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
        namespace RTC {
            bool
            available() noexcept {
                Opcode code(0, 2, 0, 0);
                return code.read16();
            }
            uint32_t
            unixtime() noexcept {
                Opcode code(0, 2, 2, 0);
                return code.read32();
            }
        } // end namespace RTC
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex