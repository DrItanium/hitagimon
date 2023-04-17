//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>
namespace cortex
{
    namespace ChipsetBasicFunctions {
        namespace Devices {
            enum {
                Info = 0,
                Serial,
                Timer,
                Display,
            };
        }
        namespace Console {
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    RW,
                    Flush,
                    Baud,
                };
            }
            uint16_t
            read() {
                static Opcode code(0, Devices::Serial, Opcodes::RW, 0);
                return code.read16();
            }

            void
            write(uint16_t c) {
                static Opcode code(0, Devices::Serial, Opcodes::RW, 0);
                code.write16(c);
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                static Opcode code(0, Devices::Serial, Opcodes::Flush, 0);
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
                static Opcode code(0, Devices::Timer, Opcodes::Available, 0);
                return code.read16();
            }
            uint32_t
            unixtime() noexcept {
                return 0;
            }
            void
            setCompareValue(uint16_t value) noexcept {
                static Opcode code(0, Devices::Timer, Opcodes::CompareValue, 0);
                code.write16(value);
            }
            uint8_t
            getCompareValue() noexcept {
                static Opcode code(0, Devices::Timer, Opcodes::CompareValue, 0);
                return code.read16();
            }
            void
            setPrescalar(uint8_t value) noexcept {
                static Opcode code(0, Devices::Timer, Opcodes::Prescalar, 0);
                code.write8(value);
            }
            uint8_t getPrescalar() noexcept {
                static Opcode code(0, Devices::Timer, Opcodes::Prescalar, 0);
                return code.read8();
            }
        } // end namespace RTC
        namespace Info {
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    GetCPUClockSpeed,
                    GetChipsetClockSpeed,
                    GetExternalIAC,
                };
            }
            uint32_t
            getCPUClockSpeed() noexcept {
                static Opcode code(0, Devices::Info, Opcodes::GetCPUClockSpeed, 0);
                return code.read32();
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                static Opcode code(0, Devices::Info, Opcodes::GetCPUClockSpeed, 0);
                return code.read32();
            }
            bool
            available() noexcept {
                static Opcode code(0, Devices::Info, Opcodes::Available, 0);
                return code.read16();
            }
            IACMessage*
            getExternalMessage() noexcept {
                static Opcode code(0, Devices::Info, Opcodes::GetExternalIAC, 0);
                return reinterpret_cast<IACMessage*>(code.makeFullAddress());
            }
        }
        namespace Display {
            namespace Operations {
                enum {
                    Available,
                    Size,
                    RW, // for the "serial" aspect so we can print out to the screen
                    Flush,
                    DisplayWidthHeight,
                    Rotation,
                    InvertDisplay,
                    ScrollTo,
                    SetScrollMargins,
                    SetAddressWindow,
                    ReadCommand8,
                    CursorX, CursorY, CursorXY,
                    DrawPixel,
                    DrawFastVLine, DrawFastHLine,
                    FillRect, FillScreen,
                    DrawLine, DrawRect,
                    DrawCircle, FillCircle,
                    DrawTriangle, FillTriangle,
                    DrawRoundRect, FillRoundRect,
                    SetTextWrap,
                    DrawChar_Square, DrawChar_Rectangle,
                    SetTextSize_Square, SetTextSize_Rectangle,
                    SetTextColor0, SetTextColor1,
                    // Transaction parts
                    StartWrite, WritePixel, WriteFillRect, WriteFastVLine, WriteFastHLine, WriteLine, EndWrite,
                };

            }
            void
            drawPixel(int16_t x, int16_t y, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::DrawPixel, 0);
                static SplitWord64 args;
                args.quarters[0] = x;
                args.quarters[1] = y;
                args.quarters[2] = color;
                args.quarters[3] = 0;
                op.write64(args.getBase());
            }
            void startWrite() noexcept {
                static Opcode op(0, Devices::Display, Operations::StartWrite, 0);
                op.write8(0);
            }
            void
            writePixel(int16_t x, int16_t y, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::WritePixel, 0);
                static SplitWord64 args;
                args.quarters[0] = x;
                args.quarters[1] = y;
                args.quarters[2] = color;
                args.quarters[3] = 0;
                op.write64(args.getBase());
            }
            void
            writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::WriteFillRect, 0);
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = color;
                __builtin_i960_synmovq(reinterpret_cast<void*>(op.makeFullAddress()), args);
            }
            void
            writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::WriteFastVLine, 0);
                static SplitWord64 args;
                args.quarters[0] = x;
                args.quarters[1] = y;
                args.quarters[2] = h;
                args.quarters[3] = color;
                __builtin_i960_synmovl(reinterpret_cast<void*>(op.makeFullAddress()), &args);
            }
            void
            writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::WriteFastHLine, 0);
                static SplitWord64 args;
                args.quarters[0] = x;
                args.quarters[1] = y;
                args.quarters[2] = w;
                args.quarters[3] = color;
                __builtin_i960_synmovl(reinterpret_cast<void*>(op.makeFullAddress()), &args);
            }
            void
            writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
                static Opcode op(0, Devices::Display, Operations::WriteFillRect, 0);
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = color;
                __builtin_i960_synmovq(reinterpret_cast<void*>(op.makeFullAddress()), args);
            }
            void endWrite() noexcept {
                static Opcode op(0, Devices::Display, Operations::EndWrite, 0);
                op.write8(0);
            }
            uint16_t
            color565(uint8_t red, uint8_t green, uint8_t blue) noexcept {
                return (static_cast<uint16_t>(red & 0xF8) << 8) |
                       (static_cast<uint16_t>(green & 0xFC) << 3) | static_cast<uint16_t>(blue >> 3);
            }
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex