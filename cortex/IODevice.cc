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
#define makeAddress(func) \
                    ((static_cast<uint32_t>(0xF0) << 24) | \
                            (static_cast<uint32_t>(Devices::Serial) << 16) | \
                            (static_cast<uint32_t>(func) << 8) | \
                            (static_cast<uint32_t>(0)))
                enum {
                    Available = makeAddress(0),
                    Size = makeAddress(1),
                    RW = makeAddress(2),
                    Flush = makeAddress(3),
                    Baud = makeAddress(4),
                };
#undef makeAddress
            }
            bool available() noexcept { return memory<uint32_t>(Opcodes::Available) != 0; }

            uint16_t
            read() {
                return memory<uint16_t>(Opcodes::RW);
            }

            void
            write(uint16_t c) {
                memory<uint16_t>(Opcodes::RW) = c;
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                memory<uint8_t>(Opcodes::Flush) = 0;
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
#define makeAddress(func) \
                    ((static_cast<uint32_t>(0xF0) << 24) | \
                            (static_cast<uint32_t>(Devices::Timer) << 16) | \
                            (static_cast<uint32_t>(func) << 8) | \
                            (static_cast<uint32_t>(0)))
            namespace Opcodes {
                enum {
                    Available = makeAddress(0),
                    Size = makeAddress(1),
                    CompareValue = makeAddress(2),
                    Prescalar = makeAddress(3),
                };
            }
#undef makeAddress
            bool
            available() noexcept {
                return memory<uint32_t>(Opcodes::Available) != 0;
            }
            uint32_t
            unixtime() noexcept {
                return 0;
            }
            void
            setCompareValue(uint16_t value) noexcept {
                memory<uint16_t>(Opcodes::CompareValue) = value;
            }

            uint16_t
            getCompareValue() noexcept {
                return memory<uint16_t>(Opcodes::CompareValue);
            }
            void
            setPrescalar(uint8_t value) noexcept {
                memory<uint8_t>(Opcodes::Prescalar) = value;
            }
            uint8_t
            getPrescalar() noexcept {
                return memory<uint8_t>(Opcodes::Prescalar);
            }
        } // end namespace RTC
        namespace Info {
            namespace Opcodes {
#define makeAddress(func) \
                    ((static_cast<uint32_t>(0xF0) << 24) | \
                            (static_cast<uint32_t>(Devices::Info) << 16) | \
                            (static_cast<uint32_t>(func) << 8) | \
                            (static_cast<uint32_t>(0)))
                enum {
                    Available = makeAddress(0),
                    Size = makeAddress(1),
                    GetCPUClockSpeed = makeAddress(2),
                    GetChipsetClockSpeed = makeAddress(3),
                    GetExternalIAC = makeAddress(4),
                };
#undef makeAddress
            }
            bool
            available() noexcept {
                return memory<uint32_t>(Opcodes::Available) != 0;
            }
            uint32_t
            getCPUClockSpeed() noexcept {
                return memory<uint32_t>(Opcodes::GetCPUClockSpeed);
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return memory<uint32_t>(Opcodes::GetChipsetClockSpeed);
            }
            IACMessage*
            getExternalMessage() noexcept {
                return reinterpret_cast<IACMessage*>(Opcodes::GetExternalIAC);
            }
        }
        namespace Display {
            namespace Operations {
#define makeAddress(func) \
                    ((static_cast<uint32_t>(0xF0) << 24) | \
                            (static_cast<uint32_t>(Devices::Display) << 16) | \
                            (static_cast<uint32_t>(func) << 8) | \
                            (static_cast<uint32_t>(0)))
                enum {
                    Available = makeAddress(0),
                    Size = makeAddress(1),
                    RW = makeAddress(2),
                    Flush = makeAddress(3),
                    DisplayWidthHeight = makeAddress(4),
                    Rotation = makeAddress(5),
                    InvertDisplay = makeAddress(6),
                    ScrollTo = makeAddress(7),
                    SetScrollMargins = makeAddress(8),
                    SetAddressWindow = makeAddress(9),
                    ReadCommand8 = makeAddress(10),
                    CursorX = makeAddress(11),
                    CursorY = makeAddress(12),
                    CursorXY = makeAddress(13),
                    DrawPixel = makeAddress(14),
                    DrawFastVLine = makeAddress(15),
                    DrawFastHLine = makeAddress(16),
                    FillRect = makeAddress(17),
                    FillScreen = makeAddress(18),
                    DrawLine = makeAddress(19),
                    DrawRect = makeAddress(20),
                    DrawCircle = makeAddress(21),
                    FillCircle = makeAddress(22),
                    DrawTriangle = makeAddress(23),
                    FillTriangle = makeAddress(24),
                    DrawRoundRect = makeAddress(25),
                    FillRoundRect = makeAddress(26),
                    SetTextWrap = makeAddress(27),
                    DrawChar_Square = makeAddress(28),
                    DrawChar_Rectangle = makeAddress(29),
                    SetTextSize_Square = makeAddress(30),
                    SetTextSize_Rectangle = makeAddress(31),
                    SetTextColor0 = makeAddress(32),
                    SetTextColor1 = makeAddress(33),
                    // Transaction parts
                    StartWrite = makeAddress(34),
                    WritePixel = makeAddress(35),
                    WriteFillRect = makeAddress(36),
                    WriteFastVLine = makeAddress(37),
                    WriteFastHLine = makeAddress(38),
                    WriteLine = makeAddress(39),
                    EndWrite = makeAddress(40),
                };

            }
#undef makeAddress
            bool available() noexcept { return memory<uint32_t>(Operations::Available) != 0; }
            void flush() noexcept { memory<uint8_t>(Operations::Flush) = 0; }
            void write(uint8_t value) noexcept { memory<uint8_t>(Operations::RW) = value; }
            void drawPixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::DrawPixel) = makeLongOrdinal(x, y, color, 0);
            }
            void startWrite() noexcept {
                memory<uint8_t>(Operations::StartWrite) = 0;
            }
            void
            writePixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::WritePixel) = makeLongOrdinal(x, y, color, 0);
            }
            void
            writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::WriteFillRect, args);
            }
            void
            writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
                memory<uint64_t>(Operations::WriteFastVLine) = makeLongOrdinal(x, y, h, color);
            }
            void
            writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept {
                memory<uint64_t>(Operations::WriteFastHLine) = makeLongOrdinal(x, y, w, color);
            }
            void
            writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::WriteLine, args);
            }
            void endWrite() noexcept {
                memory<uint8_t>(Operations::EndWrite) = 0;
            }
            uint16_t
            color565(uint8_t red, uint8_t green, uint8_t blue) noexcept {
                return (static_cast<uint16_t>(red & 0xF8) << 8) |
                       (static_cast<uint16_t>(green & 0xFC) << 3) | static_cast<uint16_t>(blue >> 3);
            }
            uint32_t getDisplayWidthHeight() noexcept {
                return memory<uint32_t>(Operations::DisplayWidthHeight);
            }
            uint16_t getDisplayWidth() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight()); }
            uint16_t getDisplayHeight() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight() >> 16); }
            uint8_t getRotation() noexcept { return memory<uint8_t>(Operations::Rotation); }
            void setRotation(uint8_t value) noexcept { memory<uint8_t>(Operations::Rotation) = value; }
            void setAddressWindow(uint16_t a, uint16_t b, uint16_t c, uint16_t d) noexcept { memory<uint64_t>(Operations::SetAddressWindow) = makeLongOrdinal(a, b, c, d); }
            void invertDisplay(bool value) noexcept {
                memory<uint8_t>(Operations::InvertDisplay) = value ? 0xFF : 0;
            }
            void setScrollMargins(uint16_t a, uint16_t b) noexcept {
                memory<uint32_t>(Operations::SetScrollMargins) = makeOrdinal(a, b);
            }
            void scrollTo(uint16_t a) noexcept {
                memory<uint16_t>(Operations::ScrollTo) = a;
            }
        } // end namespace Display
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex