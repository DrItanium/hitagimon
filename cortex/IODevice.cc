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
            Code_Info_GetExternalIAC,

            Code_Serial_RW = 0x10,
            Code_Serial_Flush,
            Code_Serial_Baud,

            Code_Timer_CompareValue = 0x20,
            Code_Timer_Prescalar,
            Code_Timer_Unixtime,

            // display opcodes begin
            Code_Display_RW = 0x30,
            Code_Display_Flush,
            Code_Display_WidthHeight,
            Code_Display_Rotation,
            Code_Display_InvertDisplay,
            Code_Display_ScrollTo,
            Code_Display_SetScrollMargins,
            Code_Display_SetAddressWindow,
            Code_Display_ReadCommand8,
            Code_Display_CursorX,
            Code_Display_CursorY,
            Code_Display_CursorXY,
            Code_Display_DrawPixel,
            Code_Display_DrawFastVLine,
            Code_Display_DrawFastHLine,
            Code_Display_FillRect,
            Code_Display_FillScreen,
            Code_Display_DrawLine,
            Code_Display_DrawRect,
            Code_Display_DrawCircle,
            Code_Display_FillCircle,
            Code_Display_DrawTriangle,
            Code_Display_FillTriangle,
            Code_Display_DrawRoundRect,
            Code_Display_FillRoundRect,
            Code_Display_SetTextWrap,
            Code_Display_DrawChar_Square,
            Code_Display_DrawChar_Rectangle,
            Code_Display_SetTextSize_Square,
            Code_Display_SetTextSize_Rectangle,
            Code_Display_SetTextColor0,
            Code_Display_SetTextColor1,
            Code_Display_StartWrite,
            Code_Display_WritePixel,
            Code_Display_WriteFillRect,
            Code_Display_WriteFastVLine,
            Code_Display_WriteFastHLine,
            Code_Display_WriteLine,
            Code_Display_EndWrite,

#define Y(opcode) opcode = X( Code_ ## opcode )
            Y(Info_GetCPUClockSpeed),
            Y(Info_GetChipsetClockSpeed),
            Y(Info_GetExternalIAC),
            // serial operations begin
            Y(Serial_RW),
            Y(Serial_Flush),
            Y(Serial_Baud),
            // timer operations begin
            Y(Timer_CompareValue),
            Y(Timer_Prescalar),
            Y(Timer_Unixtime),
            // Display Operations begin
            Y(Display_RW),
            Y(Display_Flush),
            Y(Display_WidthHeight),
            Y(Display_Rotation),
            Y(Display_InvertDisplay),
            Y(Display_ScrollTo),
            Y(Display_SetScrollMargins),
            Y(Display_SetAddressWindow),
            Y(Display_ReadCommand8),
            Y(Display_CursorX),
            Y(Display_CursorY),
            Y(Display_CursorXY),
            Y(Display_DrawPixel),
            Y(Display_DrawFastVLine),
            Y(Display_DrawFastHLine),
            Y(Display_FillRect),
            Y(Display_FillScreen),
            Y(Display_DrawLine),
            Y(Display_DrawRect),
            Y(Display_DrawCircle),
            Y(Display_FillCircle),
            Y(Display_DrawTriangle),
            Y(Display_FillTriangle),
            Y(Display_DrawRoundRect),
            Y(Display_FillRoundRect),
            Y(Display_SetTextWrap),
            Y(Display_DrawChar_Square),
            Y(Display_DrawChar_Rectangle),
            Y(Display_SetTextSize_Square),
            Y(Display_SetTextSize_Rectangle),
            Y(Display_SetTextColor0),
            Y(Display_SetTextColor1),
            Y(Display_StartWrite),
            Y(Display_WritePixel),
            Y(Display_WriteFillRect),
            Y(Display_WriteFastVLine),
            Y(Display_WriteFastHLine),
            Y(Display_WriteLine),
            Y(Display_EndWrite),


        };
#undef Y
#undef X
    } // end namespace Operations
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
            void
            setCompareValue(uint16_t value) noexcept {
                memory<uint16_t>(Operations::Timer_CompareValue) = value;
            }

            uint16_t
            getCompareValue() noexcept {
                return memory<uint16_t>(Operations::Timer_CompareValue);
            }
            void
            setPrescalar(uint8_t value) noexcept {
                memory<uint8_t>(Operations::Timer_Prescalar) = value;
            }
            uint8_t
            getPrescalar() noexcept {
                return memory<uint8_t>(Operations::Timer_Prescalar);
            }
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
            IACMessage*
            getExternalMessage() noexcept {
                return reinterpret_cast<IACMessage*>(Operations::Info_GetExternalIAC);
            }
        }
        namespace Display {
            void flush() noexcept { memory<uint8_t>(Operations::Display_Flush) = 0; }
            void write(uint8_t value) noexcept { memory<uint8_t>(Operations::Display_RW) = value; }
            void drawPixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_DrawPixel) = makeLongOrdinal(x, y, color, 0);
            }
            void startWrite() noexcept {
                memory<uint8_t>(Operations::Display_StartWrite) = 0;
            }
            void
            writePixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WritePixel) = makeLongOrdinal(x, y, color, 0);
            }
            void
            writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::Display_WriteFillRect, args);
            }
            void
            writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WriteFastVLine) = makeLongOrdinal(x, y, h, color);
            }
            void
            writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WriteFastHLine) = makeLongOrdinal(x, y, w, color);
            }
            void
            writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::Display_WriteLine, args);
            }
            void endWrite() noexcept {
                memory<uint8_t>(Operations::Display_EndWrite) = 0;
            }
            uint16_t
            color565(uint8_t red, uint8_t green, uint8_t blue) noexcept {
                return (static_cast<uint16_t>(red & 0xF8) << 8) |
                       (static_cast<uint16_t>(green & 0xFC) << 3) | static_cast<uint16_t>(blue >> 3);
            }
            uint32_t getDisplayWidthHeight() noexcept {
                return memory<uint32_t>(Operations::Display_WidthHeight);
            }
            uint16_t getDisplayWidth() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight()); }
            uint16_t getDisplayHeight() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight() >> 16); }
            uint8_t getRotation() noexcept { return memory<uint8_t>(Operations::Display_Rotation); }
            void setRotation(uint8_t value) noexcept { memory<uint8_t>(Operations::Display_Rotation) = value; }
            void setAddressWindow(uint16_t a, uint16_t b, uint16_t c, uint16_t d) noexcept { memory<uint64_t>(Operations::Display_SetAddressWindow) = makeLongOrdinal(a, b, c, d); }
            void invertDisplay(bool value) noexcept {
                memory<uint8_t>(Operations::Display_InvertDisplay) = value ? 0xFF : 0;
            }
            void setScrollMargins(uint16_t a, uint16_t b) noexcept {
                memory<uint32_t>(Operations::Display_SetScrollMargins) = makeOrdinal(a, b);
            }
            void scrollTo(uint16_t a) noexcept {
                memory<uint16_t>(Operations::Display_ScrollTo) = a;
            }
        } // end namespace Display
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex