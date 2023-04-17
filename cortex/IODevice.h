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

            void end();
            void begin(unsigned long baud, uint8_t config);
        } // end namespace Console
        namespace Timer {
            bool available() noexcept;
            /**
             * @brief Get the unixtime of the system
             * @return the unixtime as an unsigned 32-bit number
             */
            uint32_t unixtime() noexcept;

            void setCompareValue(uint16_t value) noexcept;
            uint8_t getCompareValue() noexcept;
            void setPrescalar(uint8_t value) noexcept;
            uint8_t getPrescalar() noexcept;

        }
        namespace Info {
            bool available() noexcept;
            IACMessage* getExternalMessage() noexcept;
            uint32_t getCPUClockSpeed() noexcept;
            uint32_t getChipsetClockSpeed() noexcept;
        } // end namespace Info
        namespace Display {
            bool available() noexcept;
#if 0
            void flush();
            uint64_t getDisplayWidthHeight() noexcept;
            uint32_t getDisplayWidth() noexcept;
            uint32_t getDisplayHeight() noexcept;
            uint8_t getRotation() noexcept;
            void setRotation(uint8_t value) noexcept;
            void invertDisplay(bool value) noexcept;
            void setScrollMargins(uint16_t a, uint16_t b) noexcept;
            void setAddressWindow(uint16_t a, uint16_t b, uint16_t c, uint16_t d) noexcept;
            void scrollTo(uint16_t a) noexcept;
            void write(uint8_t value) noexcept;
#endif
            void drawPixel(int16_t x, int16_t y, uint16_t color) noexcept;
            void startWrite() noexcept;
            void writePixel(int16_t x, int16_t y, uint16_t color) noexcept;
            void writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept;
            void writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept;
            void writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept;
            void writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept;
            void endWrite() noexcept;
            uint16_t color565(uint8_t r, uint8_t g, uint8_t b) noexcept;
        }
#if 0

    /// @todo add drawBitmap support

        [[nodiscard]] bool isAvailable() const noexcept { return true; }
        [[gnu::always_inline]] inline void handleWriteOperations(const SplitWord128& body, uint8_t function, uint8_t offset) noexcept {
            using K = ConnectedOpcode_t<TargetPeripheral::Display>;
            switch (getFunctionCode<TargetPeripheral::Display>(function)) {
                case K::SetScrollMargins: tft_.setScrollMargins(body[0].halves[0], body[0].halves[1]); break;
                case K::SetAddressWindow: tft_.setAddrWindow(body[0].halves[0], body[0].halves[1], body[1].halves[0], body[1].halves[1]); break;
                case K::ScrollTo: tft_.scrollTo(body[0].halves[0]); break;
                case K::InvertDisplay: tft_.invertDisplay(body.bytes[0] != 0); break;
                case K::Rotation: tft_.setRotation(body.bytes[0]); break;
                case K::RW: tft_.print(static_cast<uint8_t>(body.bytes[0])); break;
                case K::Flush: tft_.flush(); break;
                case K::DrawPixel:
                    tft_.drawPixel(body[0].halves[0], body[0].halves[1], body[1].halves[0]);
                    break;
                case K::DrawFastHLine:
                    tft_.drawFastHLine(body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::DrawFastVLine:
                    tft_.drawFastVLine(body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::FillRect:
                    tft_.fillRect( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0]);
                    break;

                case K::FillScreen:
                    tft_.fillScreen(body[0].halves[0]);
                    break;
                case K::DrawLine:
                    tft_.drawLine( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0]);
                    break;
                case K::DrawRect:
                    tft_.drawRect( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0]);
                    break;
                case K::DrawCircle:
                    tft_.drawCircle(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::FillCircle:
                    tft_.fillCircle(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::DrawTriangle:
                    tft_.drawTriangle( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1],
                            body[3].halves[0]);
                    break;
                case K::FillTriangle:
                    tft_.fillTriangle( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1],
                            body[3].halves[0]);
                    break;
                case K::DrawRoundRect:
                    tft_.drawRoundRect( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1]);
                    break;
                case K::FillRoundRect:
                    tft_.fillRoundRect( body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1]);
                    break;
                case K::SetTextWrap:
                    tft_.setTextWrap(body.bytes[0]);
                    break;
                case K::CursorX:
                    tft_.setCursor(body[0].halves[0], tft_.getCursorY());
                    break;
                case K::CursorY:
                    tft_.setCursor(tft_.getCursorX(), body[0].halves[0]);
                    break;
                case K::CursorXY:
                    tft_.setCursor(body[0].halves[0], body[0].halves[1]);
                    break;
                case K::DrawChar_Square:
                    tft_.drawChar(body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1]);
                    break;
                case K::DrawChar_Rectangle:
                    tft_.drawChar(body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0],
                            body[2].halves[1],
                            body[3].halves[0]);
                    break;
                case K::SetTextSize_Square:
                    tft_.setTextSize(body[0].halves[0]);
                    break;
                case K::SetTextSize_Rectangle:
                    tft_.setTextSize(body[0].halves[0],
                            body[0].halves[1]);
                    break;
                case K::SetTextColor0:
                    tft_.setTextColor(body[0].halves[0]);
                    break;
                case K::SetTextColor1:
                    tft_.setTextColor(body[0].halves[0], body[0].halves[1]);
                    break;
                case K::StartWrite:
                    tft_.startWrite();
                    break;
                case K::WritePixel:
                    tft_.writePixel(body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0]);
                    break;
                case K::WriteFillRect:
                    tft_.writeFillRect(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0]);
                    break;
                case K::WriteFastVLine:
                    tft_.writeFastVLine(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::WriteFastHLine:
                    tft_.writeFastHLine(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1]);
                    break;
                case K::WriteLine:
                    tft_.writeLine(
                            body[0].halves[0],
                            body[0].halves[1],
                            body[1].halves[0],
                            body[1].halves[1],
                            body[2].halves[0]);
                    break;
                case K::EndWrite:
                    tft_.endWrite();
                    break;
                default:
                    break;
            }

        }
#endif
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
