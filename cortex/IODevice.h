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
#include <map>
#include "ChipsetInteract.h"
#include "IAC.h"
#include <fileuids.h>
#include <GraphicsCommands.h>
// disable multicharacter constants warnings since we need them for the ascii
// code dispatch
#pragma GCC diagnostic ignored "-Wmultichar"
namespace cortex {
    namespace ChipsetBasicFunctions {
        namespace Console {
            void flush();
            uint16_t read();
            void write(uint16_t value);
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
            ssize_t write(const char *buffer, size_t nbyte);
        } // end namespace Console
        namespace Timer {
            /**
             * @brief Get the unixtime of the system
             * @return the unixtime as an unsigned 32-bit number
             */
            uint32_t unixtime() noexcept;
            /**
             * @brief Return the amount of time elapsed since
             */
            uint32_t secondstime() noexcept;
            uint32_t millis() noexcept;
            uint32_t micros() noexcept;
            float getRTCTemperature() noexcept;
            uint32_t chipsetCycleCount() noexcept;
        }
        namespace SystemCounter {
            bool active() noexcept;
            void enable() noexcept;
            void disable() noexcept;
            uint64_t get() noexcept;
            void set(uint64_t value) noexcept;
        } // end namespace SystemCounter
        namespace Info {
            uint32_t getCPUClockSpeed() noexcept;
            uint32_t getChipsetClockSpeed() noexcept;
            uint64_t getIdleCycles() noexcept;
            uint64_t getTotalCycles() noexcept;
        } // end namespace Info
        namespace Display {
            using DrawCommand = GraphicsOpcode;
            void command(DrawCommand cmd) noexcept;
            void command(DrawCommand cmd, uint16_t arg0) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) noexcept;
            void command(DrawCommand cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) noexcept;
            // Adafruit GFX Commands 
#define ColorArgument uint16_t color
            inline void drawPixel(int16_t x, int16_t y, ColorArgument) noexcept { command(DrawCommand::DrawPixel, x, y, color); }
            inline void drawFastVerticalLine(int16_t x, int16_t y, int16_t h, ColorArgument) noexcept { command(DrawCommand::DrawFastVerticalLine, x, y, h, color); }
            inline void drawFastHorizontalLine(int16_t x, int16_t y, int16_t w, ColorArgument) noexcept { command(DrawCommand::DrawFastHorizontalLine, x, y, w, color); }
            inline void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, ColorArgument) noexcept { command(DrawCommand::FillRectangle, x, y, w, h, color); }
            void fillScreen(ColorArgument) noexcept;
            inline void drawLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, ColorArgument) noexcept { command(DrawCommand::DrawLine, x0, y0, x1, y1, color); }
            inline void drawRect(int16_t x, int16_t y, int16_t w, int16_t h, ColorArgument) noexcept { command(DrawCommand::DrawRectangle, x, y, w, h, color); }
            inline void drawCircle(int16_t x0, int16_t y0, int16_t r, ColorArgument) noexcept { command(DrawCommand::DrawCircle, x0, y0, r, color); }
            inline void fillCircle(int16_t x0, int16_t y0, int16_t r, ColorArgument) noexcept { command(DrawCommand::FillCircle, x0, y0, r, color); }
            inline void drawEllipse(int16_t x0, int16_t y0, int16_t rw, int16_t rh, ColorArgument) noexcept { command(DrawCommand::DrawEllipse, x0, y0, rw, rh, color); }
            inline void fillEllipse(int16_t x0, int16_t y0, int16_t rw, int16_t rh, ColorArgument) noexcept { command(DrawCommand::FillEllipse, x0, y0, rw, rh, color); }
            inline void drawTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, ColorArgument) noexcept { command(DrawCommand::DrawTriangle, x0, y0, x1, y1, x2, y2, color); }
            inline void fillTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, ColorArgument) noexcept { command(DrawCommand::FillTriangle, x0, y0, x1, y1, x2, y2, color); }
            inline void drawRoundRect(int16_t x0, int16_t y0, int16_t w, int16_t h, int16_t radius, ColorArgument) noexcept { command(DrawCommand::DrawRoundedRectangle, x0, y0, w, h, radius, color); }
            inline void fillRoundRect(int16_t x0, int16_t y0, int16_t w, int16_t h, int16_t radius, ColorArgument) noexcept { command(DrawCommand::FillRoundedRectangle, x0, y0, w, h, radius, color); }
            inline void drawRotatedRect(int16_t centerX, int16_t centerY, int16_t w, int16_t h, int16_t angleDec, ColorArgument) noexcept { command(DrawCommand::DrawRotatedRectangle, centerX, centerY, w, h, angleDec, color); }
            inline void fillRotatedRect(int16_t centerX, int16_t centerY, int16_t w, int16_t h, int16_t angleDec, ColorArgument) noexcept { command(DrawCommand::FillRotatedRectangle, centerX, centerY, w, h, angleDec, color); }
            inline void setTextSize(uint8_t sx, uint8_t sy) noexcept { command(DrawCommand::SetTextSize, sx, sy); }
            inline void setTextSize(uint8_t s) noexcept { setTextSize(s, s); }
            inline void drawChar(int16_t x, int16_t y, unsigned char c, uint16_t color, uint16_t bg, uint8_t sizeX, uint8_t sizeY) noexcept { command(DrawCommand::DrawCharacter, x, y, c, color, bg, sizeX, sizeY); }
            inline void drawChar(int16_t x, int16_t y, unsigned char c, uint16_t color, uint16_t bg, uint8_t s) noexcept { command(DrawCommand::DrawCharacter, x, y, c, color, bg, s, s); }
            // transaction API components
            inline void startWrite() noexcept { command(DrawCommand::StartWrite); }
            inline void endWrite() noexcept { command(DrawCommand::EndWrite); }

#undef ColorArgument
            inline constexpr uint16_t computeColor(uint8_t r, uint8_t g, uint8_t b) noexcept { return color565_Adafruit(r, g, b); }
            uint16_t width() noexcept;
            uint16_t height() noexcept;
            uint8_t getRotation() noexcept;
            void setRotation(uint8_t value) noexcept;
            void invertDisplay(bool value = true) noexcept;
            void setCursor(uint16_t x, uint16_t y) noexcept;
            inline void resetCursor() noexcept { setCursor(0, 0); }
            uint16_t getCursorX() noexcept;
            uint16_t getCursorY() noexcept;
            inline void clearScreen() noexcept { fillScreen(0); }
            void print(uint16_t value) noexcept;
            inline void print(char* value, size_t nbyte) noexcept {
                for (size_t i = 0; i < nbyte; ++i) {
                    print(value[i]);
                }
            }
            inline void println() noexcept { print('\n'); }
            inline void println(char* value, size_t nbyte) noexcept {
                print(value, nbyte);
                println();
            }
        }
        namespace Random {
            uint32_t getHardwareRandomNumber() noexcept;
        }
        void begin() noexcept;
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
    struct IOMemoryBlock {
        IOMemoryBlock(uint8_t* address, uint32_t capacity) : _address(address), _capacity(capacity), _mask(capacity - 1) { }
        uint16_t capacity() const noexcept { return _capacity; }
        uint8_t read(uint32_t address) const noexcept { return _address[address & _mask]; }
        void write(uint32_t address, uint8_t value) noexcept { _address[address & _mask] = value; }
        uint8_t* data() const noexcept { return _address; }
        template<typename T> T& as() noexcept { return *(reinterpret_cast<T*>(_address)); }
        template<typename T> const T& as() const noexcept { return *(reinterpret_cast<const T*>(_address)); }
    private:
        uint8_t* _address;
        uint32_t _capacity;
        uint32_t _mask;
    };
    //IOMemoryBlock& EEPROM() noexcept;
    //IOMemoryBlock& SRAM() noexcept;
    //IOMemoryBlock& SRAM2() noexcept;

    namespace SDCard {
        using RequestStructure = FilesystemOperation*; /// @todo fix this to not be so generic
        FilesystemInterfaceErrorCodes postRequest(RequestStructure request);
    } // end namespace SDCard

}
#endif //I960SXCHIPSET_IODEVICE_H
