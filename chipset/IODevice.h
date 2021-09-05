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

class BuiltinIOBaseDevice {
public:
    BuiltinIOBaseDevice(uint32_t offset);
    uint32_t getBaseAddress() const { return baseAddress_; }
    uint32_t getOffset() const { return offset_; }
protected:
    uint32_t offset_;
    uint32_t baseAddress_;
};
/**
 * @brief Manages the builtin led provided by the chipset
 */
class ChipsetBasicFunctions : public BuiltinIOBaseDevice {
public:
    ChipsetBasicFunctions(uint32_t offset = 0);
    void flush();
    uint16_t read() const;
    void write(uint16_t value);
    void write(char c);
    void write(const char* ptr);
    void writeLine();
    void writeLine(const char* ptr);
    /**
     * @brief sequential read from the console into the provided buffer
     * @param buffer the buffer to save to
     * @param nbyte the maximum number of bytes to read
     * @return number of bytes read
     */
    ssize_t read(char* buffer, size_t nbyte) ;
    /**
     * @brief Sequential write to the console into the provided buffer
     * @param buffer the buffer to write into
     * @param nbyte the maximum number of bytes to write
     * @return the number of bytes written
     */
    ssize_t write(char* buffer, size_t nbyte);
private:
    uint16_t waitForLegalCharacter();
private:
    struct ChipsetRegistersRaw {
        volatile uint16_t consoleIOPort;
        volatile uint16_t consoleFlushPort;
    } __attribute__((packed));
private:
    volatile ChipsetRegistersRaw& _memory;
};

class BuiltinTFTDisplay : public BuiltinIOBaseDevice {
public:
    BuiltinTFTDisplay(uint32_t offset = 0x200);
    uint16_t color565(uint8_t r, uint8_t g, uint8_t b);
    inline uint16_t color565(uint32_t packedColor) {
        return color565(packedColor & 0xFF,
                        (packedColor >> 8) & 0xFF,
                        (packedColor >> 16) & 0xFF);
    }
    void drawPixel(int16_t x, int16_t y, uint16_t color);
    void fillScreen(uint16_t color);
    inline void clearScreen() { fillScreen(color565(0,0,0)); }
    inline void drawPixel(int16_t x, int16_t y, uint8_t r, uint8_t g, uint8_t b)  {
        drawPixel(x, y, color565(r, g, b));
    }
    inline void drawPixel(int16_t x, int16_t y, uint32_t rgb) {
        drawPixel(x, y, color565(rgb));
    }
    void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color);
    inline void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint8_t r, uint8_t g, uint8_t b) {
        uint16_t color = color565(r, g, b);
        fillRect(x, y, w, h, color);
    }
    inline void fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint32_t rgb) {
        uint16_t color = color565(rgb);
        fillRect(x, y, w, h, color);
    }
    void setCursor(int x, int y);
    inline void resetCursor() {
       setCursor(0, 0) ;
    }
    void print(char c);
    void println(const char* c);
    void print(const char* c);
    void print(const std::string& line);
    void println(const std::string& line);
    void flush();

    uint16_t getX() const { return _memory.xPort; }
    void setX(uint16_t value) {_memory.xPort = value; };
    uint16_t getY() const { return _memory.yPort; }
    void setY(uint16_t value) {_memory.yPort = value; };

    uint16_t getX0() const { return _memory.x0Port; }
    void setX0(uint16_t value) {_memory.x0Port = value; };
    uint16_t getY0() const { return _memory.y0Port; }
    void setY0(uint16_t value) {_memory.y0Port = value; };

    uint16_t getX1() const { return _memory.x1Port; }
    void setX1(uint16_t value) {_memory.x1Port = value; };
    uint16_t getY1() const { return _memory.y1Port; }
    void setY1(uint16_t value) {_memory.y1Port = value; };
    uint16_t getX2() const { return _memory.x2Port; }
    void setX2(uint16_t value) {_memory.x2Port = value; };
    uint16_t getY2() const { return _memory.y2Port; }
    void setY2(uint16_t value) {_memory.y2Port = value; };

    uint16_t getRed() const { return _memory.redPort; }
    void setRed(uint16_t value) {_memory.redPort = value; };
    uint16_t getGreen() const { return _memory.greenPort; }
    void setGreen(uint16_t value) {_memory.greenPort = value; };
    uint16_t getBlue() const { return _memory.bluePort; }
    void setBlue(uint16_t value) {_memory.bluePort = value; };

    uint16_t getRadius() const { return _memory.radiusPort; }
    void setRadius(uint16_t value) {_memory.radiusPort= value; };
    uint16_t getForegroundColor() const { return _memory.colorPort; }
    void setForegroundColor(uint16_t value) {_memory.colorPort = value; };
    uint16_t getBackgroundColor() const { return _memory.bgColorPort; }
    void setBackgrounColor(uint16_t value) {_memory.bgColorPort = value; };

    void setRotation(int16_t rot);
    void invertDisplay(bool value);
    void setTextSize(int16_t width);
    void setTextSize(int16_t width, int16_t length);
    uint16_t getCursorX() const;
    uint16_t getCursorY() const;
    uint16_t getRotation() const;


private:
    struct RawTFTCommand {
#define X(type, name) volatile type name ## Port
#define Z(name) X(uint16_t, name)
#define Y(name) X(int16_t, name)
        Y(flush);
        Y(io);
        Y(available);
        Y(availableForWriting);
        Y(command);
        Y(x);
        Y(y);
        Y(w);
        Y(h);
        Y(radius);
        Y(color);
        Y(bgColor);
        Y(x0);
        Y(y0);
        Y(x1);
        Y(y1);
        Y(x2);
        Y(y2);
        Y(red);
        Y(green);
        Y(blue);
        Y(doorbell);
        Z(backlight);
        Z(backlightFrequency);
        X(uint32_t, buttons);
        Z(buttonsQuery);
#undef Y
#undef Z
#undef X
    } __attribute__((packed));
private:
    volatile RawTFTCommand& _memory;
};



class SDCardInterface : public BuiltinIOBaseDevice {
public:
    SDCardInterface();
    bool fileExists(const char* path);
    uint32_t getMaximumNumberOfOpenFiles() const { return maxFileCount_; }
    int openFile(const std::string& path, int flags);
    ssize_t readFile(int fileId, void* buf, size_t count);
    ssize_t writeFile(int fileId, const void* buf, size_t count);
    int closeFile(int fileId);
    off_t seek(int fileId, off_t offset, int whence);
private:
    union ResultPack {
        uint8_t bytes[16];
        uint16_t halves[16 / sizeof(uint16_t)];
        int ints[16/sizeof(int)];
        unsigned int uints[16/sizeof(unsigned int)];
        size_t sizets[16/sizeof(size_t)];
        ssize_t ssizets[16/sizeof(ssize_t)];
        uint32_t words[16 / sizeof(uint32_t)];
        int32_t swords[16/sizeof(int32_t)];
        uint64_t quads[16 / sizeof(uint64_t)];
    };
    struct RawSDCardInterface {
        volatile uint16_t doorbell;
        volatile uint16_t command;
        volatile uint16_t fileId;
        volatile uint16_t modeBits;
        volatile uint32_t seekPosition;
        volatile uint16_t whence;
        volatile uint32_t permissionBits;
        volatile uint16_t openReadWrite;
        volatile uint16_t errorCode;
        volatile ResultPack result;
        volatile char path[80];
        volatile uint16_t reserved0_;
        volatile uint32_t address;
        volatile uint32_t count;
    } __attribute__((packed));
private:
    volatile RawSDCardInterface& _memory;
    uint32_t maxFileCount_;
};

ChipsetBasicFunctions& getBasicChipsetInterface();
BuiltinTFTDisplay& getDisplay();
SDCardInterface& getSDCardInterface();

#endif //I960SXCHIPSET_IODEVICE_H
