//
// Created by jwscoggins on 5/3/21.
//

#ifndef I960SXCHIPSET_IODEVICE_H
#define I960SXCHIPSET_IODEVICE_H
#include <stdint.h>
#include <unistd.h>
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
enum PortZPins {
    PZ0 = 0,
    PZ1,
    PZ2,
    PZ3,
    PZ4,
    PZ5,
    PZ6,
    PZ7,
};

enum PinModes {
    INPUT,
    OUTPUT,
    INPUT_PULLUP,
};
/**
 * @brief Manages the builtin led provided by the chipset
 */
class ChipsetBasicFunctions : public BuiltinIOBaseDevice {
public:
    ChipsetBasicFunctions(uint32_t offset = 0);
    bool getLEDValue();
    void setLEDValue(bool value);
    void toggleLED();
    uint8_t readPortZGPIO();
    void setPortZGPIO(uint8_t value);
    uint8_t readPortZGPIOPullup();
    void setPortZGPIOPullup(uint8_t value);
    uint8_t readPortZGPIOPolarity();
    void setPortZGPIOPolarity(uint8_t value);
    uint8_t readPortZGPIODirection();
    void setPortZGPIODirection(uint8_t value);
    void digitalWrite(PortZPins pin, bool value);
    void pinMode(PortZPins pin, PinModes mode);
    bool digitalRead(PortZPins pin);
private:
    struct ChipsetRegistersRaw {
       volatile uint8_t led;
       volatile uint8_t portzGPIO;
       volatile uint8_t portzGPIOPullup;
       volatile uint8_t portzGPIOPolarity;
       volatile uint8_t portzGPIODirection;
    } __attribute__((packed));
private:
    volatile ChipsetRegistersRaw& _memory;
};

/**
 * @brief The console on hitagi is very simple, it really only acts as a input/output channel
 */
class BuiltinConsole : public BuiltinIOBaseDevice {
public:
    BuiltinConsole(uint32_t offset = 0x100);
    void flush();
    bool available() const;
    bool availableForWrite() const;
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
    ssize_t read(char* buffer, size_t nbyte) const;
    /**
     * @brief Sequential write to the console into the provided buffer
     * @param buffer the buffer to write into
     * @param nbyte the maximum number of bytes to write
     * @return the number of bytes written
     */
    ssize_t write(char* buffer, size_t nbyte);
private:
    struct RawConsoleStructure {
        volatile uint16_t flushPort;
        volatile uint16_t isAvailable;
        volatile uint16_t isAvailableForWriting;
        volatile uint16_t ioPort;
        volatile uint16_t bufDoorbell;
        volatile uint16_t bufSize;
        volatile uint16_t bufLength;
        volatile char buf[128];
    } __attribute__ ((packed));
private:
    volatile RawConsoleStructure& _memory;
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
    void flush();

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

class BuiltinChipsetDebugInterface : public BuiltinIOBaseDevice {
public:
    BuiltinChipsetDebugInterface();
    void enableMemoryReadWriteLogging();
    void disableMemoryReadWriteLogging();
    void enableCacheLineActivityLogging();
    void disableCacheLineActivityLogging();
    bool memoryReadWriteLoggingEnabled() const { return _memory.displayMemoryReadWrites; }
    bool cacheLineActivityLoggingEnabled() const { return _memory.displayCacheLineActivity; }
    bool sdCardActivityLoggingEnabled() const { return _memory.displaySDCardActivity; }
    void enableSDCardActivityLogging();
    void disableSDCardActivityLogging();
private:
    struct RawDebugRegisters {
        volatile bool displayMemoryReadWrites;
        volatile bool displayCacheLineActivity;
        volatile bool displaySDCardActivity;
    } __attribute__((packed));
private:
    volatile RawDebugRegisters& _memory;
};

class TemporaryReadWriteLoggingEnable {
public:
    TemporaryReadWriteLoggingEnable(BuiltinChipsetDebugInterface& iface) : iface_(iface) { iface_.enableMemoryReadWriteLogging(); }
    ~TemporaryReadWriteLoggingEnable() { iface_.disableMemoryReadWriteLogging(); }
private:
    BuiltinChipsetDebugInterface& iface_;
};

class TemporaryRWLoggingDisabler {
public:
    TemporaryRWLoggingDisabler(BuiltinChipsetDebugInterface& iface) : iface_(iface), shouldToggle_(iface.memoryReadWriteLoggingEnabled())  {
        if (shouldToggle_) {
            iface_.disableMemoryReadWriteLogging();
        }
    }
    ~TemporaryRWLoggingDisabler() {
        if (shouldToggle_) {
            iface_.enableMemoryReadWriteLogging();
        }
    }
private:
    BuiltinChipsetDebugInterface& iface_;
    bool shouldToggle_;
};

class SDCardInterface : public BuiltinIOBaseDevice {
public:
    SDCardInterface();
private:
    union ResultPack {
        uint8_t bytes[16];
        uint16_t halves[16 / sizeof(uint16_t)];
        uint32_t words[16 / sizeof(uint32_t)];
        uint64_t quads[16 / sizeof(uint64_t)];
    };
    struct RawSDCardInterface {
        volatile uint16_t doorbell;
        volatile uint16_t command;
        volatile uint16_t fileId;
        volatile uint16_t modeBits;
        volatile uint32_t seekPosition;
        volatile uint16_t whence;
        volatile ResultPack result;
        volatile uint16_t errorCode;
        volatile char path[80];
    } __attribute__((packed));
private:
    volatile RawSDCardInterface& _memory;
};

ChipsetBasicFunctions& getBasicChipsetInterface();
BuiltinConsole& getConsole();
BuiltinTFTDisplay& getDisplay();
BuiltinChipsetDebugInterface& getChipsetDebugInterface();
SDCardInterface& getSDCardInterface();

#endif //I960SXCHIPSET_IODEVICE_H
