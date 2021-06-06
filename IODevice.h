//
// Created by jwscoggins on 5/3/21.
//

#ifndef I960SXCHIPSET_IODEVICE_H
#define I960SXCHIPSET_IODEVICE_H
#include <stdint.h>
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
class BuiltinLED : public BuiltinIOBaseDevice {
public:
    BuiltinLED(uint32_t offset = 0);
    bool getValue();
    void setValue(bool value);
    void toggle();
private:
    volatile uint8_t& _memory;
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
    uint16_t read();
    void write(uint16_t value);
    void write(char c);
    void write(const char* ptr, bool newLine = false);
    inline void writeLine(const char* ptr) { write(ptr, true); }
    inline void writeLine(const std::string& str) { write(str, true); }
    void write(const std::string& str, bool newLine = false);
private:
    struct RawConsoleStructure {
        volatile uint16_t flushPort;
        volatile uint16_t isAvailable;
        volatile uint16_t isAvailableForWriting;
        volatile uint16_t ioPort;
    } __attribute__ ((packed));
private:
    volatile RawConsoleStructure& _memory;
};

#endif //I960SXCHIPSET_IODEVICE_H
