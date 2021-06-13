//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"
#include <string.h>


BuiltinIOBaseDevice::BuiltinIOBaseDevice(uint32_t offset) : offset_(offset), baseAddress_(getIOBase0Address(offset)) { }
BuiltinLED::BuiltinLED(uint32_t offset) : BuiltinIOBaseDevice(offset), _memory(memory<uint8_t>(baseAddress_)) {

}
void
BuiltinLED::toggle() {
    _memory = (_memory != 0) ? 0 : 0xFF;
}

bool
BuiltinLED::getValue() {
    return _memory != 0;
}

void
BuiltinLED::setValue(bool value) {
    _memory = (value ? 0xFF : 0x00);
}

BuiltinConsole::BuiltinConsole(uint32_t offset) : BuiltinIOBaseDevice(offset), _memory(memory<RawConsoleStructure>(baseAddress_)) { }

bool
BuiltinConsole::available() const {
    return static_cast<bool>(_memory.isAvailable);
}
bool
BuiltinConsole::availableForWrite() const {
    return static_cast<bool>(_memory.isAvailableForWriting);
}

uint16_t
BuiltinConsole::read() const {
    return _memory.ioPort ;
}

void
BuiltinConsole::write(uint16_t c) {
    _memory.ioPort = c;
}

void
BuiltinConsole::write(char c) {
    _memory.ioPort = c;
}
void
BuiltinConsole::flush() {
    // doesn't matter what you write as long as you write it
    _memory.flushPort = 1;
}
void
BuiltinConsole::write(const char* ptr) {
    for (const char* v = ptr; *v; ++v) {
        write(*v);
    }
}

enum TFTOpcodes {
    None = 0,
    SetRotation,
    InvertDisplay,
    FillRect,
    FillScreen,
    DrawLine,
    DrawRect,
    DrawCircle,
    FillCircle,
    DrawTriangle,
    FillTriangle,
    SetTextSizeSquare,
    SetTextSizeRectangle,
    SetCursor,
    SetTextColor0,
    SetTextColor1,
    SetTextWrap,
    GetWidth,
    GetHeight,
    GetRotation,
    GetCursorX,
    GetCursorY,
    DrawPixel,
    Color565,
    DrawRoundRect,
    FillRoundRect,
};
BuiltinTFTDisplay::BuiltinTFTDisplay(uint32_t offset) : BuiltinIOBaseDevice(offset), _memory(memory<RawTFTCommand>(baseAddress_)) { }

void
BuiltinTFTDisplay::fillScreen(uint16_t color) {
    _memory.commandPort = FillScreen;
    _memory.colorPort = color;
    _memory.doorbellPort = 1;
}
void
BuiltinTFTDisplay::drawPixel(int16_t x, int16_t y, uint16_t color) {
    _memory.commandPort = DrawPixel;
    _memory.colorPort = color;
    _memory.xPort = x;
    _memory.yPort = y;
    _memory.doorbellPort = 1;
}

uint16_t
BuiltinTFTDisplay::color565(uint8_t r, uint8_t g, uint8_t b) {
    _memory.commandPort = Color565;
    _memory.redPort = r;
    _memory.greenPort = g;
    _memory.bluePort = b;
    return _memory.doorbellPort;
}

void
BuiltinTFTDisplay::fillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) {
    _memory.commandPort = FillRect;
    _memory.xPort = x;
    _memory.yPort = y;
    _memory.colorPort = color;
    _memory.wPort = w;
    _memory.hPort = h;
    _memory.doorbellPort = 1;
}


void
BuiltinTFTDisplay::setCursor(int x, int y) {
    _memory.commandPort = SetCursor;
    _memory.xPort = x;
    _memory.yPort = y;
    _memory.doorbellPort = 1;
}

void
BuiltinTFTDisplay::flush() {
    /// @todo implement
}

void
BuiltinTFTDisplay::print(char c) {
    /// @todo implement
}
void
BuiltinConsole::writeLine() {
    write('\n');
}
void BuiltinConsole::writeLine(const char* ptr) {
    write(ptr);
    writeLine();
}

BuiltinChipsetDebugInterface::BuiltinChipsetDebugInterface() : BuiltinIOBaseDevice(0xFFFF00),
_memory(memory<RawDebugRegisters>(getIOBase0Address(0xFFFF00))) {}

void BuiltinChipsetDebugInterface::disableCacheLineActivityLogging() {
    _memory.displayCacheLineActivity = false;
}

void BuiltinChipsetDebugInterface::enableCacheLineActivityLogging() {
    _memory.displayCacheLineActivity = true;
}

void BuiltinChipsetDebugInterface::enableMemoryReadWriteLogging() {
    _memory.displayMemoryReadWrites = true;
}

void BuiltinChipsetDebugInterface::disableMemoryReadWriteLogging() {
    _memory.displayMemoryReadWrites = false;
}

void BuiltinChipsetDebugInterface::disableSDCardActivityLogging() {
    _memory.displaySDCardActivity = false;
}

void BuiltinChipsetDebugInterface::enableSDCardActivityLogging() {
    _memory.displaySDCardActivity = true;
}

BuiltinLED&
getBuiltinLed() {
    static BuiltinLED theLed;
    return theLed;
}

BuiltinConsole& getConsole() {
    static BuiltinConsole theConsole;
    return theConsole;
}
BuiltinTFTDisplay& getDisplay() {
    static BuiltinTFTDisplay theDisplay;
    return theDisplay;
}
BuiltinChipsetDebugInterface& getChipsetDebugInterface() {
    static BuiltinChipsetDebugInterface theDebug;
    return theDebug;
}

ssize_t
BuiltinConsole::write(char *buffer, size_t nbyte) {
    for (size_t i = 0; i < nbyte; ++i) {
        _memory.ioPort = buffer[i];
    }
    return nbyte;
}

ssize_t
BuiltinConsole::read(char *buffer, size_t nbyte) const {
    ssize_t numRead = 0;
    for (size_t i = 0; i < nbyte; ++i) {
        int16_t curr = static_cast<int16_t>(_memory.ioPort);
        if (curr == -1) {
            return numRead;
        }
        buffer[i] = static_cast<char>(curr);
        ++numRead;
    }
    return numRead;
}
namespace SDCard {
    enum Operations {
        NoneOperation = 0,
        OpenFile,
        CloseFile,
        FileExists,
        MakeDirectory,
        RemoveDirectory,
        GetNumberOfOpenFiles,
        GetMaximumNumberOfOpenFiles,
        GetFixedPathMaximum,
        // File specific operations
        IsValidFileId = 0x8000,
        FileRead,
        FileWrite,
        FileFlush,
        FileSeek,
        FileIsOpen,
        GetFileName,
        GetFileBytesAvailable,
        GetFilePosition,
        GetFilePermissions,
        GetFileSize,
        GetFileCoordinates,
    };

    enum ErrorCodes {
        NoError = 0,
        NoCommandProvided,
        UndefinedCommandProvided,
        BadFileId,
        FileIsNotValid,
        CriticalFileSideChannelAttempt,
        UnimplementedCommand,
    };
} // end namespace SDCard

SDCardInterface::SDCardInterface() : BuiltinIOBaseDevice(0x300), _memory(memory<RawSDCardInterface>(getIOBase0Address(0x300))) {
    // clear out any operations
    _memory.command = SDCard::NoneOperation;
}


SDCardInterface& getSDCardInterface() {
    static SDCardInterface theSDCard;
    return theSDCard;
}
