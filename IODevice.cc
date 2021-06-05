//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"


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
BuiltinConsole::read() {
    return _memory.ioPort ;
}

void
BuiltinConsole::write(uint16_t c) {
    _memory.ioPort = c;
}

void
BuiltinConsole::write(char c) {
    write(static_cast<uint16_t>(c));
}
void
BuiltinConsole::flush() {
    // doesn't matter what you write as long as you write it
    _memory.flushPort = 1;
}
void
BuiltinConsole::write(const std::string &str) {
    for (int i = 0; i < str.length(); ++i) {
        char c = str[i];
        write(c);
    }
    flush();
}
void
BuiltinConsole::writeLine(const std::string &str) {
    write(str);
    write('\n');
    flush();
}