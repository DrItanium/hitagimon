//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"
#include <string.h>
#include <errno.h>
#include <fcntl.h>


BuiltinIOBaseDevice::BuiltinIOBaseDevice(uint32_t offset) : offset_(offset), baseAddress_(getIOBase0Address(offset)) { }
ChipsetBasicFunctions::ChipsetBasicFunctions(uint32_t offset) : BuiltinIOBaseDevice(offset),
_memory(memory<ChipsetRegistersRaw>(baseAddress_)),
_sdbase(memory<SDCardBaseInterfaceRaw>(baseAddress_ + 0x100))
{

}

uint16_t
ChipsetBasicFunctions::read() const {
    return _memory.consoleIOPort;
}

void
ChipsetBasicFunctions::write(uint16_t c) {
    _memory.consoleIOPort = c;
}

void
ChipsetBasicFunctions::write(char c) {
    write(static_cast<uint16_t>(c));
}
void
ChipsetBasicFunctions::flush() {
    // doesn't matter what you write as long as you write it
    _memory.consoleFlushPort = 1;
}
void
ChipsetBasicFunctions::write(const char* ptr) {
    write(const_cast<char*>(ptr), strlen(ptr));
}

void
ChipsetBasicFunctions::writeLine() {
    write('\n');
}
void
ChipsetBasicFunctions::writeLine(const char* ptr) {
    write(ptr);
    writeLine();
}


ChipsetBasicFunctions&
getBasicChipsetInterface() {
    static ChipsetBasicFunctions theLed;
    return theLed;
}

ssize_t
ChipsetBasicFunctions::write(char *buffer, size_t nbyte) {
    // unlike reading, we must be sequential in writing
    ssize_t numWritten = 0;
    for (size_t i = 0; i < nbyte; ++i) {
        _memory.consoleIOPort = static_cast<uint16_t>(static_cast<unsigned char>(buffer[i]));
        ++numWritten;
    }
    flush();
    return numWritten;
}
uint16_t
ChipsetBasicFunctions::waitForLegalCharacter() {
    uint16_t rawConsoleValue = _memory.consoleIOPort;
    while (rawConsoleValue == 0xFFFF) {
        rawConsoleValue = _memory.consoleIOPort;
    }
    return rawConsoleValue;
}
ssize_t
ChipsetBasicFunctions::read(char *buffer, size_t nbyte) {
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




void
ChipsetBasicFunctions::triggerInt0() {
    // force the chipset to trigger an interrupt
    _memory.triggerInt0Port = 1;
}
bool
ChipsetBasicFunctions::addressDebuggingEnabled() const {
    return _memory.addressDebuggingFlag != 0;
}
void
ChipsetBasicFunctions::enableAddressDebugging() {
    // upper 16-bits are ignored on the chipset side but who cares
    _memory.addressDebuggingFlag = 0xFFFFFFFF;
}
void
ChipsetBasicFunctions::disableAddressDebugging() {
    _memory.addressDebuggingFlag = 0;
}
