//
// Created by jwscoggins on 5/3/21.
//

#include "IODevice.h"
#include "ChipsetInteract.h"
#include <string.h>
#include <errno.h>
#include <fcntl.h>
namespace cortex
{
    uint16_t
    ChipsetBasicFunctions::read() const {
        Opcode code;
        code.full_ = 0;
        return code.memory<uint16_t>();
    }

    void
    ChipsetBasicFunctions::write(uint16_t c) {
        Opcode code;
        code.full_ = 0;
        code.memory<uint16_t>() = c;
    }

    void
    ChipsetBasicFunctions::write(char c) {
        write(static_cast<uint16_t>(c));
    }
    void
    ChipsetBasicFunctions::flush() {
        Opcode code;
        code.function = 2;
        code.group = 0;
        code.subminor = 0;
        // doesn't matter what you write as long as you write it
        code.memory<uint16_t>() = 0;
    }
    void
    ChipsetBasicFunctions::write(const char *ptr) {
        write(const_cast<char *>(ptr), strlen(ptr));
    }

    void
    ChipsetBasicFunctions::writeLine() {
        write('\n');
    }
    void
    ChipsetBasicFunctions::writeLine(const char *ptr) {
        write(ptr);
        writeLine();
    }

    ssize_t
    ChipsetBasicFunctions::write(char *buffer, size_t nbyte) {
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
    ChipsetBasicFunctions::waitForLegalCharacter() {
        uint16_t rawConsoleValue = read();
        while (rawConsoleValue == 0xFFFF) {
            rawConsoleValue = read();
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


    ChipsetBasicFunctions::~ChipsetBasicFunctions() { }
}