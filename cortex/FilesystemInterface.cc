/*
hitagimon
Copyright (c) 2020-2025, Joshua Scoggins
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

#include <cortex/FilesystemInterface.h>
#include <cortex/IODevice.h>
namespace cortex {
    File::~File() {
        // @todo call close here?
        close();
    }
    void
    File::close() {
        // do nothing for the generic implementation
    }
    void
    File::write(char c) {
        write(static_cast<uint16_t>(c));
    }
    void
    File::write(const char* str, size_t len) {

    }
    void
    File::write(const char* str) {
        write(str, strlen(str));
    }
    void
    File::write(const std::string& str) {
        write(str.c_str(), str.size());
    }
    void
    File::writeLine() {
        write('\n');
    }
    void
    File::writeLine(const std::string& str) {
        writeLine(str.c_str(), str.size());
    }
    void
    File::writeLine(const char* str, size_t len) {
        write(str, len);
        writeLine();
    }
    void
    File::writeLine(const char* str) {
        writeLine(str, strlen(str));
    }


    bool File::matches(int id) const { return id == _uid; }
    bool File::valid() const { return !matches(-1); }

    ConsoleFile::~ConsoleFile() { }

    bool
    ConsoleFile::matches(int id) const {

        switch (id) {
            case 0:
            case 1:
            case 2:
                return true;
            default:
                return false;
        }
    }
    uint16_t ConsoleFile::read() { return ChipsetBasicFunctions::Console::read(); }
    void ConsoleFile::write(uint16_t value) { ChipsetBasicFunctions::Console::write(value); }
    bool ConsoleFile::valid() const { return true; }
    void ConsoleFile::flush() { ChipsetBasicFunctions::Console::flush(); }
    ssize_t ConsoleFile::read(char*buffer, size_t nbyte) { return ChipsetBasicFunctions::Console::read(buffer, nbyte); }
    ssize_t ConsoleFile::write(char*buffer, size_t nbyte) { return ChipsetBasicFunctions::Console::write(buffer, nbyte); }








}