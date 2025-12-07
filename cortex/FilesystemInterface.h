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

#ifndef HITAGIMON_FILESYSTEMINTERFACE_H
#define HITAGIMON_FILESYSTEMINTERFACE_H
#include <stdint.h>
#include <cortex/ModernCpp.h>
#include <string>
namespace cortex {
class File {
public:
    virtual ~File();
    File(int value = -1) : _uid(value) { }
    virtual bool matches(int id) const noexcept;
    virtual bool valid() const noexcept;
    inline int uid() const noexcept { return _uid; }
    virtual uint16_t read() = 0;
    virtual void write(uint16_t value) = 0;
    virtual void flush() = 0;

    virtual void write(char c);
    virtual void write(const char* str, size_t len);
    virtual void write(const char* str);
    virtual void write(const std::string& str);
    virtual void writeLine();
    virtual void writeLine(const std::string& str);
    virtual void writeLine(const char* str, size_t len);
    virtual void writeLine(const char* str);
    virtual void close();
    virtual ssize_t read(char* buffer, size_t nbyte) = 0;
    virtual ssize_t write(char* buffer, size_t nbyte) = 0;
private:
    int _uid;
};

class ConsoleFile : public File {
public:
    ~ConsoleFile();
    ConsoleFile(int id) : File(id) { }
    uint16_t read();
    void write(uint16_t value);
    void flush();
    bool matches(int id) const noexcept;
    bool valid() const noexcept;
    ssize_t read(char* buffer, size_t nbyte);
    ssize_t write(char* buffer, size_t nbyte);
};
} // end namespace cortex
#endif //HITAGIMON_FILESYSTEMINTERFACE_H