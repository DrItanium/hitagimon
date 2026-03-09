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
#include <cstdint>
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
    virtual void write(const std::string& str);
    virtual void writeLine(const std::string& str);
    virtual void close();
    virtual ssize_t read(char* buffer, size_t nbyte) = 0;
    virtual ssize_t write(const char* buffer, size_t nbyte) = 0;
    operator bool() const noexcept;
    virtual bool canSeek() const noexcept;
    virtual off_t seek(off_t offset, int whence) noexcept;
    virtual bool isatty() const noexcept { return false; }
private:
    int _uid;
};
class NullFile : public File {
    public:
        NullFile() : File() { }
        ~NullFile() override = default;
        uint16_t read() { return 0; }
        void write(uint16_t) { }
        bool valid() const noexcept { return false; }
        void flush() noexcept { }
        ssize_t read(char*, size_t) { return 0; }
        ssize_t write(const char*, size_t) { return 0; }
        bool canSeek() const noexcept { return false; }
};
/**
 * @brief A very thin wrapper around the IO space, useful for eliminating direct dependencies
 */
class ConsoleFile : public File {
public:
    ~ConsoleFile() override;
    ConsoleFile() : File(0) { }
    uint16_t read();
    void write(uint16_t value);
    void flush();
    bool matches(int id) const noexcept;
    bool valid() const noexcept;
    ssize_t read(char* buffer, size_t nbyte);
    ssize_t write(const char* buffer, size_t nbyte);
    bool canSeek() const noexcept { return true; }
    off_t seek(off_t offset, int whence) noexcept;
    bool isatty() const noexcept { return true; }
};

class SDCardFile : public File {
    public:
        SDCardFile(int systemUid = -1, uint64_t chipsetUid = 0) : File(systemUid), _chipsetUid(chipsetUid) { }
        ~SDCardFile() override;
        uint16_t read() override;
        void write(uint16_t value) override;
        void flush() override;
        bool matches(int id) const noexcept override;
        bool valid() const noexcept override;
        ssize_t read(char* buffer, size_t nbyte) override;
        ssize_t write(const char* buffer, size_t nbyte) override;
        bool canSeek() const noexcept override;
        off_t seek(off_t offset, int whence) noexcept override;
        bool isatty() const noexcept override { return false; }
    private:
        uint64_t _chipsetUid = 0;
};

File& getConsole();
File& getNullFile();
namespace Filesystem {
    File& getFile(int fd) noexcept;
    File& openFile(const char* path, int flags, int mode);
    bool linkFile(const char* path1, const char* path2);
    bool unlinkFile(const char* path);
}
} // end namespace cortex
#endif //HITAGIMON_FILESYSTEMINTERFACE_H
