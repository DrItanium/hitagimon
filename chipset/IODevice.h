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
    class SDFile {
    private:
#define TwoByteEntry(name) volatile uint16_t name
#define FourByteEntry(name) volatile uint32_t name
        struct FileInterfaceRaw {
            TwoByteEntry(ioPort);
            TwoByteEntry(flushPort);
            TwoByteEntry(syncPort);
            TwoByteEntry(isOpenPort);
            TwoByteEntry(seekEndPort);
            TwoByteEntry(seekBeginningPort);
            FourByteEntry(seekAbsolutePort);
            volatile int32_t seekRelativePort;
            FourByteEntry(sizePort);
            FourByteEntry(permissionsPort);
            TwoByteEntry(writeErrorPort);
            TwoByteEntry(errorCodePort);
            TwoByteEntry(closePort);
        } __attribute__((packed));
#undef TwoByteEntry
#undef FourByteEntry
    public:
        SDFile(uint32_t baseAddress);
        void flush() { raw.flushPort = 1; }
        void sync() { raw.syncPort = 1; }
        void seekEnd() { raw.seekEndPort = 1; }
        void seekBeginning() { raw.seekBeginningPort = 1; }
        void seekRelative(int32_t value) { raw.seekRelativePort = value; }
        void seekAbsolute(uint32_t value) { raw.seekAbsolutePort = value; }
        bool isOpen() const { return raw.isOpenPort; }
        uint32_t getPermissions() const { return raw.permissionsPort; }
        bool getWriteError() const { return raw.writeErrorPort; }
        uint16_t getErrorCode() const { return raw.errorCodePort; }
        void putChar(char value) { raw.ioPort = value; }
        int getChar() { return raw.ioPort; }
        void close() { raw.closePort = 1; }
    private:
        volatile FileInterfaceRaw& raw;
    };
public:
    ChipsetBasicFunctions(uint32_t offset = 0);
    ~ChipsetBasicFunctions();
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
    void triggerInt0();
    /**
     * @brief Has the chipset been told to enable console debugging?
     * @return True if the chipset has console debugging enabled
     */
    bool addressDebuggingEnabled() const;
    /**
     * @brief Turn chipset address debugging on (VERY EXPENSIVE!!!)
     */
    void enableAddressDebugging();
    /**
     * @brief Turn chipset address debugging off
     */
    void disableAddressDebugging();

    int openFile(const char* path, int flags, int mode);

    bool readFile(int fd, void *buf, size_t sz, int &nread);
    bool writeFile(int fd, const void *buf, size_t sz, int &nwrite);
private:
    uint16_t waitForLegalCharacter();
private:
#define TwoByteEntry(name) volatile uint16_t name
#define FourByteEntry(name) volatile uint32_t name
    struct ChipsetRegistersRaw {
        volatile uint16_t consoleIOPort;
        volatile uint16_t consoleFlushPort;
        volatile uint32_t consoleTimeoutPort;
        volatile uint16_t consoleRXBufferSize;
        volatile uint16_t consoleTXBufferSize;
        volatile uint32_t chipsetClockSpeedPort;
        volatile uint16_t cacheLineCountPort;
        volatile uint16_t cacheLineSizePort;
        volatile uint16_t numberOfCacheWaysPort;
        volatile uint16_t triggerInt0Port;
        volatile uint32_t addressDebuggingFlag;
    } __attribute__((packed));
    struct SDCardBaseInterfaceRaw {
        volatile char path[80];
        TwoByteEntry(openPort);
        TwoByteEntry(makeDirectoryPort);
        TwoByteEntry(existsPort);
        TwoByteEntry(removePort);
        FourByteEntry(sdCardClusterCountPort);
        FourByteEntry(sdVolumeSectorCountPort);
        TwoByteEntry(sdbytesPerSectorPort);
        TwoByteEntry(numberOfOpenFilesPort);
        TwoByteEntry(maximumNumberOfOpenFilesPort);
        TwoByteEntry(errorCodePort);
        FourByteEntry(permissionsPort);
        TwoByteEntry(makeMissingParentDirectoriesPort);
    } __attribute__((packed));
#undef FourByteEntry
#undef TwoByteEntry
private:
    volatile ChipsetRegistersRaw& _memory;
    volatile SDCardBaseInterfaceRaw& _sdbase;
    SDFile** openFiles;
};

ChipsetBasicFunctions& getBasicChipsetInterface();

#endif //I960SXCHIPSET_IODEVICE_H
