//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>
#include <fileuids.h>

namespace cortex
{
    typedef uint8_t IOPage[256];
    struct IOSpace {
        union {
            IOPage bytes;
            struct {
                uint32_t cpuClockSpeed;
                uint32_t chipsetClockSpeed;
                uint32_t SerialRW;
                uint32_t SerialFlush;
                //
                uint32_t ms;
                uint32_t us;
                uint32_t cycleCount;
                uint32_t timeSinceBoot;
                // rtc functions
                uint32_t _unixtime;
                uint32_t _secondstime;
                float _rtcTemperature;
                uint32_t _configure32k;
                // random entropy
                uint32_t _hardwareRandomSource;
                uint16_t _systemCounterStatus;
                uint16_t _unused0;
                uint32_t _hardwareEntropySource;
                uint32_t _unused2;
                // capacity information
                uint32_t _unused3;
                uint32_t _sramCapacity;
                uint32_t _sram2Capacity;
                uint32_t _unused5;
                // system execution stats
                uint64_t _idleCycles;
                uint64_t _totalCycles;
                uint64_t _systemCounter;
            };
        } builtin;
        static_assert(sizeof(builtin) == 256);
        union {
            IOPage bytes;
            struct {
                uint16_t _width;
                uint16_t _height;
                uint8_t _rotation;
                uint8_t _invert;
                uint16_t _unused;
                uint16_t _cursorX;
                uint16_t _cursorY;
                uint32_t _graphicsReturn;
                // opcode instruction
                uint16_t _graphicsOpcode;
                uint16_t _args[7];
            };
        } display;
        static_assert(sizeof(display) == 256);
        IOPage unmappedPages[14];
        // remaining 48 pages are part of the sram cache
        uint8_t sramCache[256 * (256-16)];
        inline uint16_t read() volatile noexcept { return builtin.SerialRW; }
        inline void write(uint16_t c) volatile noexcept { builtin.SerialRW = c; }
        inline void flush() volatile noexcept { builtin.SerialFlush = 0; }
        inline uint32_t millis() volatile noexcept { return builtin.ms; }
        inline uint32_t micros() volatile noexcept { return builtin.us; }
        inline uint32_t cycleCount() volatile noexcept { return builtin.cycleCount; }
        inline uint32_t sramCacheCapacity() volatile noexcept { return builtin._sramCapacity; }
        inline uint32_t sram2CacheCapacity() volatile noexcept { return builtin._sram2Capacity; }
        inline uint32_t unixtime() volatile noexcept { return builtin._unixtime; }
        inline uint32_t secondstime() volatile noexcept { return builtin._secondstime; }
        inline float rtc_getTemperature() volatile noexcept { return builtin._rtcTemperature; }
        inline bool rtc_32kEnabled() volatile noexcept { return builtin._configure32k; }
        inline void rtc_enable32k() volatile noexcept { builtin._configure32k = 1; }
        inline void rtc_disable32k() volatile noexcept { builtin._configure32k = 0; }
        bool systemCounterActive() volatile noexcept {
            return builtin._systemCounterStatus != 0;
        }
        void enableSystemCounter() volatile noexcept {
            builtin._systemCounterStatus = 0xFFFF;
        }
        void disableSystemCounter() volatile noexcept {
            builtin._systemCounterStatus = 0;
        }
        void setSystemCounter(uint64_t value) volatile noexcept {
            builtin._systemCounter = value;
        }
        uint64_t getSystemCounter() const volatile noexcept { return builtin._systemCounter; }
        uint64_t getIdleCycles() const volatile noexcept { return builtin._idleCycles; }
        uint64_t getTotalCycles() const volatile noexcept { return builtin._totalCycles; }
        uint16_t getWidth() const volatile noexcept { return display._width; }
        uint16_t getHeight() const volatile noexcept { return display._height; }
        uint8_t getRotation() const volatile noexcept { return display._rotation; }
        void setRotation(uint8_t value) volatile noexcept { display._rotation = value; }
        bool inverted() const volatile noexcept { return display._invert != 0; }
        void invertDisplay(bool value) volatile noexcept { display._invert = value; }
        uint16_t getCursorX() const volatile noexcept { return display._cursorX; }
        uint16_t getCursorY() const volatile noexcept { return display._cursorY; }
        void setCursorX(uint16_t value) volatile noexcept { display._cursorX = value; }
        void setCursorY(uint16_t value) volatile noexcept { display._cursorY = value; }
        uint32_t getGraphicsReturn() const volatile noexcept { return display._graphicsReturn; }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._args[2] = arg2;
            display._args[3] = arg3;
            display._args[4] = arg4;
            display._args[5] = arg5;
            display._args[6] = arg6;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._args[2] = arg2;
            display._args[3] = arg3;
            display._args[4] = arg4;
            display._args[5] = arg5;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._args[2] = arg2;
            display._args[3] = arg3;
            display._args[4] = arg4;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._args[2] = arg2;
            display._args[3] = arg3;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1, uint16_t arg2) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._args[2] = arg2;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0, uint16_t arg1) volatile noexcept { 
            display._args[0] = arg0;
            display._args[1] = arg1;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode, uint16_t arg0) volatile noexcept { 
            display._args[0] = arg0;
            display._graphicsOpcode = opcode;
        }
        void 
        doGraphicsInstruction(uint16_t opcode) volatile noexcept { 
            display._graphicsOpcode = opcode;
        }
    } __attribute__((packed));
    volatile IOSpace& getIOSpace() noexcept {
        return memory<IOSpace>(0xFE000000);
    }
    namespace ChipsetBasicFunctions {
        namespace Console {

            uint16_t
            read() {
                return getIOSpace().read();
            }

            void
            write(uint16_t c) {
                getIOSpace().write(c);
            }

            void
            flush() {
                getIOSpace().flush();
            }

            ssize_t
            write(const char *buffer, size_t nbyte) {
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
            waitForLegalCharacter() {
                uint16_t rawConsoleValue = read();
                while (rawConsoleValue == 0xFFFF) {
                    rawConsoleValue = read();
                }
                return rawConsoleValue;
            }
            ssize_t
            read(char *buffer, size_t nbyte) {
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
        } // end namespace Console
        namespace Timer {
            uint32_t secondstime() noexcept { return getIOSpace().secondstime(); }
            uint32_t unixtime() noexcept { return getIOSpace().unixtime(); }
            uint32_t millis() noexcept { return getIOSpace().millis(); }
            uint32_t micros() noexcept { return getIOSpace().micros(); }
            float getRTCTemperature() noexcept { return getIOSpace().rtc_getTemperature(); }
            uint32_t chipsetCycleCount() noexcept { return getIOSpace().cycleCount(); }
        } // end namespace RTC
        namespace Info {
            uint32_t
            getCPUClockSpeed() noexcept {
                return getIOSpace().builtin.cpuClockSpeed;
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return getIOSpace().builtin.chipsetClockSpeed;
            }
            uint64_t getIdleCycles() noexcept { return getIOSpace().getIdleCycles(); }
            uint64_t getTotalCycles() noexcept { return getIOSpace().getTotalCycles(); }
        }
        namespace Random {
            uint32_t getHardwareRandomNumber() noexcept {
                return getIOSpace().builtin._hardwareRandomSource;
            }
        }
        namespace SystemCounter {
            bool active() noexcept { 
                return getIOSpace().systemCounterActive(); 
            }
            void enable() noexcept { 
                getIOSpace().enableSystemCounter();
            }
            void disable() noexcept {
                getIOSpace().disableSystemCounter();
            }
            void set(uint64_t value) noexcept {
                getIOSpace().setSystemCounter(value);
            }
            uint64_t get() noexcept {
                return getIOSpace().getSystemCounter();
            }

        }
        namespace Display {
            void command(uint16_t cmd) noexcept { getIOSpace().doGraphicsInstruction(cmd); }
            void command(uint16_t cmd, uint16_t arg0) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0); }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1); }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1, arg2); }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1, arg2, arg3); } 
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1, arg2, arg3, arg4); }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1, arg2, arg3, arg4, arg5); }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) noexcept { getIOSpace().doGraphicsInstruction(cmd, arg0, arg1, arg2, arg3, arg4, arg5, arg6); }
            uint16_t width() noexcept { return getIOSpace().getWidth(); }
            uint16_t height() noexcept { return getIOSpace().getHeight(); }
            uint8_t getRotation() noexcept { return getIOSpace().getRotation(); }
            void setRotation(uint8_t value) noexcept { getIOSpace().setRotation(value); }
            void invertDisplay(bool value) noexcept { getIOSpace().invertDisplay(value); }
            void setCursor(uint16_t x, uint16_t y) noexcept {
                auto& space = getIOSpace();
                space.setCursorX(x);
                space.setCursorY(y);
            }
            uint16_t getCursorX() noexcept { return getIOSpace().getCursorX(); }
            uint16_t getCursorY() noexcept { return getIOSpace().getCursorY(); }
        }
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
    IOMemoryBlock&
    SRAM() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getIOSpace().sramCache), getIOSpace().sramCacheCapacity());
        return thing;
    }
    union SRAMCache2 {
#define X(name, type) type name [ 0x10000 / sizeof(type)]
        X(bytes, uint8_t);
        X(shorts, uint16_t);
        X(words, uint32_t);
        X(longs, uint64_t);
#undef X
    };
    volatile SRAMCache2& getSRAM2() noexcept {
        return memory<SRAMCache2>(0xFE000000 + 0x10000);
    }
    IOMemoryBlock&
    SRAM2() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getSRAM2().bytes), getIOSpace().sram2CacheCapacity());
        return thing;
    }

} // end namespace cortex
