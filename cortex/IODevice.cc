//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>

namespace cortex
{
    typedef uint8_t IOPage[256];
    struct IOSpace {
        union {
            uint8_t bytes[256];
            struct {
                uint32_t cpuClockSpeed;
                uint32_t chipsetClockSpeed;
                uint32_t SerialRW;
                uint32_t SerialFlush;
                //
                uint32_t ms;
                uint32_t us;
                uint32_t _eepromCapacity;
                uint32_t _sramCapacity;
                // rtc functions
                uint32_t _unixtime;
                uint32_t _secondstime;
                float _rtcTemperature;
                uint32_t _configure32k;
                // random entropy
                uint32_t _hardwareRandomSource;
                uint16_t _systemCounterStatus;
                uint16_t _unused0;
                uint32_t _unused1;
                uint32_t _unused2;
            };
        } builtin;
        union {
            uint8_t bytes[256];
            struct {
                uint16_t execute;
                uint16_t operation;
#define X(index) uint16_t arg ## index 
                X(0);
                X(1);
                X(2);
                X(3);
                X(4);
                X(5);
                X(6);
                X(7);
                X(8);
                X(9);
                X(10);
                X(11);
                X(12);
                X(13);
#undef X
                uint16_t rotation;
                uint16_t brightness;

                uint16_t width;
                uint16_t height;

                uint16_t cursorX;
                uint16_t cursorY;
                uint16_t textSize;
            } __attribute__((packed));
        } gfx;
        uint8_t unmappedPages[6][256];
        uint8_t sramCache[2048];
        uint8_t eeprom[4096];
        inline uint16_t read() volatile noexcept { return builtin.SerialRW; }
        inline void write(uint16_t c) volatile noexcept { builtin.SerialRW = c; }
        inline void flush() volatile noexcept { builtin.SerialFlush = 0; }
        inline uint32_t millis() volatile noexcept { return builtin.ms; }
        inline uint32_t micros() volatile noexcept { return builtin.us; }
        inline uint16_t eepromCapacity() const volatile noexcept { return builtin._eepromCapacity; }
        inline uint16_t sramCacheCapacity() volatile noexcept { return builtin._sramCapacity; }
        inline uint32_t unixtime() volatile noexcept { return builtin._unixtime; }
        inline uint32_t secondstime() volatile noexcept { return builtin._secondstime; }
        inline float rtc_getTemperature() volatile noexcept { return builtin._rtcTemperature; }
        inline bool rtc_32kEnabled() volatile noexcept { return builtin._configure32k; }
        inline void rtc_enable32k() volatile noexcept { builtin._configure32k = 1; }
        inline void rtc_disable32k() volatile noexcept { builtin._configure32k = 0; }
        inline void gfx_set_text_size(uint8_t sx, uint8_t sy) noexcept {
            gfx.textSize = static_cast<uint16_t>(sx) | (static_cast<uint16_t>(sy) << 8);
        }
        inline void gfx_set_text_size(uint8_t sx) noexcept {
            gfx_set_text_size(sx, sx);
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0) volatile noexcept {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1) volatile noexcept {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2) volatile noexcept  {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) volatile noexcept  {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.arg3 = arg3;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) volatile noexcept  {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.arg3 = arg3;
            gfx.arg4 = arg4;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) volatile noexcept {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.arg3 = arg3;
            gfx.arg4 = arg4;
            gfx.arg5 = arg5;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) volatile noexcept {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.arg3 = arg3;
            gfx.arg4 = arg4;
            gfx.arg5 = arg5;
            gfx.arg6 = arg6;
            gfx.execute = 0xFFFF;
        }
        inline void gfx_command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6, uint16_t arg7) volatile noexcept {
            gfx.operation = cmd;
            gfx.arg0 = arg0;
            gfx.arg1 = arg1;
            gfx.arg2 = arg2;
            gfx.arg3 = arg3;
            gfx.arg4 = arg4;
            gfx.arg5 = arg5;
            gfx.arg6 = arg6;
            gfx.arg7 = arg7;
            gfx.execute = 0xFFFF;
        }
        inline uint16_t gfx_width() volatile noexcept { return gfx.width; }
        inline uint16_t gfx_height() volatile noexcept { return gfx.height; }
        inline uint16_t gfx_rotation() volatile noexcept { return gfx.rotation; }
        inline void gfx_set_rotation(uint16_t value) volatile noexcept { gfx.rotation = value; }
        bool systemCounterActive() volatile noexcept {
            return builtin._systemCounterStatus != 0;
        }
        void enableSystemCounter() volatile noexcept {
            builtin._systemCounterStatus = 0xFFFF;
        }
        void disableSystemCounter() volatile noexcept {
            builtin._systemCounterStatus = 0;
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
            write(char c) {
                getIOSpace().write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                getIOSpace().flush();
            }
            void
            write(const char *ptr) {
                write(const_cast<char *>(ptr), strlen(ptr));
            }

            void
            writeLine() {
                write('\n');
            }
            void
            writeLine(const char *ptr) {
                write(ptr);
                writeLine();
            }

            ssize_t
            write(char *buffer, size_t nbyte) {
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
        }
        namespace OLED {
            void command(uint16_t cmd, uint16_t arg0) {
                getIOSpace().gfx_command(cmd, arg0);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1) {
                getIOSpace().gfx_command(cmd, arg0, arg1);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2) {
                getIOSpace().gfx_command(cmd, arg0, arg1, arg2);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3) {
                getIOSpace().gfx_command(cmd, arg0, arg1, arg2, arg3);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4) {
                getIOSpace().gfx_command(cmd, arg0, arg1, arg2, arg3, arg4);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5) {
                getIOSpace().gfx_command(cmd, arg0, arg1, arg2, arg3, arg4, arg5);
            }
            void command(uint16_t cmd, uint16_t arg0, uint16_t arg1, uint16_t arg2, uint16_t arg3, uint16_t arg4, uint16_t arg5, uint16_t arg6) {
                getIOSpace().gfx_command(cmd, arg0, arg1, arg2, arg3, arg4, arg5, arg6);
            }
            uint16_t width() { return getIOSpace().gfx_width(); }
            uint16_t height() { return getIOSpace().gfx_height(); }
            uint8_t getRotation() { return getIOSpace().gfx_rotation(); }
            void setRotation(uint8_t value) { getIOSpace().gfx_set_rotation(value); }
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
        }
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
    IOMemoryBlock&
    EEPROM() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getIOSpace().eeprom), getIOSpace().eepromCapacity());
        return thing;
    }
    IOMemoryBlock&
    SRAM() noexcept {
        static IOMemoryBlock thing(const_cast<uint8_t*>(getIOSpace().sramCache), getIOSpace().sramCacheCapacity());
        return thing;
    }

} // end namespace cortex
