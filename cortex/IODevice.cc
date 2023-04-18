//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>
namespace cortex
{
    namespace ChipsetBasicFunctions {
        template<typename T>
        inline volatile T* computeRegisterBasePointer(uint8_t group, uint8_t device, uint8_t opcode, uint8_t subminor = 0) noexcept {
            return &Opcode(group, device, opcode, subminor).memory<T>();
        }
        template<typename T>
        inline volatile T& computeRegisterBaseReference(uint8_t group, uint8_t device, uint8_t opcode, uint8_t subminor = 0) noexcept {
            return Opcode(group, device, opcode, subminor).memory<T>();
        }

        namespace Devices {
            enum {
                Info = 0,
                Serial,
                Timer,
                Display,
            };
        }
        namespace {
            template<uint8_t DeviceKind>
            inline bool available() noexcept {
                static volatile uint16_t& address = computeRegisterBaseReference<uint16_t>(0, DeviceKind, 0, 0);
                return address;
            }
            template<uint8_t DeviceKind>
            inline uint8_t size() noexcept {
                static volatile uint8_t & address = computeRegisterBaseReference<uint8_t>(0, DeviceKind, 0, 0);
                return address;
            }
        }

        namespace Console {
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    RW,
                    Flush,
                    Baud,
                };
            }
            namespace {
                volatile uint16_t* rwAddress = nullptr;
                volatile uint16_t* flushAddress = nullptr;
                void
                configure() noexcept {
                    rwAddress = computeRegisterBasePointer<uint16_t>(0, Devices::Serial, Opcodes::RW, 0);
                    flushAddress = computeRegisterBasePointer<uint16_t>(0, Devices::Serial, Opcodes::Flush, 0);
                }
            }
            bool available() noexcept { return cortex::ChipsetBasicFunctions::available<Devices::Serial>(); }

            uint16_t
            read() {
                return *rwAddress;
            }

            void
            write(uint16_t c) {
                *rwAddress = c;
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                *flushAddress = 0;
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
            // this is a pre c++11 "hack" to support referencing enums by name
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    CompareValue,
                    Prescalar,
                };
            }
            namespace {
                volatile uint16_t* compareValueRegister = nullptr;
                volatile uint8_t* prescalarRegister = nullptr;
                void
                configure() noexcept {
                    compareValueRegister = computeRegisterBasePointer<uint16_t>(0, Devices::Timer, Opcodes::CompareValue, 0);
                    prescalarRegister = computeRegisterBasePointer<uint8_t>(0, Devices::Timer, Opcodes::Prescalar, 0);
                }
            }
            bool available() noexcept { return cortex::ChipsetBasicFunctions::available<Devices::Timer>(); }
            uint32_t
            unixtime() noexcept {
                return 0;
            }
            void
            setCompareValue(uint16_t value) noexcept {
                *compareValueRegister = value;
            }

            uint16_t
            getCompareValue() noexcept {
                return *compareValueRegister;
            }
            void
            setPrescalar(uint8_t value) noexcept {
                *prescalarRegister = value;
            }
            uint8_t
            getPrescalar() noexcept {
                return *prescalarRegister;
            }
        } // end namespace RTC
        namespace Info {
            namespace Opcodes {
                enum {
                    Available = 0,
                    Size,
                    GetCPUClockSpeed,
                    GetChipsetClockSpeed,
                    GetExternalIAC,
                };
            }
            namespace {
                volatile uint32_t* cpuClockSpeedRegister = nullptr;
                volatile uint32_t* chipsetClockSpeedRegister = nullptr;
                volatile IACMessage* externalIACMessageRegister = nullptr;

                void
                configure() noexcept {
                    cpuClockSpeedRegister = computeRegisterBasePointer<uint32_t>(0, Devices::Info, Opcodes::GetCPUClockSpeed, 0);
                    chipsetClockSpeedRegister = computeRegisterBasePointer<uint32_t>(0, Devices::Info, Opcodes::GetChipsetClockSpeed, 0);
                    externalIACMessageRegister = computeRegisterBasePointer<IACMessage>(0, Devices::Info, Opcodes::GetExternalIAC, 0);
                }
            }
            bool available() noexcept { return cortex::ChipsetBasicFunctions::available<Devices::Info>(); }
            uint32_t
            getCPUClockSpeed() noexcept {
                return *cpuClockSpeedRegister;
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return *chipsetClockSpeedRegister;
            }
            IACMessage*
            getExternalMessage() noexcept {
                return const_cast<IACMessage*>(externalIACMessageRegister);
            }
        }
        namespace Display {
            namespace Operations {
                enum {
                    Available,
                    Size,
                    RW, // for the "serial" aspect so we can print out to the screen
                    Flush,
                    DisplayWidthHeight,
                    Rotation,
                    InvertDisplay,
                    ScrollTo,
                    SetScrollMargins,
                    SetAddressWindow,
                    ReadCommand8,
                    CursorX, CursorY, CursorXY,
                    DrawPixel,
                    DrawFastVLine, DrawFastHLine,
                    FillRect, FillScreen,
                    DrawLine, DrawRect,
                    DrawCircle, FillCircle,
                    DrawTriangle, FillTriangle,
                    DrawRoundRect, FillRoundRect,
                    SetTextWrap,
                    DrawChar_Square, DrawChar_Rectangle,
                    SetTextSize_Square, SetTextSize_Rectangle,
                    SetTextColor0, SetTextColor1,
                    // Transaction parts
                    StartWrite, WritePixel, WriteFillRect, WriteFastVLine, WriteFastHLine, WriteLine, EndWrite,
                    // always last
                    Count,
                };

            }
            namespace {
                volatile void* drawRegisters[Operations::Count];

                void
                configure() noexcept {
                    // setup all of the registers as needed
                    for (int i = 0 ; i < Operations::Count; ++i) {
                        drawRegisters[i] = reinterpret_cast<volatile void*>(computeRegisterBasePointer<uint8_t>(0, Devices::Display, static_cast<uint8_t>(i), 0));
                    }
                }
            } // end namespace
            bool available() noexcept { return cortex::ChipsetBasicFunctions::available<Devices::Display>(); }
            void
            drawPixel(int16_t x, int16_t y, uint16_t color) noexcept {
                *reinterpret_cast<volatile uint64_t*>(drawRegisters[Operations::DrawPixel]) = makeLongOrdinal(x, y, color, 0);
            }
            void startWrite() noexcept {
                *reinterpret_cast<volatile uint8_t*>(drawRegisters[Operations::StartWrite]) = 0;
            }
            void
            writePixel(int16_t x, int16_t y, uint16_t color) noexcept {
                *reinterpret_cast<volatile uint64_t*>(drawRegisters[Operations::WritePixel]) = makeLongOrdinal(x, y, color, 0);
            }
            void
            writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = color;
                __builtin_i960_synmovq((void*)drawRegisters[Operations::WriteFillRect], args);
            }
            void
            writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
                *reinterpret_cast<volatile uint64_t*>(drawRegisters[Operations::WriteFastVLine]) = makeLongOrdinal(x, y, h, color);
            }
            void
            writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept {
                *reinterpret_cast<volatile uint64_t*>(drawRegisters[Operations::WriteFastHLine]) = makeLongOrdinal(x, y, w, color);
            }
            void
            writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = color;
                __builtin_i960_synmovq((void*)drawRegisters[Operations::WriteLine], args);
            }
            void endWrite() noexcept {
                *reinterpret_cast<volatile uint8_t*>(drawRegisters[Operations::EndWrite]) = 0;
            }
            uint16_t
            color565(uint8_t red, uint8_t green, uint8_t blue) noexcept {
                return (static_cast<uint16_t>(red & 0xF8) << 8) |
                       (static_cast<uint16_t>(green & 0xFC) << 3) | static_cast<uint16_t>(blue >> 3);
            }
        } // end namespace Display
        void
        begin() noexcept {
            Console::configure();
            Timer::configure();
            Info::configure();
            Display::configure();
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex