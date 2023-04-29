//
// Created by jwscoggins on 5/3/21.
//

#include <cortex/Types.h>
#include <cortex/IODevice.h>
#include <cortex/ChipsetInteract.h>
namespace cortex
{
    namespace Operations {
#define X(x) ((static_cast<uint32_t>(0xFE) << 24) | ((static_cast<uint32_t>(x) << 4) & 0x00FFFFF0))
        enum {
            Code_Info_GetCPUClockSpeed = 0,
            Code_Info_GetChipsetClockSpeed,
            Code_Info_GetExternalIAC,

            Code_Serial_RW = 0x10,
            Code_Serial_Flush,
            Code_Serial_Baud,

            Code_Timer_CompareValue = 0x20,
            Code_Timer_Prescalar,
            Code_Timer_Unixtime,

            // display opcodes begin
            Code_Display_RW = 0x30,
            Code_Display_Flush,
            Code_Display_WidthHeight,
            Code_Display_Rotation,
            Code_Display_InvertDisplay,
            Code_Display_ScrollTo,
            Code_Display_SetScrollMargins,
            Code_Display_SetAddressWindow,
            Code_Display_CursorX,
            Code_Display_CursorY,
            Code_Display_CursorXY,
            Code_Display_DrawPixel,
            Code_Display_DrawFastVLine,
            Code_Display_DrawFastHLine,
            Code_Display_FillRect,
            Code_Display_FillScreen,
            Code_Display_DrawLine,
            Code_Display_DrawRect,
            Code_Display_DrawCircle,
            Code_Display_FillCircle,
            Code_Display_DrawTriangle,
            Code_Display_FillTriangle,
            Code_Display_DrawRoundRect,
            Code_Display_FillRoundRect,
            Code_Display_SetTextWrap,
            Code_Display_DrawChar_Square,
            Code_Display_DrawChar_Rectangle,
            Code_Display_SetTextSize_Square,
            Code_Display_SetTextSize_Rectangle,
            Code_Display_SetTextColor0,
            Code_Display_SetTextColor1,
            Code_Display_StartWrite,
            Code_Display_WritePixel,
            Code_Display_WriteFillRect,
            Code_Display_WriteFastVLine,
            Code_Display_WriteFastHLine,
            Code_Display_WriteLine,
            Code_Display_EndWrite,

            Code_Display_ReadCommand8_0 = 0x100,
#define Z(index) Code_Display_ReadCommand8_ ## index
            Z(1),
            Z(2),
            Z(3),
            Z(4),
            Z(5),
            Z(6),
            Z(7),
            Z(8),
            Z(9),
            Z(10),
            Z(11),
            Z(12),
            Z(13),
            Z(14),
            Z(15),
            Z(16),
            Z(17),
            Z(18),
            Z(19),
            Z(20),
            Z(21),
            Z(22),
            Z(23),
            Z(24),
            Z(25),
            Z(26),
            Z(27),
            Z(28),
            Z(29),
            Z(30),
            Z(31),
            Z(32),
            Z(33),
            Z(34),
            Z(35),
            Z(36),
            Z(37),
            Z(38),
            Z(39),
            Z(40),
            Z(41),
            Z(42),
            Z(43),
            Z(44),
            Z(45),
            Z(46),
            Z(47),
            Z(48),
            Z(49),
            Z(50),
            Z(51),
            Z(52),
            Z(53),
            Z(54),
            Z(55),
            Z(56),
            Z(57),
            Z(58),
            Z(59),
            Z(60),
            Z(61),
            Z(62),
            Z(63),
            Z(64),
            Z(65),
            Z(66),
            Z(67),
            Z(68),
            Z(69),
            Z(70),
            Z(71),
            Z(72),
            Z(73),
            Z(74),
            Z(75),
            Z(76),
            Z(77),
            Z(78),
            Z(79),
            Z(80),
            Z(81),
            Z(82),
            Z(83),
            Z(84),
            Z(85),
            Z(86),
            Z(87),
            Z(88),
            Z(89),
            Z(90),
            Z(91),
            Z(92),
            Z(93),
            Z(94),
            Z(95),
            Z(96),
            Z(97),
            Z(98),
            Z(99),
            Z(100),
            Z(101),
            Z(102),
            Z(103),
            Z(104),
            Z(105),
            Z(106),
            Z(107),
            Z(108),
            Z(109),
            Z(110),
            Z(111),
            Z(112),
            Z(113),
            Z(114),
            Z(115),
            Z(116),
            Z(117),
            Z(118),
            Z(119),
            Z(120),
            Z(121),
            Z(122),
            Z(123),
            Z(124),
            Z(125),
            Z(126),
            Z(127),
            Z(129),
            Z(130),
            Z(131),
            Z(132),
            Z(133),
            Z(134),
            Z(135),
            Z(136),
            Z(137),
            Z(138),
            Z(139),
            Z(140),
            Z(141),
            Z(142),
            Z(143),
            Z(144),
            Z(145),
            Z(146),
            Z(147),
            Z(148),
            Z(149),
            Z(150),
            Z(151),
            Z(152),
            Z(153),
            Z(154),
            Z(155),
            Z(156),
            Z(157),
            Z(158),
            Z(159),
            Z(160),
            Z(161),
            Z(162),
            Z(163),
            Z(164),
            Z(165),
            Z(166),
            Z(167),
            Z(168),
            Z(169),
            Z(170),
            Z(171),
            Z(172),
            Z(173),
            Z(174),
            Z(175),
            Z(176),
            Z(177),
            Z(178),
            Z(179),
            Z(180),
            Z(181),
            Z(182),
            Z(183),
            Z(184),
            Z(185),
            Z(186),
            Z(187),
            Z(188),
            Z(189),
            Z(190),
            Z(191),
            Z(192),
            Z(193),
            Z(194),
            Z(195),
            Z(196),
            Z(197),
            Z(198),
            Z(199),
            Z(200),
            Z(201),
            Z(202),
            Z(203),
            Z(204),
            Z(205),
            Z(206),
            Z(207),
            Z(208),
            Z(209),
            Z(210),
            Z(211),
            Z(212),
            Z(213),
            Z(214),
            Z(215),
            Z(216),
            Z(217),
            Z(218),
            Z(219),
            Z(220),
            Z(221),
            Z(222),
            Z(223),
            Z(224),
            Z(225),
            Z(226),
            Z(227),
            Z(228),
            Z(229),
            Z(230),
            Z(231),
            Z(232),
            Z(233),
            Z(234),
            Z(235),
            Z(236),
            Z(237),
            Z(238),
            Z(239),
            Z(240),
            Z(241),
            Z(242),
            Z(243),
            Z(244),
            Z(245),
            Z(246),
            Z(247),
            Z(248),
            Z(249),
            Z(250),
            Z(251),
            Z(252),
            Z(253),
            Z(254),
            Z(255),
#undef Z


#define Y(opcode) opcode = X( Code_ ## opcode )
            Y(Info_GetCPUClockSpeed),
            Y(Info_GetChipsetClockSpeed),
            Y(Info_GetExternalIAC),
            // serial operations begin
            Y(Serial_RW),
            Y(Serial_Flush),
            Y(Serial_Baud),
            // timer operations begin
            Y(Timer_CompareValue),
            Y(Timer_Prescalar),
            Y(Timer_Unixtime),
            // Display Operations begin
            Y(Display_RW),
            Y(Display_Flush),
            Y(Display_WidthHeight),
            Y(Display_Rotation),
            Y(Display_InvertDisplay),
            Y(Display_ScrollTo),
            Y(Display_SetScrollMargins),
            Y(Display_SetAddressWindow),
            Y(Display_CursorX),
            Y(Display_CursorY),
            Y(Display_CursorXY),
            Y(Display_DrawPixel),
            Y(Display_DrawFastVLine),
            Y(Display_DrawFastHLine),
            Y(Display_FillRect),
            Y(Display_FillScreen),
            Y(Display_DrawLine),
            Y(Display_DrawRect),
            Y(Display_DrawCircle),
            Y(Display_FillCircle),
            Y(Display_DrawTriangle),
            Y(Display_FillTriangle),
            Y(Display_DrawRoundRect),
            Y(Display_FillRoundRect),
            Y(Display_SetTextWrap),
            Y(Display_DrawChar_Square),
            Y(Display_DrawChar_Rectangle),
            Y(Display_SetTextSize_Square),
            Y(Display_SetTextSize_Rectangle),
            Y(Display_SetTextColor0),
            Y(Display_SetTextColor1),
            Y(Display_StartWrite),
            Y(Display_WritePixel),
            Y(Display_WriteFillRect),
            Y(Display_WriteFastVLine),
            Y(Display_WriteFastHLine),
            Y(Display_WriteLine),
            Y(Display_EndWrite),
            Y(Display_ReadCommand8_0),
            Y(Display_ReadCommand8_1),
            Y(Display_ReadCommand8_2),
            Y(Display_ReadCommand8_3),
            Y(Display_ReadCommand8_4),
            Y(Display_ReadCommand8_5),
            Y(Display_ReadCommand8_6),
            Y(Display_ReadCommand8_7),
            Y(Display_ReadCommand8_8),
            Y(Display_ReadCommand8_9),
            Y(Display_ReadCommand8_10),
            Y(Display_ReadCommand8_11),
            Y(Display_ReadCommand8_12),
            Y(Display_ReadCommand8_13),
            Y(Display_ReadCommand8_14),
            Y(Display_ReadCommand8_15),
            Y(Display_ReadCommand8_16),
            Y(Display_ReadCommand8_17),
            Y(Display_ReadCommand8_18),
            Y(Display_ReadCommand8_19),
            Y(Display_ReadCommand8_20),
            Y(Display_ReadCommand8_21),
            Y(Display_ReadCommand8_22),
            Y(Display_ReadCommand8_23),
            Y(Display_ReadCommand8_24),
            Y(Display_ReadCommand8_25),
            Y(Display_ReadCommand8_26),
            Y(Display_ReadCommand8_27),
            Y(Display_ReadCommand8_28),
            Y(Display_ReadCommand8_29),
            Y(Display_ReadCommand8_30),
            Y(Display_ReadCommand8_31),
            Y(Display_ReadCommand8_32),
            Y(Display_ReadCommand8_33),
            Y(Display_ReadCommand8_34),
            Y(Display_ReadCommand8_35),
            Y(Display_ReadCommand8_36),
            Y(Display_ReadCommand8_37),
            Y(Display_ReadCommand8_38),
            Y(Display_ReadCommand8_39),
            Y(Display_ReadCommand8_40),
            Y(Display_ReadCommand8_41),
            Y(Display_ReadCommand8_42),
            Y(Display_ReadCommand8_43),
            Y(Display_ReadCommand8_44),
            Y(Display_ReadCommand8_45),
            Y(Display_ReadCommand8_46),
            Y(Display_ReadCommand8_47),
            Y(Display_ReadCommand8_48),
            Y(Display_ReadCommand8_49),
            Y(Display_ReadCommand8_50),
            Y(Display_ReadCommand8_51),
            Y(Display_ReadCommand8_52),
            Y(Display_ReadCommand8_53),
            Y(Display_ReadCommand8_54),
            Y(Display_ReadCommand8_55),
            Y(Display_ReadCommand8_56),
            Y(Display_ReadCommand8_57),
            Y(Display_ReadCommand8_58),
            Y(Display_ReadCommand8_59),
            Y(Display_ReadCommand8_60),
            Y(Display_ReadCommand8_61),
            Y(Display_ReadCommand8_62),
            Y(Display_ReadCommand8_63),
            Y(Display_ReadCommand8_64),
            Y(Display_ReadCommand8_65),
            Y(Display_ReadCommand8_66),
            Y(Display_ReadCommand8_67),
            Y(Display_ReadCommand8_68),
            Y(Display_ReadCommand8_69),
            Y(Display_ReadCommand8_70),
            Y(Display_ReadCommand8_71),
            Y(Display_ReadCommand8_72),
            Y(Display_ReadCommand8_73),
            Y(Display_ReadCommand8_74),
            Y(Display_ReadCommand8_75),
            Y(Display_ReadCommand8_76),
            Y(Display_ReadCommand8_77),
            Y(Display_ReadCommand8_78),
            Y(Display_ReadCommand8_79),
            Y(Display_ReadCommand8_80),
            Y(Display_ReadCommand8_81),
            Y(Display_ReadCommand8_82),
            Y(Display_ReadCommand8_83),
            Y(Display_ReadCommand8_84),
            Y(Display_ReadCommand8_85),
            Y(Display_ReadCommand8_86),
            Y(Display_ReadCommand8_87),
            Y(Display_ReadCommand8_88),
            Y(Display_ReadCommand8_89),
            Y(Display_ReadCommand8_90),
            Y(Display_ReadCommand8_91),
            Y(Display_ReadCommand8_92),
            Y(Display_ReadCommand8_93),
            Y(Display_ReadCommand8_94),
            Y(Display_ReadCommand8_95),
            Y(Display_ReadCommand8_96),
            Y(Display_ReadCommand8_97),
            Y(Display_ReadCommand8_98),
            Y(Display_ReadCommand8_99),
            Y(Display_ReadCommand8_100),
            Y(Display_ReadCommand8_101),
            Y(Display_ReadCommand8_102),
            Y(Display_ReadCommand8_103),
            Y(Display_ReadCommand8_104),
            Y(Display_ReadCommand8_105),
            Y(Display_ReadCommand8_106),
            Y(Display_ReadCommand8_107),
            Y(Display_ReadCommand8_108),
            Y(Display_ReadCommand8_109),
            Y(Display_ReadCommand8_110),
            Y(Display_ReadCommand8_111),
            Y(Display_ReadCommand8_112),
            Y(Display_ReadCommand8_113),
            Y(Display_ReadCommand8_114),
            Y(Display_ReadCommand8_115),
            Y(Display_ReadCommand8_116),
            Y(Display_ReadCommand8_117),
            Y(Display_ReadCommand8_118),
            Y(Display_ReadCommand8_119),
            Y(Display_ReadCommand8_120),
            Y(Display_ReadCommand8_121),
            Y(Display_ReadCommand8_122),
            Y(Display_ReadCommand8_123),
            Y(Display_ReadCommand8_124),
            Y(Display_ReadCommand8_125),
            Y(Display_ReadCommand8_126),
            Y(Display_ReadCommand8_127),
            Y(Display_ReadCommand8_129),
            Y(Display_ReadCommand8_130),
            Y(Display_ReadCommand8_131),
            Y(Display_ReadCommand8_132),
            Y(Display_ReadCommand8_133),
            Y(Display_ReadCommand8_134),
            Y(Display_ReadCommand8_135),
            Y(Display_ReadCommand8_136),
            Y(Display_ReadCommand8_137),
            Y(Display_ReadCommand8_138),
            Y(Display_ReadCommand8_139),
            Y(Display_ReadCommand8_140),
            Y(Display_ReadCommand8_141),
            Y(Display_ReadCommand8_142),
            Y(Display_ReadCommand8_143),
            Y(Display_ReadCommand8_144),
            Y(Display_ReadCommand8_145),
            Y(Display_ReadCommand8_146),
            Y(Display_ReadCommand8_147),
            Y(Display_ReadCommand8_148),
            Y(Display_ReadCommand8_149),
            Y(Display_ReadCommand8_150),
            Y(Display_ReadCommand8_151),
            Y(Display_ReadCommand8_152),
            Y(Display_ReadCommand8_153),
            Y(Display_ReadCommand8_154),
            Y(Display_ReadCommand8_155),
            Y(Display_ReadCommand8_156),
            Y(Display_ReadCommand8_157),
            Y(Display_ReadCommand8_158),
            Y(Display_ReadCommand8_159),
            Y(Display_ReadCommand8_160),
            Y(Display_ReadCommand8_161),
            Y(Display_ReadCommand8_162),
            Y(Display_ReadCommand8_163),
            Y(Display_ReadCommand8_164),
            Y(Display_ReadCommand8_165),
            Y(Display_ReadCommand8_166),
            Y(Display_ReadCommand8_167),
            Y(Display_ReadCommand8_168),
            Y(Display_ReadCommand8_169),
            Y(Display_ReadCommand8_170),
            Y(Display_ReadCommand8_171),
            Y(Display_ReadCommand8_172),
            Y(Display_ReadCommand8_173),
            Y(Display_ReadCommand8_174),
            Y(Display_ReadCommand8_175),
            Y(Display_ReadCommand8_176),
            Y(Display_ReadCommand8_177),
            Y(Display_ReadCommand8_178),
            Y(Display_ReadCommand8_179),
            Y(Display_ReadCommand8_180),
            Y(Display_ReadCommand8_181),
            Y(Display_ReadCommand8_182),
            Y(Display_ReadCommand8_183),
            Y(Display_ReadCommand8_184),
            Y(Display_ReadCommand8_185),
            Y(Display_ReadCommand8_186),
            Y(Display_ReadCommand8_187),
            Y(Display_ReadCommand8_188),
            Y(Display_ReadCommand8_189),
            Y(Display_ReadCommand8_190),
            Y(Display_ReadCommand8_191),
            Y(Display_ReadCommand8_192),
            Y(Display_ReadCommand8_193),
            Y(Display_ReadCommand8_194),
            Y(Display_ReadCommand8_195),
            Y(Display_ReadCommand8_196),
            Y(Display_ReadCommand8_197),
            Y(Display_ReadCommand8_198),
            Y(Display_ReadCommand8_199),
            Y(Display_ReadCommand8_200),
            Y(Display_ReadCommand8_201),
            Y(Display_ReadCommand8_202),
            Y(Display_ReadCommand8_203),
            Y(Display_ReadCommand8_204),
            Y(Display_ReadCommand8_205),
            Y(Display_ReadCommand8_206),
            Y(Display_ReadCommand8_207),
            Y(Display_ReadCommand8_208),
            Y(Display_ReadCommand8_209),
            Y(Display_ReadCommand8_210),
            Y(Display_ReadCommand8_211),
            Y(Display_ReadCommand8_212),
            Y(Display_ReadCommand8_213),
            Y(Display_ReadCommand8_214),
            Y(Display_ReadCommand8_215),
            Y(Display_ReadCommand8_216),
            Y(Display_ReadCommand8_217),
            Y(Display_ReadCommand8_218),
            Y(Display_ReadCommand8_219),
            Y(Display_ReadCommand8_220),
            Y(Display_ReadCommand8_221),
            Y(Display_ReadCommand8_222),
            Y(Display_ReadCommand8_223),
            Y(Display_ReadCommand8_224),
            Y(Display_ReadCommand8_225),
            Y(Display_ReadCommand8_226),
            Y(Display_ReadCommand8_227),
            Y(Display_ReadCommand8_228),
            Y(Display_ReadCommand8_229),
            Y(Display_ReadCommand8_230),
            Y(Display_ReadCommand8_231),
            Y(Display_ReadCommand8_232),
            Y(Display_ReadCommand8_233),
            Y(Display_ReadCommand8_234),
            Y(Display_ReadCommand8_235),
            Y(Display_ReadCommand8_236),
            Y(Display_ReadCommand8_237),
            Y(Display_ReadCommand8_238),
            Y(Display_ReadCommand8_239),
            Y(Display_ReadCommand8_240),
            Y(Display_ReadCommand8_241),
            Y(Display_ReadCommand8_242),
            Y(Display_ReadCommand8_243),
            Y(Display_ReadCommand8_244),
            Y(Display_ReadCommand8_245),
            Y(Display_ReadCommand8_246),
            Y(Display_ReadCommand8_247),
            Y(Display_ReadCommand8_248),
            Y(Display_ReadCommand8_249),
            Y(Display_ReadCommand8_250),
            Y(Display_ReadCommand8_251),
            Y(Display_ReadCommand8_252),
            Y(Display_ReadCommand8_253),
            Y(Display_ReadCommand8_254),
            Y(Display_ReadCommand8_255),

            Display_ReadCommand8_RegistersBase = Display_ReadCommand8_0,
        };
#undef Y
#undef X
    } // end namespace Operations
    namespace ChipsetBasicFunctions {

        namespace Devices {
            enum {
                Info = 0,
                Serial,
                Timer,
                Display,
            };
        }

        namespace Console {

            uint16_t
            read() {
                return memory<uint16_t>(Operations::Serial_RW);
            }

            void
            write(uint16_t c) {
                memory<uint16_t>(Operations::Serial_RW) = c;
            }

            void
            write(char c) {
                write(static_cast<uint16_t>(c));
            }
            void
            flush() {
                memory<uint8_t>(Operations::Serial_Flush) = 0;
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
            uint32_t
            unixtime() noexcept {
                return 0;
            }
            void
            setCompareValue(uint16_t value) noexcept {
                memory<uint16_t>(Operations::Timer_CompareValue) = value;
            }

            uint16_t
            getCompareValue() noexcept {
                return memory<uint16_t>(Operations::Timer_CompareValue);
            }
            void
            setPrescalar(uint8_t value) noexcept {
                memory<uint8_t>(Operations::Timer_Prescalar) = value;
            }
            uint8_t
            getPrescalar() noexcept {
                return memory<uint8_t>(Operations::Timer_Prescalar);
            }
        } // end namespace RTC
        namespace Info {
            uint32_t
            getCPUClockSpeed() noexcept {
                return memory<uint32_t>(Operations::Info_GetCPUClockSpeed);
            }
            uint32_t
            getChipsetClockSpeed() noexcept {
                return memory<uint32_t>(Operations::Info_GetChipsetClockSpeed);
            }
            IACMessage*
            getExternalMessage() noexcept {
                return reinterpret_cast<IACMessage*>(Operations::Info_GetExternalIAC);
            }
        }
        namespace Display {
            void flush() noexcept { memory<uint8_t>(Operations::Display_Flush) = 0; }
            void write(uint8_t value) noexcept { memory<uint8_t>(Operations::Display_RW) = value; }
            void drawPixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_DrawPixel) = makeLongOrdinal(x, y, color, 0);
            }
            void startWrite() noexcept {
                memory<uint8_t>(Operations::Display_StartWrite) = 0;
            }
            void
            writePixel(int16_t x, int16_t y, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WritePixel) = makeLongOrdinal(x, y, color, 0);
            }
            void
            writeFillRect(int16_t x, int16_t y, int16_t w, int16_t h, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::Display_WriteFillRect, args);
            }
            void
            writeFastVLine(int16_t x, int16_t y, int16_t h, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WriteFastVLine) = makeLongOrdinal(x, y, h, color);
            }
            void
            writeFastHLine(int16_t x, int16_t y, int16_t w, uint16_t color) noexcept {
                memory<uint64_t>(Operations::Display_WriteFastHLine) = makeLongOrdinal(x, y, w, color);
            }
            void
            writeLine(int16_t x0, int16_t y0, int16_t x1, int16_t y1, uint16_t color) noexcept {
#if 1
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = color;
                __builtin_i960_synmovq((void*)Operations::Display_WriteLine, args);
#else
                // this is a port of the Adafruit_GFX routine over to see if we can't accelerate things
                int steep = abs(y1 - y0) > abs(x1 - x0);
                if (steep) {
                    int tmp = x0;
                    x0 = y0;
                    y0 = tmp;
                    tmp = x1;
                    x1 = y1;
                    y1 = tmp;
                }
                if (x0 > x1) {
                    int tmp = x0;
                    x0 = x1;
                    x1 = tmp;
                    tmp = y0;
                    y0 = y1;
                    y1 = tmp;
                }
                int dx = x1 - x0;
                int dy = abs(y1 - y0);
                int err = dx / 2;
                int ystep = 0;
                if (y0 < y1) {
                    ystep = 1;
                } else {
                    ystep = -1;
                }
                for (; x0 <= x1; ++x0) {
                    if (steep) {
                        writePixel(y0, x0, color);
                    } else {
                        writePixel(x0, y0, color);
                    }
                    err -= dy;
                    if (err < 0) {
                        y0 += ystep;
                        err += dx;
                    }
                }
#endif
            }
            void endWrite() noexcept {
                memory<uint8_t>(Operations::Display_EndWrite) = 0;
            }
            uint16_t
            color565(uint8_t red, uint8_t green, uint8_t blue) noexcept {
                return (static_cast<uint16_t>(red & 0xF8) << 8) |
                       (static_cast<uint16_t>(green & 0xFC) << 3) | static_cast<uint16_t>(blue >> 3);
            }
            uint32_t getDisplayWidthHeight() noexcept {
                return memory<uint32_t>(Operations::Display_WidthHeight);
            }
            uint16_t getDisplayHeight() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight() >> 16); }
            uint16_t getDisplayWidth() noexcept { return static_cast<uint16_t>(getDisplayWidthHeight()); }
            uint8_t getRotation() noexcept { return memory<uint8_t>(Operations::Display_Rotation); }
            void setRotation(uint8_t value) noexcept { memory<uint8_t>(Operations::Display_Rotation) = value; }
            void setAddressWindow(uint16_t a, uint16_t b, uint16_t c, uint16_t d) noexcept { memory<uint64_t>(Operations::Display_SetAddressWindow) = makeLongOrdinal(a, b, c, d); }
            void invertDisplay(bool value) noexcept {
                memory<uint8_t>(Operations::Display_InvertDisplay) = value ? 0xFF : 0;
            }
            void setScrollMargins(uint16_t a, uint16_t b) noexcept {
                memory<uint32_t>(Operations::Display_SetScrollMargins) = makeOrdinal(a, b);
            }
            void scrollTo(uint16_t a) noexcept {
                memory<uint16_t>(Operations::Display_ScrollTo) = a;
            }
            uint8_t
            getDisplayRegister(uint8_t index) noexcept {
                return memory<uint8_t[256]>(Operations::Display_ReadCommand8_RegistersBase)[index];
            }
            void
            fillScreen(uint16_t color) noexcept {
                // this puts strain on the chipset instead of having the i960 handle it since it is doing it anyway
                memory<uint16_t>(Operations::Display_FillScreen) = color;
            }
            void
            drawRoundRect(int16_t x,
                               int16_t y,
                               int16_t w,
                               int16_t h,
                               int16_t r,
                               uint16_t color) noexcept {
                // have the AVR do the computation
                static uint16_t args[8] = { 0 };
                args[0] = x;
                args[1] = y;
                args[2] = w;
                args[3] = h;
                args[4] = r;
                args[5] = color;
                __builtin_i960_synmovq((void*)Operations::Display_DrawRoundRect, args);
            }
            void drawTriangle(int16_t x0, int16_t y0, int16_t x1, int16_t y1, int16_t x2, int16_t y2, uint16_t color) noexcept {
                static uint16_t args[8] = { 0 };
                args[0] = x0;
                args[1] = y0;
                args[2] = x1;
                args[3] = y1;
                args[4] = x2;
                args[5] = y2;
                args[6] = color;
                __builtin_i960_synmovq((void*)Operations::Display_DrawTriangle, args);
            }
        } // end namespace Display
        void
        begin() noexcept {
        }
    } // end namespace ChipsetBasicFunctions
} // end namespace cortex