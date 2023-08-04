cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.1)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 98)

set(tools /home/jwscoggins/dev/i960/i960-elf-gcc-3.4.6_v4)
set(CMAKE_C_COMPILER ${tools}/bin/i960-elf-gcc)
set(CMAKE_CXX_COMPILER ${tools}/bin/i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER ${tools}/bin/i960-elf-as)
set(CMAKE_LINKER ${tools}/bin/i960-elf-ld)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -O2 -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -fno-exceptions")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -O2 -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 --std=gnu99")
set(CMAKE_OBJCOPY_COMPILER ${tools}/bin/i960-elf-objcopy)
set(CMAKE_OBJDUMP_COMPILER ${tools}/bin/i960-elf-objdump)
# gcc 3.4.6 doesn't support colorizing output
unset(CMAKE_COLOR_DIAGNOSTICS CACHE)
add_compile_options(-Wall)
# for some reason, the compiler generated multiple copies of the same entries
#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmc -mnumerics")
set(CMAKE_C_FLAGS "${CMAKE_CXX_FLAGS} -mmc -mnumerics")
set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -AMC -I${CMAKE_SOURCE_DIR}/boot")
#add_link_options(-mmc -mnumerics)
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -msb -mnumerics -ffreestanding")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
add_library(hitagisys
		# disabled entries are because mon960 provides the implementations for them
		#sys/brk.cc
		#sys/fstat.cc
		#sys/isatty.cc
		sys/lseek.cc
		sys/write.cc
		sys/read.cc
		sys/open.cc
		#sys/kill.cc
		sys/setitimer.cc
		sys/close.cc
		sys/gettimeofday.cc
		sys/access.cc
		#sys/getpid.cc
		sys/_exit.cc
		sys/link.cc
		sys/unlink.cc sys/linkage.cc sys/getrusage.cc)
add_library(hitagicortex
		cortex/Faults.def
		cortex/IODevice.cc
		cortex/IODevice.h
		cortex/ChipsetInteract.h
		cortex/Faults.cc
		cortex/Faults.h
		cortex/IAC.h
		cortex/IAC.cc
		cortex/IACImplementation.s
		cortex/ModernCpp.h
		cortex/Types.h cortex/SystemCounter.h cortex/SystemCounter.cc cortex/ModernGCC.h cortex/ModernGCC.cc cortex/builtins.h cortex/builtins.c cortex/IndirectInterrupts.cc)
add_executable(hitagimon
		boot/sx_init.s # this must ALWAYS be first!
		boot/f_table.s
		boot/i_handle.s
		boot/i_table.s
		boot/f_handle.cc
		boot/macros.s
		hitagimain.cc
		tinyscheme-1.42/scheme-private.h
		tinyscheme-1.42/scheme.h
		tinyscheme-1.42/scheme.c
		tinyscheme-1.42/opdefines.h
		)
add_executable(hitagiwidebootloader
		wide/bootloader.s
)

add_library(hitagiemul

		emulation/m6502.cc emulation/m6502.h emulation/Segment.cc emulation/Segment.h emulation/Domain.cc emulation/Domain.h)

target_include_directories(hitagiemul PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_include_directories(hitagicortex PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hitagisys PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hitagimon PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(hitagimon
		gcc
		c
		m
		hitagicortex
		hitagisys
		hitagiemul
		mon960
		stdc++
		c
		m
		gcc # necessary for soft float support, will need to provide my own custom routines ahead of this point to access the fpu
		mon960
		hitagicortex
		)

#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
target_link_options(hitagimon
		PRIVATE -Wl,--as-needed
		PRIVATE -nostdlib
		PRIVATE -static
		PRIVATE -T ${CMAKE_SOURCE_DIR}/start.ld
		PRIVATE -ffreestanding
		PRIVATE -Wl,--allow-multiple-definition
		PRIVATE -mmc
		PRIVATE -mnumerics
)

target_link_options(hitagiwidebootloader
		PRIVATE -T ${CMAKE_SOURCE_DIR}/wide/start.ld
)

add_custom_command(
		OUTPUT firmware.bin
		COMMAND ${CMAKE_OBJCOPY_COMPILER} --pad-to=0x01100000 --only-section=.text --only-section=.data --only-section=.bss -O binary hitagimon firmware.bin
		MAIN_DEPENDENCY hitagimon
)

add_custom_target(hitagimon_firmware_narrow DEPENDS firmware.bin)