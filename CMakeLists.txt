cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.2)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_C_COMPILER i960-elf-gcc)
set(CMAKE_CXX_COMPILER i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER i960-elf-as)
set(CMAKE_LINKER i960-elf-ld)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DUSH_CONFIG_PLATFORM_POSIX -fno-exceptions -fno-use-cxa-atexit -DLUA_32BITS")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DCONFIG_PLATFORM_POSIX --std=gnu99 -DLUA_32BITS")
set(CMAKE_OBJCOPY_COMPILER i960-elf-objcopy)
set(CMAKE_OBJDUMP_COMPILER i960-elf-objdump)
# gcc 3.4.6 doesn't support colorizing output
unset(CMAKE_COLOR_DIAGNOSTICS CACHE)
#add_compile_options(-Wall)
# for some reason, the compiler generated multiple copies of the same entries
#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
# disable memset optimization
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmc -fno-builtin")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mmc -fno-builtin")
set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -AMC -I${CMAKE_SOURCE_DIR}/boot")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmc -msoft-float -O2")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mmc -msoft-float -O2")
#set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -AKA -I${CMAKE_SOURCE_DIR}/boot")
#add_link_options(-mmc -mnumerics)
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -msb -mnumerics -ffreestanding")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
add_library(coremark
	coremark/core_list_join.c
	coremark/core_main.c
	coremark/coremark.h
	coremark/core_matrix.c
	coremark/core_portme.c
	coremark/core_portme.h
	coremark/core_state.c
	coremark/core_util.c
)
add_library(features
	features/disassembly.h
	features/disassembly.cc
	features/opcodes.def
)
add_library(hitagisys
		# disabled entries are because mon960 provides the implementations for them
		sys/setjmp.s
        sys/brk.cc
        sys/kill.cc
        sys/getpid.cc
		sys/_exit.cc
        sys/times.cc
        sys/time.cc
        sys/filesystem.cc)
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
		cortex/Types.h
        cortex/SystemCounter.h cortex/SystemCounter.cc cortex/ModernGCC.h cortex/ModernGCC.cc cortex/builtins.h cortex/builtins.c cortex/IndirectInterrupts.cc
        cortex/SoftwareDMA.h
		cortex/SoftwareDMA.cc
		cortex/SysExamine.cc
        cortex/builtins.s
        cortex/FilesystemInterface.h
        cortex/FilesystemInterface.cc
)
add_library(hitagibench
	benchmarks/benchmarks.h
    benchmarks/mandlebrot.cc
	benchmarks/move0.s
	benchmarks/transcendental.s
	benchmarks/nullTime.s
	benchmarks/loadStore.s
    benchmarks/indirect_function_call_tests.cc
)
add_library(tinyscheme
    tinyscheme/scheme.c
    tinyscheme/scheme.h
    tinyscheme/scheme-private.h
    tinyscheme/opdefines.h
    )
add_library(lua
lua-5.5.0/src/lapi.c
lua-5.5.0/src/lapi.h
lua-5.5.0/src/lauxlib.c
lua-5.5.0/src/lauxlib.h
lua-5.5.0/src/lbaselib.c
lua-5.5.0/src/lcode.c
lua-5.5.0/src/lcode.h
lua-5.5.0/src/lcorolib.c
lua-5.5.0/src/lctype.c
lua-5.5.0/src/lctype.h
lua-5.5.0/src/ldblib.c
lua-5.5.0/src/ldebug.c
lua-5.5.0/src/ldebug.h
lua-5.5.0/src/ldo.c
lua-5.5.0/src/ldo.h
lua-5.5.0/src/ldump.c
lua-5.5.0/src/lfunc.c
lua-5.5.0/src/lfunc.h
lua-5.5.0/src/lgc.c
lua-5.5.0/src/lgc.h
lua-5.5.0/src/linit.c
lua-5.5.0/src/liolib.c
lua-5.5.0/src/ljumptab.h
lua-5.5.0/src/llex.c
lua-5.5.0/src/llex.h
lua-5.5.0/src/llimits.h
lua-5.5.0/src/lmathlib.c
lua-5.5.0/src/lmem.c
lua-5.5.0/src/lmem.h
lua-5.5.0/src/loadlib.c
lua-5.5.0/src/lobject.c
lua-5.5.0/src/lobject.h
lua-5.5.0/src/lopcodes.c
lua-5.5.0/src/lopcodes.h
lua-5.5.0/src/lopnames.h
lua-5.5.0/src/loslib.c
lua-5.5.0/src/lparser.c
lua-5.5.0/src/lparser.h
lua-5.5.0/src/lprefix.h
lua-5.5.0/src/lstate.c
lua-5.5.0/src/lstate.h
lua-5.5.0/src/lstring.c
lua-5.5.0/src/lstring.h
lua-5.5.0/src/lstrlib.c
lua-5.5.0/src/ltable.c
lua-5.5.0/src/ltable.h
lua-5.5.0/src/ltablib.c
lua-5.5.0/src/ltm.c
lua-5.5.0/src/ltm.h
lua-5.5.0/src/lua.c
lua-5.5.0/src/luac.c
lua-5.5.0/src/luaconf.h
lua-5.5.0/src/lua.h
lua-5.5.0/src/lua.hpp
lua-5.5.0/src/lualib.h
lua-5.5.0/src/lundump.c
lua-5.5.0/src/lundump.h
lua-5.5.0/src/lutf8lib.c
lua-5.5.0/src/lvm.c
lua-5.5.0/src/lvm.h
lua-5.5.0/src/lzio.c
lua-5.5.0/src/lzio.h
)
add_executable(hitagimon
		boot/sx_init.s # this must ALWAYS be first!
		boot/f_table.s
		boot/i_handle.s
		boot/i_table.s
		boot/f_handle.cc
		boot/macros.s
		hitagimain.cc
		start.ld
)


# Make assembly files depend on macros.s so they get reassembled when macros.s changes
set_source_files_properties(
    boot/sx_init.s
    boot/f_table.s
    boot/i_handle.s
    boot/i_table.s
    PROPERTIES
    OBJECT_DEPENDS ${CMAKE_SOURCE_DIR}/boot/macros.s
)

add_subdirectory(microshell/src)
build_ush(microshell)
target_compile_definitions(
        microshell
        PUBLIC
        USH_CONFIG_PLATFORM_POSIX
)
set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/start.ld)
add_library(hitagiemul
        emulation/Segment.cc emulation/Segment.h emulation/Domain.cc emulation/Domain.h)

target_include_directories(hitagiemul PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_include_directories(hitagicortex PUBLIC 
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(hitagisys PUBLIC 
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(hitagimon PUBLIC 
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/lua-5.5.0/src
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(hitagibench PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(coremark PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(features PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(hitagimon
		hitagisys
		hitagicortex
		hitagisys
		hitagiemul
		hitagibench
        tinyscheme
        microshell
		lua
		coremark
		hitagisys
        gcc
        m
        c
        stdc++
		)

#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
target_link_options(hitagimon
		PRIVATE -Wl,--as-needed
		PRIVATE -nostdlib
		PRIVATE -static
		PRIVATE -T ${CMAKE_SOURCE_DIR}/start.ld
		PRIVATE -ffreestanding
		#PRIVATE -Wl,--allow-multiple-definition
        PRIVATE -fno-use-cxa-atexit
        PRIVATE -fno-exceptions
        #PRIVATE -msoft-float
		#PRIVATE -mmc
		#PRIVATE -mnumerics
)
add_custom_command(
		OUTPUT prog.bin
		COMMAND ${CMAKE_OBJCOPY_COMPILER} --pad-to=0x00100000 
        --only-section=.text 
        --only-section=.bss 
        --only-section=.data 
        --only-section=.init_array 
        --only-section=.fini_array 
        --gap-fill=0x00 --set-section-flags .bss=alloc,load,contents -O binary hitagimon prog.bin
		DEPENDS hitagimon
		#MAIN_DEPENDENCY hitagimon
)
add_custom_target(hitagimon_sd_image
		DEPENDS prog.bin
)
#add_custom_command(
#		OUTPUT data.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --pad-to=0xFE900000 --only-section=.bss --only-section=.data --gap-fill=0x00 --set-section-flags .bss=alloc,load,contents -O binary hitagimon data.bin
#		MAIN_DEPENDENCY
#			hitagimon
#)
#
#add_custom_command(
#		OUTPUT code.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --pad-to=0x00200000 --only-section=.text -O binary hitagimon code.bin
#		MAIN_DEPENDENCY
#		hitagimon
#)
#
#
#
#add_custom_target(hitagimon_firmware
#		DEPENDS
#		code.bin
#		data.bin
#)
#
#
#add_custom_command(
#		OUTPUT flash_firmware_p0.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --interleave-width=1 -b 0 -i 4 -I binary -O binary code.bin flash_firmware_p0.bin
#		DEPENDS hitagimon_firmware
#		MAIN_DEPENDENCY code.bin
#)
#
#add_custom_command(
#		OUTPUT flash_firmware_p1.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --interleave-width=1 -b 1 -i 4 -I binary -O binary code.bin flash_firmware_p1.bin
#		DEPENDS hitagimon_firmware
#		MAIN_DEPENDENCY code.bin
#)
#
#add_custom_command(
#		OUTPUT flash_firmware_p2.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --interleave-width=1 -b 2 -i 4 -I binary -O binary code.bin flash_firmware_p2.bin
#		DEPENDS hitagimon_firmware
#		MAIN_DEPENDENCY code.bin
#)
#
#add_custom_command(
#		OUTPUT flash_firmware_p3.bin
#		COMMAND ${CMAKE_OBJCOPY_COMPILER} --interleave-width=1 -b 3 -i 4 -I binary -O binary code.bin flash_firmware_p3.bin
#		DEPENDS hitagimon_firmware
#		MAIN_DEPENDENCY code.bin
#)
#
#add_custom_target(hitagimon_wide_mode_parts
#		DEPENDS
#		flash_firmware_p0.bin
#		flash_firmware_p1.bin
#		flash_firmware_p2.bin
#		flash_firmware_p3.bin
#)
#
#set(TARGET_FLASH_CHIP SST39SF040@PLCC32)
#add_custom_command(
#		OUTPUT uploaded_flash_firmware0
#		COMMAND minipro -p ${TARGET_FLASH_CHIP} --write flash_firmware_p0.bin
#		DEPENDS hitagimon_wide_mode_parts
#		MAIN_DEPENDENCY flash_firmware_p0.bin
#)
#
#add_custom_target(install_wide_flash0
#		DEPENDS
#		uploaded_flash_firmware0
#)
#
#add_custom_command(
#		OUTPUT uploaded_flash_firmware1
#		COMMAND minipro -p ${TARGET_FLASH_CHIP} --write flash_firmware_p1.bin
#		DEPENDS hitagimon_wide_mode_parts
#		MAIN_DEPENDENCY flash_firmware_p1.bin
#)
#
#add_custom_target(install_wide_flash1
#		DEPENDS
#		uploaded_flash_firmware1
#)
#add_custom_command(
#		OUTPUT uploaded_flash_firmware2
#		COMMAND minipro -p ${TARGET_FLASH_CHIP} --write flash_firmware_p2.bin
#		DEPENDS hitagimon_wide_mode_parts
#		MAIN_DEPENDENCY flash_firmware_p2.bin
#)
#
#add_custom_target(install_wide_flash2
#		DEPENDS
#		uploaded_flash_firmware2
#)
#add_custom_command(
#		OUTPUT uploaded_flash_firmware3
#		COMMAND minipro -p ${TARGET_FLASH_CHIP} --write flash_firmware_p3.bin
#		DEPENDS hitagimon_wide_mode_parts
#		MAIN_DEPENDENCY flash_firmware_p3.bin
#)
#
#add_custom_target(install_wide_flash3
#		DEPENDS
#		uploaded_flash_firmware3
#)
