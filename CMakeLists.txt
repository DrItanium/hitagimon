cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.2)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 98)

set(tools /home/jwscoggins/dev/i960/i960-elf-gcc-3.4.6_v4)
set(CMAKE_C_COMPILER ${tools}/bin/i960-elf-gcc)
set(CMAKE_CXX_COMPILER ${tools}/bin/i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER ${tools}/bin/i960-elf-as)
set(CMAKE_LINKER ${tools}/bin/i960-elf-ld)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DUSH_CONFIG_PLATFORM_POSIX -fno-exceptions")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DCONFIG_PLATFORM_POSIX --std=gnu99")
set(CMAKE_OBJCOPY_COMPILER ${tools}/bin/i960-elf-objcopy)
set(CMAKE_OBJDUMP_COMPILER ${tools}/bin/i960-elf-objdump)
# gcc 3.4.6 doesn't support colorizing output
unset(CMAKE_COLOR_DIAGNOSTICS CACHE)
add_compile_options(-Wall)
# for some reason, the compiler generated multiple copies of the same entries
#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -mmc -mnumerics -O2")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -mmc -mnumerics -O2")
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
		cortex/Types.h cortex/SystemCounter.h cortex/SystemCounter.cc cortex/ModernGCC.h cortex/ModernGCC.cc cortex/builtins.h cortex/builtins.c cortex/IndirectInterrupts.cc
		cortex/ext2.h
		cortex/SoftwareDMA.h
		cortex/SoftwareDMA.cc
		cortex/SysExamine.cc
        cortex/builtins.s
)
add_library(wasm3
        wasm3/wasm3.h
        wasm3/wasm3_defs.h
        wasm3/m3_api_libc.c
        wasm3/m3_api_libc.h
        wasm3/m3_api_meta_wasi.c
        wasm3/m3_api_tracer.c
        wasm3/m3_api_tracer.h
        wasm3/m3_api_uvwasi.c
        wasm3/m3_api_wasi.c
        wasm3/m3_api_wasi.h
        wasm3/m3_bind.c
        wasm3/m3_bind.h
        wasm3/m3_code.c
        wasm3/m3_compile.c
        wasm3/m3_core.c
        wasm3/m3_env.c
        wasm3/m3_exec.c
        wasm3/m3_exec.h
        wasm3/m3_function.c
        wasm3/m3_info.c
        wasm3/m3_info.h
        wasm3/m3_math_utils.h
        wasm3/m3_module.c
        wasm3/m3_parse.c
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
# add the microshell library directly since I get an infinite loop otherwise
#add_library(microshell
#        microshell/src/inc/ush.h
#        microshell/src/inc/ush_commands.h
#        microshell/src/inc/ush_const.h
#        microshell/src/inc/ush_file.h
#        microshell/src/inc/ush_internal.h
#        microshell/src/inc/ush_node.h
#        microshell/src/inc/ush_preconfig.h
#        microshell/src/inc/ush_shell.h
#        microshell/src/inc/ush_types.h
#        microshell/src/inc/ush_utils.h
#        microshell/src/config/ush_config_platform.h
#        microshell/src/config/posix/ush_config.h
#        microshell/src/src/commands/ush_cmd.c
#        microshell/src/src/commands/ush_cmd_cat.c
#        microshell/src/src/commands/ush_cmd_cd.c
#        microshell/src/src/commands/ush_cmd_echo.c
#        microshell/src/src/commands/ush_cmd_help.c
#        microshell/src/src/commands/ush_cmd_ls.c
#        microshell/src/src/commands/ush_cmd_pwd.c
#        microshell/src/src/commands/ush_cmd_xxd.c
#        microshell/src/src/ush.c
#        microshell/src/src/ush_autocomp.c
#        microshell/src/src/ush_autocomp_state.c
#        microshell/src/src/ush_autocomp_utils.c
#        microshell/src/src/ush_commands.c
#        microshell/src/src/ush_file.c
#        microshell/src/src/ush_node.c
#        microshell/src/src/ush_node_mount.c
#        microshell/src/src/ush_node_utils.c
#        microshell/src/src/ush_parse.c
#        microshell/src/src/ush_parse_char.c
#        microshell/src/src/ush_parse_utils.c
#        microshell/src/src/ush_process.c
#        microshell/src/src/ush_prompt.c
#        microshell/src/src/ush_read.c
#        microshell/src/src/ush_read_char.c
#        microshell/src/src/ush_read_utils.c
#        microshell/src/src/ush_reset.c
#        microshell/src/src/ush_utils.c
#        microshell/src/src/ush_write.c
#        microshell/src/src/ush_write_utils.c
#
#)
add_subdirectory(microshell/src)
build_ush(microshell)
target_compile_definitions(
        microshell
        PUBLIC
        USH_CONFIG_PLATFORM_POSIX
)
set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/start.ld)
add_library(hitagiemul
        emulation/Segment.cc 
        emulation/Segment.h 
        emulation/Domain.cc 
        emulation/Domain.h
        emulation/RV32.h
        emulation/RV32.cc
        emulation/RV32.s
    )

target_include_directories(hitagiemul PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_include_directories(hitagicortex PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hitagisys PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hitagimon PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(wasm3 PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

add_library(tinyscheme
	tinyscheme/scheme.h
	tinyscheme/scheme-private.h
	tinyscheme/scheme.c
)
target_include_directories(tinyscheme PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(hitagimon
		gcc
		c
		m
		hitagicortex
		hitagisys
		hitagiemul
        wasm3
        microshell
		tinyscheme
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
        #PRIVATE -msoft-float
		#PRIVATE -mmc
		#PRIVATE -mnumerics
)
add_custom_command(
		OUTPUT prog.bin
		COMMAND ${CMAKE_OBJCOPY_COMPILER} --pad-to=0x00100000 --only-section=.text --only-section=.bss --only-section=.data --gap-fill=0x00 --set-section-flags .bss=alloc,load,contents -O binary hitagimon prog.bin
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
