cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.2)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 17)

#set(tools ${CMAKE_HOME}/dev/i960/i960-elf-gcc-11.5.0)
#set(CMAKE_C_COMPILER ${tools}/bin/i960-elf-gcc)
#set(CMAKE_CXX_COMPILER ${tools}/bin/i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER i960-elf-as)
set(CMAKE_LINKER i960-elf-ld)
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DUSH_CONFIG_PLATFORM_POSIX -fno-exceptions -ffreestanding")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DCONFIG_PLATFORM_POSIX --std=gnu99")
set(CMAKE_OBJCOPY_COMPILER i960-elf-objcopy)
set(CMAKE_OBJDUMP_COMPILER i960-elf-objdump)
# gcc 3.4.6 doesn't support colorizing output
unset(CMAKE_COLOR_DIAGNOSTICS CACHE)
add_compile_options(-Wall)
# for some reason, the compiler generated multiple copies of the same entries
#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -msc -mnumerics")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -msc -mnumerics")
#set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -msa -msoft-float")
#set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -msa -msoft-float")
set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -AMC -I${CMAKE_SOURCE_DIR}/boot")
#set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -AKA -I${CMAKE_SOURCE_DIR}/boot")
#add_link_options(-mmc -mnumerics)
#set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -msb -mnumerics -ffreestanding")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
#add_library(hitagicortex
#		cortex/Faults.def
#		cortex/IODevice.cc
#		cortex/IODevice.h
#		cortex/ChipsetInteract.h
#		cortex/Faults.cc
#		cortex/Faults.h
#		cortex/IAC.h
#		cortex/IAC.cc
#		cortex/IACImplementation.s
#		cortex/ModernCpp.h
#        cortex/SystemCounter.h cortex/SystemCounter.cc cortex/builtins.h cortex/builtins.c cortex/IndirectInterrupts.cc
#		cortex/ext2.h
#		cortex/SoftwareDMA.h
#		cortex/SoftwareDMA.cc
#		cortex/SysExamine.cc
#        cortex/builtins.s
#)
add_executable(hitagimon
		boot/sx_init.s # this must ALWAYS be first!
		boot/f_table.s
		boot/i_table.s
		boot/macros.s
		boot/riscv.s
		start.ld
)
set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/start.ld)
set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/start2.ld)


#target_include_directories(hitagicortex PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(hitagimon PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(hitagimon)

#add_link_options(-Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -ffreestanding -Wl,--allow-multiple-definition)
target_link_options(hitagimon
		#PRIVATE -Wl,--as-needed
		PRIVATE -static
		PRIVATE -T ${CMAKE_SOURCE_DIR}/start.ld
		#PRIVATE -ffreestanding
		#PRIVATE -Wl,--allow-multiple-definition
)
add_custom_command(
		OUTPUT prog.bin
		COMMAND ${CMAKE_OBJCOPY_COMPILER}
        --only-section=.boot --set-section-flags .boot=alloc,load,contents
        --only-section=.text
        --only-section=.data
        --only-section=.init --set-section-flags .init=alloc,load,contents
        --only-section=.bss --set-section-flags .bss=alloc,load,contents
        --only-section=.fini --set-section-flags .fini=alloc,load,contents
        --only-section=.rodata
        --only-section=.gcc_except_table --set-section-flags .gcc_except_table=alloc,load,contents
        --only-section=.init_array --set-section-flags .init_array=alloc,load,contents
        --only-section=.ctors --set-section-flags .ctors=alloc,load,contents
        --only-section=.dtors --set-section-flags .dtors=alloc,load,contents
        --gap-fill=0x00
        --pad-to=0x00100000
        -O binary hitagimon prog.bin
        VERBATIM
		DEPENDS hitagimon
		#MAIN_DEPENDENCY hitagimon
)

add_custom_target(hitagimon_sd_image
		DEPENDS prog.bin
)
