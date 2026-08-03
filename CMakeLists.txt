cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.2)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 17)

set(CMAKE_C_COMPILER i960-elf-gcc)
set(CMAKE_CXX_COMPILER i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER i960-elf-as)
set(CMAKE_LINKER i960-elf-ld)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DUSH_CONFIG_PLATFORM_POSIX -fno-exceptions -fno-use-cxa-atexit")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -DBARE_METAL -DUSE_STRLWR=0 -DUSE_DL=0 -DCONFIG_PLATFORM_POSIX --std=gnu99")
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
add_library(hitagisys
		# disabled entries are because mon960 provides the implementations for them
        sys/brk.cc
        sys/kill.cc
        sys/getpid.cc
		sys/_exit.cc
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
)
add_library(tinyscheme
    tinyscheme/scheme.c
    tinyscheme/scheme.h
    tinyscheme/scheme-private.h
    tinyscheme/opdefines.h
    )

add_executable(hitagimon
		boot/sx_init.s # this must ALWAYS be first!
		boot/f_table.s
		boot/i_handle.s
		boot/i_table.s
		boot/f_handle.cc
		boot/macros.s
		boot/syscall_table.s
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


add_library(clips
maya/clips/agenda.c
maya/clips/analysis.c
maya/clips/argacces.c
maya/clips/bload.c
maya/clips/bmathfun.c
maya/clips/bsave.c
maya/clips/classcom.c
maya/clips/classexm.c
maya/clips/classfun.c
maya/clips/classinf.c
maya/clips/classini.c
maya/clips/classpsr.c
maya/clips/clsltpsr.c
maya/clips/commline.c
maya/clips/conscomp.c
maya/clips/constrct.c
maya/clips/constrnt.c
maya/clips/crstrtgy.c
maya/clips/cstrcbin.c
maya/clips/cstrccom.c
maya/clips/cstrcpsr.c
maya/clips/cstrnbin.c
maya/clips/cstrnchk.c
maya/clips/cstrncmp.c
maya/clips/cstrnops.c
maya/clips/cstrnpsr.c
maya/clips/cstrnutl.c
maya/clips/default.c
maya/clips/defins.c
maya/clips/developr.c
maya/clips/dffctbin.c
maya/clips/dffctbsc.c
maya/clips/dffctcmp.c
maya/clips/dffctdef.c
maya/clips/dffctpsr.c
maya/clips/dffnxbin.c
maya/clips/dffnxcmp.c
maya/clips/dffnxexe.c
maya/clips/dffnxfun.c
maya/clips/dffnxpsr.c
maya/clips/dfinsbin.c
maya/clips/dfinscmp.c
maya/clips/drive.c
maya/clips/emathfun.c
maya/clips/engine.c
maya/clips/envrnbld.c
maya/clips/envrnmnt.c
maya/clips/evaluatn.c
maya/clips/expressn.c
maya/clips/exprnbin.c
maya/clips/exprnops.c
maya/clips/exprnpsr.c
maya/clips/extnfunc.c
maya/clips/factbin.c
maya/clips/factbld.c
maya/clips/factcmp.c
maya/clips/factcom.c
maya/clips/factfile.c
maya/clips/factfun.c
maya/clips/factgen.c
maya/clips/facthsh.c
maya/clips/factlhs.c
maya/clips/factmch.c
maya/clips/factmngr.c
maya/clips/factprt.c
maya/clips/factqpsr.c
maya/clips/factqury.c
maya/clips/factrete.c
maya/clips/factrhs.c
maya/clips/filecom.c
maya/clips/filertr.c
maya/clips/fileutil.c
maya/clips/generate.c
maya/clips/genrcbin.c
maya/clips/genrccmp.c
maya/clips/genrccom.c
maya/clips/genrcexe.c
maya/clips/genrcfun.c
maya/clips/genrcpsr.c
maya/clips/globlbin.c
maya/clips/globlbsc.c
maya/clips/globlcmp.c
maya/clips/globlcom.c
maya/clips/globldef.c
maya/clips/globlpsr.c
maya/clips/immthpsr.c
maya/clips/incrrset.c
maya/clips/inherpsr.c
maya/clips/inscom.c
maya/clips/insfile.c
maya/clips/insfun.c
maya/clips/insmngr.c
maya/clips/insmoddp.c
maya/clips/insmult.c
maya/clips/inspsr.c
maya/clips/insquery.c
maya/clips/insqypsr.c
maya/clips/iofun.c
maya/clips/lgcldpnd.c
maya/clips/memalloc.c
maya/clips/miscfun.c
maya/clips/modulbin.c
maya/clips/modulbsc.c
maya/clips/modulcmp.c
maya/clips/moduldef.c
maya/clips/modulpsr.c
maya/clips/modulutl.c
maya/clips/msgcom.c
maya/clips/msgfun.c
maya/clips/msgpass.c
maya/clips/msgpsr.c
maya/clips/multifld.c
maya/clips/multifun.c
maya/clips/objbin.c
maya/clips/objcmp.c
maya/clips/objrtbin.c
maya/clips/objrtbld.c
maya/clips/objrtcmp.c
maya/clips/objrtfnx.c
maya/clips/objrtgen.c
maya/clips/objrtmch.c
maya/clips/parsefun.c
maya/clips/pattern.c
maya/clips/pprint.c
maya/clips/prccode.c
maya/clips/prcdrfun.c
maya/clips/prcdrpsr.c
maya/clips/prdctfun.c
maya/clips/prntutil.c
maya/clips/proflfun.c
maya/clips/reorder.c
maya/clips/reteutil.c
maya/clips/retract.c
maya/clips/router.c
maya/clips/rulebin.c
maya/clips/rulebld.c
maya/clips/rulebsc.c
maya/clips/rulecmp.c
maya/clips/rulecom.c
maya/clips/rulecstr.c
maya/clips/ruledef.c
maya/clips/ruledlt.c
maya/clips/rulelhs.c
maya/clips/rulepsr.c
maya/clips/scanner.c
maya/clips/sortfun.c
maya/clips/strngfun.c
maya/clips/strngrtr.c
maya/clips/symblbin.c
maya/clips/symblcmp.c
maya/clips/symbol.c
maya/clips/sysdep.c
maya/clips/textpro.c
maya/clips/tmpltbin.c
maya/clips/tmpltbsc.c
maya/clips/tmpltcmp.c
maya/clips/tmpltdef.c
maya/clips/tmpltfun.c
maya/clips/tmpltlhs.c
maya/clips/tmpltpsr.c
maya/clips/tmpltrhs.c
maya/clips/tmpltutl.c
maya/clips/userdata.c
maya/clips/userfunctions.c
maya/clips/utility.c
maya/clips/watch.c
maya/clips/agenda.h
maya/clips/analysis.h
maya/clips/argacces.h
maya/clips/bload.h
maya/clips/bmathfun.h
maya/clips/bsave.h
maya/clips/classcom.h
maya/clips/classexm.h
maya/clips/classfun.h
maya/clips/classinf.h
maya/clips/classini.h
maya/clips/classpsr.h
maya/clips/clips.h
maya/clips/clsltpsr.h
maya/clips/commline.h
maya/clips/conscomp.h
maya/clips/constant.h
maya/clips/constrct.h
maya/clips/constrnt.h
maya/clips/crstrtgy.h
maya/clips/cstrcbin.h
maya/clips/cstrccmp.h
maya/clips/cstrccom.h
maya/clips/cstrcpsr.h
maya/clips/cstrnbin.h
maya/clips/cstrnchk.h
maya/clips/cstrncmp.h
maya/clips/cstrnops.h
maya/clips/cstrnpsr.h
maya/clips/cstrnutl.h
maya/clips/default.h
maya/clips/defins.h
maya/clips/developr.h
maya/clips/dffctbin.h
maya/clips/dffctbsc.h
maya/clips/dffctcmp.h
maya/clips/dffctdef.h
maya/clips/dffctpsr.h
maya/clips/dffnxbin.h
maya/clips/dffnxcmp.h
maya/clips/dffnxexe.h
maya/clips/dffnxfun.h
maya/clips/dffnxpsr.h
maya/clips/dfinsbin.h
maya/clips/dfinscmp.h
maya/clips/drive.h
maya/clips/emathfun.h
maya/clips/engine.h
maya/clips/entities.h
maya/clips/envrnbld.h
maya/clips/envrnmnt.h
maya/clips/evaluatn.h
maya/clips/expressn.h
maya/clips/exprnbin.h
maya/clips/exprnops.h
maya/clips/exprnpsr.h
maya/clips/extnfunc.h
maya/clips/factbin.h
maya/clips/factbld.h
maya/clips/factcmp.h
maya/clips/factcom.h
maya/clips/factfile.h
maya/clips/factfun.h
maya/clips/factgen.h
maya/clips/facthsh.h
maya/clips/factlhs.h
maya/clips/factmch.h
maya/clips/factmngr.h
maya/clips/factprt.h
maya/clips/factqpsr.h
maya/clips/factqury.h
maya/clips/factrete.h
maya/clips/factrhs.h
maya/clips/filecom.h
maya/clips/filertr.h
maya/clips/fileutil.h
maya/clips/generate.h
maya/clips/genrcbin.h
maya/clips/genrccmp.h
maya/clips/genrccom.h
maya/clips/genrcexe.h
maya/clips/genrcfun.h
maya/clips/genrcpsr.h
maya/clips/globlbin.h
maya/clips/globlbsc.h
maya/clips/globlcmp.h
maya/clips/globlcom.h
maya/clips/globldef.h
maya/clips/globlpsr.h
maya/clips/immthpsr.h
maya/clips/incrrset.h
maya/clips/inherpsr.h
maya/clips/inscom.h
maya/clips/insfile.h
maya/clips/insfun.h
maya/clips/insmngr.h
maya/clips/insmoddp.h
maya/clips/insmult.h
maya/clips/inspsr.h
maya/clips/insquery.h
maya/clips/insqypsr.h
maya/clips/iofun.h
maya/clips/lgcldpnd.h
maya/clips/match.h
maya/clips/memalloc.h
maya/clips/miscfun.h
maya/clips/modulbin.h
maya/clips/modulbsc.h
maya/clips/modulcmp.h
maya/clips/moduldef.h
maya/clips/modulpsr.h
maya/clips/modulutl.h
maya/clips/msgcom.h
maya/clips/msgfun.h
maya/clips/msgpass.h
maya/clips/msgpsr.h
maya/clips/multifld.h
maya/clips/multifun.h
maya/clips/network.h
maya/clips/objbin.h
maya/clips/objcmp.h
maya/clips/object.h
maya/clips/objrtbin.h
maya/clips/objrtbld.h
maya/clips/objrtcmp.h
maya/clips/objrtfnx.h
maya/clips/objrtgen.h
maya/clips/objrtmch.h
maya/clips/parsefun.h
maya/clips/pattern.h
maya/clips/pprint.h
maya/clips/prccode.h
maya/clips/prcdrfun.h
maya/clips/prcdrpsr.h
maya/clips/prdctfun.h
maya/clips/prntutil.h
maya/clips/proflfun.h
maya/clips/reorder.h
maya/clips/reteutil.h
maya/clips/retract.h
maya/clips/router.h
maya/clips/rulebin.h
maya/clips/rulebld.h
maya/clips/rulebsc.h
maya/clips/rulecmp.h
maya/clips/rulecom.h
maya/clips/rulecstr.h
maya/clips/ruledef.h
maya/clips/ruledlt.h
maya/clips/rulelhs.h
maya/clips/rulepsr.h
maya/clips/scanner.h
maya/clips/setup.h
maya/clips/sortfun.h
maya/clips/strngfun.h
maya/clips/strngrtr.h
maya/clips/symblbin.h
maya/clips/symblcmp.h
maya/clips/symbol.h
maya/clips/sysdep.h
maya/clips/textpro.h
maya/clips/tmpltbin.h
maya/clips/tmpltbsc.h
maya/clips/tmpltcmp.h
maya/clips/tmpltdef.h
maya/clips/tmpltfun.h
maya/clips/tmpltlhs.h
maya/clips/tmpltpsr.h
maya/clips/tmpltrhs.h
maya/clips/tmpltutl.h
maya/clips/userdata.h
maya/clips/usrsetup.h
maya/clips/utility.h
maya/clips/watch.h)
# add_library(neutron
# 	maya/error/Exception.cc
# 	maya/error/Exception.h
# 	maya/debug/here.h
# 	maya/regex/regex.h
# 	maya/features/dependent_false.h
# 	maya/features/observer_ptr.h
# 	maya/features/overloaded.h
# 	maya/features/type_traits.h
# 	maya/features/cpp17/optional.h
# 	maya/string/aliases.h
# 	maya/string/join.h
# 	maya/string/predicate.h
# 	maya/string/replace.h
# 	maya/string/split.h
# 	maya/string/tokenizer.h
# 	maya/string/trim.h
# 	maya/platform/types.h
# 	maya/bugfix/fix_lhs_rhs_conflicts_begin.h
# 	maya/bugfix/fix_lhs_rhs_conflicts_end.h
# 	maya/platform/os.h
# 		)

		#add_library(electron
		#	maya/electron/Aliases.h
		#	maya/electron/api.h
		#	maya/electron/DefglobalInspector.h
		#	maya/electron/DefruleInspector.cc
		#	maya/electron/DefruleInspector.h
		#	maya/electron/DeftemplateInspector.cc
		#	maya/electron/DeftemplateInspector.h
		#	maya/electron/Environment.cc
		#	maya/electron/Environment.h
		#	maya/electron/FactBuilder.cc
		#	maya/electron/FactBuilder.h
		#	maya/electron/FactModifier.cc
		#	maya/electron/FactModifier.h
		#	maya/electron/FileSystemExtensions.cc
		#	maya/electron/FileSystemExtensions.h
		#	maya/electron/FunctionCallBuilder.cc
		#	maya/electron/GCLocker.h
		#	maya/electron/IncludePathCache.cc
		#	maya/electron/InstanceBuilder.cc
		#	maya/electron/InstanceBuilder.h
		#	maya/electron/InstanceInspector.cc
		#	maya/electron/InstanceInspector.h
		#	maya/electron/InstanceModifier.cc
		#	maya/electron/InstanceModifier.h
		#	maya/electron/IORouter.cc
		#	maya/electron/IORouter.h
		#	maya/electron/StringExtensions.h
		#	maya/electron/StringExtensions.cc
		#	maya/electron/MultifieldBuilder.cc
		#	maya/electron/MultifieldBuilder.h
		#	maya/electron/ArgumentConstructor.h
		#	maya/electron/ParsingExtensions.h
		#	maya/electron/ParsingExtensions.cc
		#        maya/electron/MultifieldExtensions.cc
		#        maya/electron/MultifieldExtensions.h
		#)
target_include_directories(clips PUBLIC 
	${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/maya
)
#target_include_directories(neutron PUBLIC 
#	${CMAKE_CURRENT_SOURCE_DIR}
#	${CMAKE_CURRENT_SOURCE_DIR}/maya
#)
# target_include_directories(electron PUBLIC 
# 	${CMAKE_CURRENT_SOURCE_DIR}
# 	${CMAKE_CURRENT_SOURCE_DIR}/maya
# )

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
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(hitagibench PUBLIC 
    ${CMAKE_CURRENT_SOURCE_DIR}
	${CMAKE_CURRENT_SOURCE_DIR}/libcommoncommunication/include
)
target_include_directories(coremark PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

target_link_libraries(hitagimon
		hitagisys
		hitagicortex
		hitagisys
		hitagibench
        tinyscheme
        microshell
		coremark
		clips
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
