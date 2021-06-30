cmake_minimum_required(VERSION 3.10)
project(hitagimon VERSION 0.1)
enable_language(CXX C ASM-ATT)
set(CMAKE_CXX_STANDARD 98)

set(tools /home/jwscoggins/sys/i960-elf-gcc-3.4.6)
set(CMAKE_C_COMPILER ${tools}/bin/i960-elf-gcc)
set(CMAKE_CXX_COMPILER ${tools}/bin/i960-elf-g++)
set(CMAKE_ASM-ATT_COMPILER ${tools}/bin/i960-elf-as)
set(CMAKE_ASM-ATT_FLAGS "${CMAKE_ASM_FLAGS} -ASB")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -msb -mnumerics -O2 -DBARE_METAL")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -msb -mnumerics -O2 -DBARE_METAL")
set(LINKER_SCRIPT "start.ld")
set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -Wl,--as-needed -nostdlib -static -T ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT} -msb -mnumerics -ffreestanding")
set(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
set(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)
set(CMAKE_FIND_ROOT_PATH_MODE_PACKAGE ONLY)
add_library(hitagichipset
		chipset/IODevice.cc
		chipset/IODevice.h
		chipset/ChipsetInteract.h
		)
add_library(hitagisys
		sys/brk.cc
		sys/fstat.cc
		sys/isatty.cc
		sys/lseek.cc
		sys/write.cc
		sys/read.cc
		sys/open.cc
		sys/kill.cc
		sys/setitimer.cc
		sys/close.cc
		sys/gettimeofday.cc
		sys/access.cc
		sys/getpid.cc
		sys/_exit.cc)
add_library(hitagicortex
		cortex/EnvironmentInterface.cc
		cortex/EnvironmentInterface.h)

add_executable(hitagimon
		boot/sx_init.s # this must ALWAYS be first!
		boot/f_table.s
		boot/i_handle.s
		boot/i_table.s
		boot/f_handle.cc
		boot/ISR.cc
		# interface libraries
		hitagimain.cc)
#add_executable(hitagiclips )
add_library(hitagidoom
		# doom source code
		doom/am_map.h
		doom/d_englsh.h
		doom/d_event.h
		doom/d_french.h
		doom/d_items.h
		doom/d_main.h
		doom/d_net.h
		doom/doomdata.h
		doom/doomdef.h
		doom/doomstat.h
		doom/doomtype.h
		doom/d_player.h
		doom/dstrings.h
		doom/d_textur.h
		doom/d_think.h
		doom/d_ticcmd.h
		doom/f_finale.h
		doom/f_wipe.h
		doom/g_game.h
		doom/hu_lib.h
		doom/hu_stuff.h
		doom/i_net.h
		doom/info.h
		doom/i_sound.h
		doom/i_system.h
		doom/i_video.h
		doom/m_argv.h
		doom/m_bbox.h
		doom/m_cheat.h
		doom/m_fixed.h
		doom/m_menu.h
		doom/m_misc.h
		doom/m_random.h
		doom/m_swap.h
		doom/p_inter.h
		doom/p_local.h
		doom/p_mobj.h
		doom/p_pspr.h
		doom/p_saveg.h
		doom/p_setup.h
		doom/p_spec.h
		doom/p_tick.h
		doom/r_bsp.h
		doom/r_data.h
		doom/r_defs.h
		doom/r_draw.h
		doom/r_local.h
		doom/r_main.h
		doom/r_plane.h
		doom/r_segs.h
		doom/r_sky.h
		doom/r_state.h
		doom/r_things.h
		doom/sounds.h
		doom/s_sound.h
		doom/st_lib.h
		doom/st_stuff.h
		doom/tables.h
		doom/v_video.h
		doom/wi_stuff.h
		doom/w_wad.h
		doom/z_zone.h
		doom/am_map.c
		doom/d_items.c
		doom/d_main.c
		doom/d_net.c
		doom/doomdef.c
		doom/doomstat.c
		doom/dstrings.c
		doom/f_finale.c
		doom/f_wipe.c
		doom/g_game.c
		doom/hu_lib.c
		doom/hu_stuff.c
		doom/i_main.c
		doom/i_net.c
		doom/info.c
		doom/i_sound.c
		doom/i_system.c
		doom/i_video.c
		doom/m_argv.c
		doom/m_bbox.c
		doom/m_cheat.c
		doom/m_fixed.c
		doom/m_menu.c
		doom/m_misc.c
		doom/m_random.c
		doom/m_swap.c
		doom/p_ceilng.c
		doom/p_doors.c
		doom/p_enemy.c
		doom/p_floor.c
		doom/p_inter.c
		doom/p_lights.c
		doom/p_map.c
		doom/p_maputl.c
		doom/p_mobj.c
		doom/p_plats.c
		doom/p_pspr.c
		doom/p_saveg.c
		doom/p_setup.c
		doom/p_sight.c
		doom/p_spec.c
		doom/p_switch.c
		doom/p_telept.c
		doom/p_tick.c
		doom/p_user.c
		doom/r_bsp.c
		doom/r_data.c
		doom/r_draw.c
		doom/r_main.c
		doom/r_plane.c
		doom/r_segs.c
		doom/r_sky.c
		doom/r_things.c
		doom/sounds.c
		doom/s_sound.c
		doom/st_lib.c
		doom/st_stuff.c
		doom/tables.c
		doom/v_video.c
		doom/wi_stuff.c
		doom/w_wad.c
		doom/z_zone.c)

#set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT})
target_link_libraries(hitagimon
		g
		g2c
		gcc
		c
		m
		#nosys # stub library
		#hitagidoom
		hitagicortex
		hitagisys
		hitagichipset
		stdc++
		g
		g2c
		c
		m
		gcc)
