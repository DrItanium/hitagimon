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
		IODevice.cc
		IODevice.h
        chipset/ChipsetInteract.cc
        chipset/ChipsetInteract.h
		IORoutines.h
		)
add_library(hitagisys
		sys/brk.cc
		fstat.cc
		isatty.cc
		lseek.cc
		write.cc
		read.cc
		open.cc
		kill.cc
		setitimer.cc
		sys/close.cc
		sys/gettimeofday.cc
		sys/access.cc
		sys/getpid.cc
		sys/_exit.cc)
add_library(hitagicortex
		cortex/EnvironmentInterface.cc
		cortex/EnvironmentInterface.h)

add_executable(hitagimon
		sx_init.s # this must ALWAYS be first!
		f_table.s
		i_handle.s
		i_table.s
		f_handle.cc
		ISR.cc
		# interface libraries
		console.cc
		console.h
		hitagimain.cc
		# doom source code
		am_map.h
		d_englsh.h
		d_event.h
		d_french.h
		d_items.h
		d_main.h
		d_net.h
		doomdata.h
		doomdef.h
		doomstat.h
		doomtype.h
		d_player.h
		dstrings.h
		d_textur.h
		d_think.h
		d_ticcmd.h
		f_finale.h
		f_wipe.h
		g_game.h
		hu_lib.h
		hu_stuff.h
		i_net.h
		info.h
		i_sound.h
		i_system.h
		i_video.h
		m_argv.h
		m_bbox.h
		m_cheat.h
		m_fixed.h
		m_menu.h
		m_misc.h
		m_random.h
		m_swap.h
		p_inter.h
		p_local.h
		p_mobj.h
		p_pspr.h
		p_saveg.h
		p_setup.h
		p_spec.h
		p_tick.h
		r_bsp.h
		r_data.h
		r_defs.h
		r_draw.h
		r_local.h
		r_main.h
		r_plane.h
		r_segs.h
		r_sky.h
		r_state.h
		r_things.h
		sounds.h
		s_sound.h
		st_lib.h
		st_stuff.h
		tables.h
		v_video.h
		wi_stuff.h
		w_wad.h
		z_zone.h
		am_map.c
		d_items.c
		d_main.c
		d_net.c
		doomdef.c
		doomstat.c
		dstrings.c
		f_finale.c
		f_wipe.c
		g_game.c
		hu_lib.c
		hu_stuff.c
		i_main.c
		i_net.c
		info.c
		i_sound.c
		i_system.c
		i_video.c
		m_argv.c
		m_bbox.c
		m_cheat.c
		m_fixed.c
		m_menu.c
		m_misc.c
		m_random.c
		m_swap.c
		p_ceilng.c
		p_doors.c
		p_enemy.c
		p_floor.c
		p_inter.c
		p_lights.c
		p_map.c
		p_maputl.c
		p_mobj.c
		p_plats.c
		p_pspr.c
		p_saveg.c
		p_setup.c
		p_sight.c
		p_spec.c
		p_switch.c
		p_telept.c
		p_tick.c
		p_user.c
		r_bsp.c
		r_data.c
		r_draw.c
		r_main.c
		r_plane.c
		r_segs.c
		r_sky.c
		r_things.c
		sounds.c
		s_sound.c
		st_lib.c
		st_stuff.c
		tables.c
		v_video.c
		wi_stuff.c
		w_wad.c
		z_zone.c)

#set_target_properties(hitagimon PROPERTIES LINK_DEPENDS ${CMAKE_SOURCE_DIR}/${LINKER_SCRIPT})
target_link_libraries(hitagimon
		g
		g2c
		gcc
		c
		m
		#nosys # stub library
		hitagicortex
		hitagisys
		hitagichipset
		stdc++
		g
		g2c
		c
		m
		gcc)
