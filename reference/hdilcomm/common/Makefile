#*******************************************************************************
#*
#* Copyright (c) 1993, 1994, 1995 Intel Corporation
#*
#* Intel hereby grants you permission to copy, modify, and distribute this
#* software and its documentation.  Intel grants this permission provided
#* that the above copyright notice appears in all copies and that both the
#* copyright notice and this permission notice appear in supporting
#* documentation.  In addition, Intel grants this permission provided that
#* you prominently mark as "not part of the original" any modifications
#* made to this software or documentation, and that the name of Intel
#* Corporation not be used in advertising or publicity pertaining to
#* distribution of the software or the documentation without specific,
#* written prior permission.
#*
#* Intel Corporation provides this AS IS, WITHOUT ANY WARRANTY, EXPRESS OR
#* IMPLIED, INCLUDING, WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY
#* OR FITNESS FOR A PARTICULAR PURPOSE.  Intel makes no guarantee or
#* representations regarding the use of, or the results of the use of,
#* the software and documentation in terms of correctness, accuracy,
#* reliability, currentness, or otherwise; and you rely on the software,
#* documentation and results solely at your own risk.
#*
#* IN NO EVENT SHALL INTEL BE LIABLE FOR ANY LOSS OF USE, LOSS OF BUSINESS,
#* LOSS OF PROFITS, INDIRECT, INCIDENTAL, SPECIAL OR CONSEQUENTIAL DAMAGES
#* OF ANY KIND.  IN NO EVENT SHALL INTEL'S TOTAL LIABILITY EXCEED THE SUM
#* PAID TO INTEL FOR THE PRODUCT LICENSED HEREUNDER.
#*
#******************************************************************************/
#*****************************************************************************
# Makefile for hdil comm libraries - hdilcomm.a
#*****************************************************************************

# Essential under System V, harmless elsewhere
SHELL = /bin/sh

#-----------  Nested Dependencies ---------
COM_H     = $(IDIR)/hdi_com.h com.h
HDIL_H    = $(IDIR)/hdil.h $(IDIR)/hdi_regs.h $(IDIR)/hdi_brk.h \
            $(IDIR)/hdi_errs.h $(IDIR)/hdi_arch.h $(IDIR)/common.h \
            $(IDIR)/hdi_stop.h $(IDIR)/hdi_com.h

#-----------  DEFAULTS  ---------------------------
TARG	= hdilcomm.a
IDIR	= ../../hdil/common
CFLAGS  = $(OPT) -DHOST $(HOSTDEFS) -I$(IDIR) $(DEBUG)
O 	= o
RM	= rm -f
CP	= cp
DEBUG  	= 
NOLINK	= -c
OPT	= -g
IOPT	= -O
AR	= ar cvr $(TARG) $(OBJS) 
RANLIB	= ranlib
DEST	= $(G960BASE)/lib

# communication drivers, two currently
# default is UNIX which is in the local directory
#	 else dos in ../dos_src
DRIVEROBJS = unix_io.o
DRIVERSRC  = unix_io.c

#-----------------------------------------------------------------------------
#		Uncomment for SunOS sun4
#__sun4__#HOSTDEFS	= -DUSG -DSUNOS_SUN4
#-----------------------------------------------------------------------------
#		Uncomment for Solaris sun4
#__sol-sun4__#HOSTDEFS	= -DUSG -DSOLARIS_SUN4
#__sol-sun4__#RANLIB	= echo >/dev/null
#-----------------------------------------------------------------------------
#		Uncomment for system V / i386  (release 4)
#__i386vr4__#HOSTDEFS	= -DUSG
#__i386vr4__#RANLIB	= echo >/dev/null
#-----------------------------------------------------------------------------
#		Uncomment for RS/6000
#__rs6000__#HOSTDEFS	= -DUSG -DPOSIX -DRS6000
#-----------------------------------------------------------------------------
#		Uncomment for HP-UX (hp9000/700)
#__hp700__#HOSTDEFS	= -DUSG -DHP700
#__hp700__#RANLIB	= echo >/dev/null
#-----------------------------------------------------------------------------
#		Uncomment for HP-UX (hp9000/300)
#__hp9000__#HOSTDEFS	= -DUSG -DHP9000
#__hp9000__#RANLIB	= echo >/dev/null
#-----------------------------------------------------------------------------
#		Uncomment for MS/DOS
# Customized for Metaware's DOS compiler and Phar Lap's DOS-Extended, 
# assembler, linker, and librarian.
#
# Macros mean the following:
#	DOS	vanilla DOS, NOT THE SAME as gnu's use of DOS
#	MSDOS	??
#
#__dos__#DRIVEROBJS	= dos_io.obj timer.obj dos_pci.obj plx_pci.obj
#__dos__#TARG		= hdilcomm.lib
#__dos__#O 		    = obj
#__dos__#RM 		= del
#__dos__#CP 		= copy
#__dos__#HOSTDEFS	= -DMSDOS -fsoft -Heol=10
#__dos__#AS         = ml
#__dos__#ASFLAGS    = -nologo -Cp -Zi
#__dos__#CC 		= HC386
#__dos__#OPT		= -O3   #-g for debug
#__dos__#NOLINK		= -c
#__dos__#IOPT 		= -O3
#__dos__#RANLIB 	= @echo 
#__dos__#AR		    = 386lib $(TARG) -create -twocase @makelib.mw
#__dos__#DEST		=$(G960BASE,/=\)\lib
#-----------------------------------------------------------------------------
#		Uncomment for Windows 95
#__win95__#TARG		= hdilcomm.lib
#__win95__#O		= obj
#__win95__#HOSTDEFS	= -W2 -DWIN95 -DMSDOS -I.
#__win95__#CC		= CL
#__win95__#OPT		= -Zi
#__win95__#NOLINK		= -c
#__win95__#IOPT		= -Ot
#__win95__#RANLIB		= @echo 
#__win95__#AR		= lib /OUT:$(TARG) @makelib.mw
#__win95__#DEST		= $(G960BASE,/=\)\lib
#__win95__#DRIVEROBJS	= win_io.obj dos_pci.obj plx_pci.obj

#-----------------------------------------------------------------------------

COMMON  = com.$O packet.$O crcarray.$O xltbaud.$O autobaud.$O \
          commopt.$O seropt.$O getopt.$O pciopt.$O
OBJS	= $(COMMON) $(DRIVEROBJS)

.c.$O:
	$(CC) $(NOLINK) $(CFLAGS) $<

$(TARG): $(OBJS) makelib.mw
	$(RM) $(TARG)
	$(AR)
	$(RANLIB) $(TARG)

makelib.mw:
#__dos__#	!foreach i $(OBJS)
#__dos__#		echo -add $i >> makelib.mw
#__dos__#	!end
#__win95__#	!foreach i $(OBJS)
#__win95__#		echo $i >> makelib.mw
#__win95__#	!end

com.$O: $(IDIR)/common.h $(IDIR)/hdi_errs.h $(COM_H) dev.h
packet.$O: $(IDIR)/common.h $(IDIR)/hdi_errs.h $(COM_H) dev.h
crcarray.$O: $(IDIR)/common.h
xltbaud.$O: $(IDIR)/common.h dev.h
autobaud.$O: $(IDIR)/common.h $(IDIR)/hdi_errs.h $(COM_H) dev.h
unix_io.$O: unix_io.c serial.h $(COM_H) $(HDIL_H) dev.h
commopt.$O: commopt.c $(COM_H) $(HDIL_H)
seropt.$O: seropt.c $(COM_H) $(HDIL_H)
getopt.$O: getopt.c $(COM_H) $(IDIR)/common.h
pciopt.$O: pciopt.c $(COM_H) $(IDIR)/common.h


#__dos__#timer.$O: timer.c $(IDIR)/common.h
#__dos__#	$(CC) $(NOLINK) $(CFLAGS) timer.c

#__dos__#dos_io.$O: dos_io.c $(IDIR)/common.h
#__dos__#	$(CC) $(NOLINK) $(CFLAGS) dos_io.c

#__dos__#dos_pci.$O: dos_pci.c $(IDIR)/hdil.h $(IDIR)/hdi_com.h dos_pci.h cyc9060.h
#__dos__#	$(CC) $(NOLINK) $(CFLAGS) dos_pci.c
#__dos__#plx_pci.$O: plx_pci.c $(IDIR)/hdil.h $(IDIR)/hdi_com.h dos_pci.h cyc9060.h
#__dos__#	$(CC) $(NOLINK) $(CFLAGS) plx_pci.c

#-----------------------------------------------------------------------------
#		'STANDARD' GNU/960 TARGETS BELOW THIS POINT
#-----------------------------------------------------------------------------

# 'G960BASE' should be defined at invocation
# Avoid explicit use of "/" in pathnames, since DOS version needs "\"
install: 
	make clean
	make $(TARG) OPT=$(IOPT)
	(cd $(DEST) ; $(RM) $(TARG))
	$(CP) $(TARG) $(DEST)


# Separate lines required in order for DOS version to work
#
clean:
	$(RM) $(TARG)
	$(RM) *.$O
#__win95__#	$(RM) makelib.mw	# DOS -- doesn't hurt anybody else
#__dos__#	$(RM) makelib.mw	# DOS -- doesn't hurt anybody else
#__dos__#	$(RM) *.rex		# DOS -- doesn't hurt anybody else

COMMONSRC = com.c packet.c crcarray.c xltbaud.c autobaud.c
LINTSRCS=$(DRIVERSRC) $(COMMONSRC)
lint: $(LINTSRCS)
	lint $(CFLAGS) $(LINTSRCS) > lint

#-----------------------------------------------------------------------------
# Target to uncomment host-specific lines in this makefile, i.e. lines
# beginning in column 1 with the following string:  #__<hostname>__# .
# Original Makefile is backed up as 'Makefile.old'.
#
# Invoke with:  make make HOST=xxx
#-----------------------------------------------------------------------------
make:
	-@grep -s "^#The next line was generated by 'make make'" Makefile; \
	if test $$? = 0 ; then	\
		echo "Makefile has already been processed with 'make make'";\
		exit 1; \
	elif test $(HOST)x = x ; then \
		echo 'Specify "make make HOST=???"'; \
		exit 1; \
	else \
		mv -f Makefile Makefile.old; \
		echo "#The next line was generated by 'make make'"> Makefile; \
		echo "HOST=$(HOST)"				 >> Makefile; \
		echo						 >> Makefile; \
		sed "s/^#__$(HOST)__#//" < Makefile.old		 >> Makefile; \
		if test $(HOST) = dos; then mv Makefile makefile; \
			mv Makefile.old makefile.unx; \
		fi \
	fi
