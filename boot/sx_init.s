/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/


# NOTE: this code is taken from the initialization code found in the i960Sx manual

/*
Below is the system initialization code and tables.
The code builds the PRCB (PRocessor Control Block) in memory, sets up the stack frame, the interrupt,
fault, and system procedure tables, and then vectors to a user defined routine. *main*
*/

# declare ahead of time
.include "macros.s"
.macro DefTableEntry name
   .word (\name + 0x2)
.endm
.macro ReservedTableEntry
.word 0
.endm
.global system_address_table
.global prcb_ptr
.global _prcb_ram
.global start_ip
.global cs1
.global STACK_SIZE

.global _user_stack
.global _sup_stack # supervisor stack
.global _intr_stack # interrupt stack


# Core Initialization Block (located at address 0)
# 8 words

.text
    .word system_address_table # SAT pointer
    .word prcb_ptr # prcb pointer
    .word 0
    .word start_ip # pointer to first ip
    .word cs1 # calculated at link time (bind ?cs1 (- (+ ?SAT ?PRCB ?startIP)))
    .word 0
    .word 0
    .word -1
 # processor starts execution at this spot upon power-up after self-test.
.macro print_text name
ldconst \name, g0
bal boot_print
.endm
.macro transfer_data size,src,dest,offset
ldconst \size, g0
ldconst \src, g1
ldconst \dest, g2
ldconst \offset, g4
bal move_data
.endm
/* -- Below is a software loop to move data */
move_data:
    ldq (g1)[g4*1], g8 # load 4 words into g8
    stq g8, (g2)[g4*1] # store 4 words into RAM
    addi g4, 16, g4    # increment index
    cmpibg g0, g4, move_data # loop until done
    bx (g14)
 start_ip:
    clear_g14
    transfer_data 1028, intr_table, intr_ram, 0 # copy the interrupt table to RAM space, more like proper spaces
    transfer_data 176, prcb_ptr, _prcb_ram, 0 # copy PRCB to RAM space, located at _prcb_ram
    # fix up the PRCB to point to a new interrupt table
    ldconst intr_ram, g0 # load address
    ldconst _prcb_ram, g1 # load prcb in ram
    st g0, 20(g1) # store into PRCB

 /*
  * -- At this point, the PRCB, and interrupt table have been moved to RAM.
  *    It is time to issue a reinitialize IAC, which will start us anew with our RAM based PRCB.
  *
  * -- The IAC message, found in the 4 words located at the reinitialize_iac label, contains pointers
  *    to the current System Address Table, the new RAM based PRCB, and to the Instruction Pointer
  *    labeled start_again_ip
 */
    ldconst 0xff000010, g5
    ldconst reinitialize_iac, g6
    synmovq g5, g6
    /* FALLTHROUGH DOES NOT HAPPEN HERE!!!! */
    .align 4 # Align BEFORE the label...holy crap
reinitialize_iac:
    .word 0x93000000    # reinitialize IAC message
    .word system_address_table
    .word _prcb_ram     # use newly copied PRCB
    .word start_again_ip    # start here

  /*
   * The process will begin execution here after being reinitialized.
   *    We will now setup the stacks and continue.
   */

  start_again_ip:
  /* -- this would be a good place to diable board interrupts if you are using an interrupt controller.
   *
   * -- Before call to main, we need to take the processor out of the "interrupted" state.
   *    In order to do this, we will execute a call statement, then "fix up" the stack frame
   *    to cause an interrupt return to be executed.
   */
    ldconst 64, g0 # bump up stack to make
    addo sp, g0, sp # room for simulated interrupt frame
    call fix_stack  # routine to turn off int state
    lda _user_stack, fp     # setup user stack space
    lda -0x40(fp), pfp      # load pfp (just in case)
    lda 0x40(fp), sp        # set up current stack pointer
/* -- This is the point where your main code is called.
 *    If any IO needs to be set up, you should do it here before your
 *    call to main. No opens have been done for STDIN, STDOUT, or STDERR
 */
    callx _init_fp
    callx setupInterruptHandler
    c_callx _main # assume a main for startup
exec_fallthrough:
    b exec_fallthrough


_init_fp:
    # initialize the floating point registers if it makes sense
    cvtir   0, fp0
    movre   fp0, fp1
    movre   fp1, fp2
    movre   fp2, fp3
    ret

setupInterruptHandler:
    # setup the interrupt handlers to work correctly
    lda 0xff000004, g5
    # give maximum priority to the interrupt handlers
    ldconst 0xFCFDFEFF, g6 # load the interrupt handler defualt value
    synmov g5, g6
    ret
# setup the bss section so do giant blocks of writes
/* The routine below fixes up the stack for a flase interrupt return.
 * We have reserved area on the stack before the call to this
 * routine. We need to build a phony interrupt record here
 * to force the processor to pick it up on return. Also, we
 * will take advantage of the fact that the processor will
 * restore the PC and AC to its registers
 */
.align 6
fix_stack:
    flushreg
    or  pfp, 7, pfp     # put interrupt return code into pfp

    ldconst 0x1f0002, g0
    st  g0, -16(fp)     # store contrived PC
    ldconst 0x3b001000, g0  # setup arithmetic controls
    st  g0, -12(fp)     # store contrived AC
    ret


.align 6
system_address_table:
    NullSegment # 0
    NullSegment # 1
    NullSegment # 2
    NullSegment # 3
    NullSegment # 4
    NullSegment # 5
    NullSegment # 6
    DeclareSegment 0, 0, sys_proc_table, 0x304000fb # 7
    SmallSegmentTable system_address_table # 8
    DeclareSegment 0, 0, sys_proc_table, 0x304000fb # 9
    DeclareSegment 0, 0, fault_proc_table, 0x304000fb # 10
.align 6
# initial PRCB
# this is our startup PRCB. After initialization.
# this will be copied to RAM
prcb_ptr:
    .word 0x0 # 0 - reserved
    .word 0xc # 4 - processor state = executing (no virtual address translation)
    .word 0x0 # 8 - reserved
    .word 0x0 # 12 - current process
    .word 0x0 # 16 - dispatch port
    .word intr_table # 20 - interrupt table physical address
    .word _intr_stack # 24 - interrupt stack pointer
    .word 0x0 # 28 - reserved
    SegmentSelector 7 # 32 - pointer to offset zero (region 3 segment selector)
    SegmentSelector 9 # 36 - system procedure table pointer
    .word fault_table # 40 - fault table
    .word 0x0 # 44 - reserved
    .space 12 # 48 -reserved
    .word 0   # 60 -reserved
    .space 8  # 64 - idle time
    .word 0   # 72 - system error fault
    .word 0   # 76 - reserved
    .space 48 # 80 - resumption record
    .space 44 # 128 - system  error fault record

# the system procedure table will _only_ be used if the user make a supervisor procedure call
    .align 6

.global sys_proc_table
sys_proc_table:
    .word 0 # Reserved
    .word 0 # Reserved
    .word 0 # Reserved
    .word (_sup_stack + 0x1) # Supervisor stack pointer
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
# up to 260 entries!
    # example entry
	.word 0, 0, 0, 0 # 0-3
	.word 0, 0, 0    # 4-6
	DefTableEntry _hitagi_unlink
	ReservedTableEntry # _hitagi_getpid
	ReservedTableEntry # _hitagi_kill
	ReservedTableEntry # _hitagi_fstat
	ReservedTableEntry # sbrk
	ReservedTableEntry #_hitagi_argvlen
	ReservedTableEntry # _hitagi_argv
	ReservedTableEntry # _hitagi_chdir
	ReservedTableEntry # _hitagi_stat
    ReservedTableEntry # _hitagi_chmod
    ReservedTableEntry # _hitagi_utime
    ReservedTableEntry # _hitagi_time
    DefTableEntry _hitagi_gettimeofday
    DefTableEntry _hitagi_setitimer
    DefTableEntry _hitagi_getrusage
	.word 0, 0 # 22-23
	.word 0, 0, 0, 0 # 24-27
	.word 0, 0, 0, 0 # 28-31
	.word 0, 0, 0, 0 # 32-35
	.word 0, 0, 0, 0 # 36-39
	.word 0, 0, 0, 0 # 40-43
	.word 0, 0, 0, 0 # 44-47
	.word 0, 0, 0, 0 # 48-51
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 4-7
	# mon960 registrations
	.word 0, 0
	DefTableEntry _hitagi_open
	DefTableEntry _hitagi_read
	DefTableEntry _hitagi_write
	DefTableEntry _hitagi_lseek
	DefTableEntry _hitagi_close
	.word 0
	.word 0, 0, 0, 0 # 236-239
	.word 0, 0, 0, 0 # 240-243
	.word 0, 0, 0, 0 # 244-247
	.word 0, 0, 0, 0 # 248-251
	.word 0, 0, 0, 0 # 252-255
	.word 0
	DefTableEntry _hitagi_exit
	.word 0, 0 # 256-259
	#.word	(_console_io + 0x2)	# Calls 0 - console I/O routines
# up to a total of 260 entries

# below is the fault table for calls to the fault handler.
# this table is provided because the above table (supervisor table) will allow
# tracing of trace-fault events (creating an endless loop), whereas this table will
# not allow tracing of trace-fault events.
.macro FaultTableEntry name
DefTableEntry _user_\()\name\()_core
.endm
    .align 6
fault_proc_table:
    .word 0 # Reserved
    .word 0 # Reserved
    .word 0 # Reserved
    .word _sup_stack # Supervisor stack pointer
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    FaultTableEntry override # entry 0
    FaultTableEntry trace
    FaultTableEntry operation
    FaultTableEntry arithmetic
    FaultTableEntry floating_point
    FaultTableEntry constraint
    FaultTableEntry virtual_memory
    FaultTableEntry protection
    FaultTableEntry machine
    FaultTableEntry structural
    FaultTableEntry type
    FaultTableEntry process # process
    FaultTableEntry descriptor
    FaultTableEntry event
    FaultTableEntry reserved
.macro DefFaultDispatcher name
.text
_user_\()\name\()_core:
	lda	-48(fp), g0	/* pass fault data */
	callx _user_\()\name
	flushreg
	ret
.endm
# We pass the fault data by grabbing it and passing it via g0 to the function itself
DefFaultDispatcher override
DefFaultDispatcher trace
DefFaultDispatcher operation
DefFaultDispatcher arithmetic
DefFaultDispatcher floating_point
DefFaultDispatcher constraint
DefFaultDispatcher protection
DefFaultDispatcher machine
DefFaultDispatcher type
DefFaultDispatcher virtual_memory
DefFaultDispatcher structural
DefFaultDispatcher process
DefFaultDispatcher descriptor
DefFaultDispatcher event
DefFaultDispatcher reserved
# reserved entries
def_system_call 7, _sys_unlink
#def_system_call 8, _sys_getpid
#def_system_call 9, _sys_kill
#def_system_call 10, _sys_fstat
#def_system_call 11, _sys_sbrk
#def_system_call 12, _sys_argvlen
#def_system_call 13, _sys_argv
#def_system_call 14, _sys_chdir
#def_system_call 15, _sys_stat
#def_system_call 16, _sys_chmod
#def_system_call 17, _sys_utime
#def_system_call 18, _sys_time
def_system_call 19, _sys_gettimeofday
def_system_call 20, _sys_setitimer
def_system_call 21, _sys_getrusage

/* -- define RAM area to copy the PRCB and interrupt table
 *    to after initial bootup from EPROM/FLASH
 */
 .bss _user_stack, 0x8000, 6
 .bss _intr_stack, 0x8000, 6
 .bss _sup_stack,  0x8000, 6
 .bss intr_ram, 1028, 6
 .bss _prcb_ram, 176, 6
