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
.text
 # processor starts execution at this spot upon power-up after self-test.
 start_ip:
    clear_g14

# enable address debugging
    # copy the interrupt table to RAM space, more like proper spaces
    ldconst 1028, g0 # load length of the interrupt table
    ldconst 0, g4 # initialize offset to 0
    ldconst intr_table, g1 # load source
    ldconst intr_ram, g2    # load address of new table
    bal move_data # branch to move routine
# copy PRCB to RAM space, located at _prcb_ram
    ldconst 176,g0 # load length of PRCB
    ldconst 0, g4 # initialize offset to 0
    ldconst prcb_ptr, g1 # load source
    ldconst _prcb_ram, g2 # load destination
    bal move_data # branch to move routine
 # fix up the PRCB to point to a new interrupt table
    ldconst intr_ram, g12 # load address
    st g12, 20(g2) # store into PRCB

    ldconst 0xff000010, g5
    ldconst reinitialize_iac, g6
    synmovq g5, g6
/*
move_data:
    ldq (g1)[g4*1], g8  # load 4 words into g8
    stq g8, (g2)[g4*1]  # store to RAM block
    addi g4,16, g4      # increment index
    cmpibg  g0,g4, move_data # loop until done
    bx (g14)
    */
move_data:
    movqstr g2, g1, g0
    bx (g14)
 /*
  * -- At this point, the PRCB, and interrupt table have been moved to RAM.
  *    It is time to issue a reinitialize IAC, which will start us anew with our RAM based PRCB.
  *
  * -- The IAC message, found in the 4 words located at the reinitialize_iac label, contains pointers
  *    to the current System Address Table, the new RAM based PRCB, and to the Instruction Pointer
  *    labeled start_again_ip
 */
    /* FALLTHROUGH DOES NOT HAPPEN HERE!!!! */
    .align 4 # Align BEFORE the label...holy crap
reinitialize_iac:
    .word 0x93000000    # reinitialize IAC message
    .word system_address_table
    .word _prcb_ram     # use newly copied PRCB
    .word start_again_ip    # start here

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
    .word (0x01200000 + 0x1) # Supervisor stack pointer is at a fixed position
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
	.word 0, 0, 0, 0 # 4-7
	.word 0, 0, 0, 0 # 8-11
	.word 0, 0, 0, 0 # 12-15
	.word 0, 0, 0, 0 # 16-19
	.word 0, 0, 0, 0 # 20-23
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
	.word 0, 0, 0, 0
	.word 0, 0, 0, 0
	.word 0, 0, 0, 0 # 236-239
	.word 0, 0, 0, 0 # 240-243
	.word 0, 0, 0, 0 # 244-247
	.word 0, 0, 0, 0 # 248-251
	.word 0, 0, 0, 0 # 252-255
	.word 0, 0, 0, 0 # 256-259
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