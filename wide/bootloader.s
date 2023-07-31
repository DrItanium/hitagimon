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
.macro clear_g14
        ldconst 0, g14 # c compiler expects g14 to be 0
.endm
.macro save_globals
/* -- We allocate a spot for a "register holder" on the stack
 *    and store data to that spot. We will take advantage of
 *    the fact that this will be allocated at the first spot on the stack
 */
        ldconst 64, r4
        addo    sp, r4, sp
        stq     g0, -64(sp)
        stq     g4, -48(sp)
        stq     g8, -32(sp)
        stt     g12, -16(sp)
 .endm
 .macro restore_globals
        ldq     -64(sp), g0
        ldq     -48(sp), g4
        ldq     -32(sp), g8
        ldt     -16(sp), g12
 .endm

.macro c_call function
    clear_g14
    call \function
.endm

.macro c_callx function
    clear_g14
    callx \function
.endm

.macro def_system_call index,name
.text
.align 4
.global _\()\name
_\()\name:
lda \index, g13
calls g13
ret
.endm

.macro DeclareSegment a, b, c, d
.word \a
.word \b
.word \c
.word \d
.endm
.macro NullSegment
.space 16
.endm

.macro SegmentSelector base
.word ((\base)<<6) | 0x3f
.endm

.macro SimpleRegion address
DeclareSegment 0, 0, \address, 0x00fc00a3
.endm

.macro PagedRegion address, size
.space 8
.word \address
.word ((\size) << 18) | 0x5
.endm

.macro BipagedRegion address, size
.space 8
.word \address
.word ((\size) << 18) | 0x7
.endm

.macro PageEntry addr
.word ((\addr) | 0xc7)
.endm

.macro SmallSegmentTable addr
.space 8
.word \addr
.word (0x3f << 18) | 0xfb
.endm

.macro PortSegment addr
DeclareSegment 0, 0, \addr, 0x204000fb
.endm
.macro DefTableEntry name
   .word (\name + 0x2)
.endm
.macro ReservedTableEntry
.word 0
.endm
.global cs1
.global prcb_ptr
.global system_address_table
.global start_ip

# Core Initialization Block (located at address 0)
# 8 words

.text
    .word system_address_table # SAT pointer
    .word prcb_ptr # prcb pointer
    .word 0
    .word start_ip # pointer to first ip
    .word cs1
    .word 0
    .word 0
    .word -1
.text
 # processor starts execution at this spot upon power-up after self-test.
 start_ip:
    #clear_g14

# enable address debugging
    # copy the interrupt table to RAM space, more like proper spaces
    #ldconst 1028, g0 # load length of the interrupt table
    #ldconst 0, g4 # initialize offset to 0
    #ldconst intr_table, g1 # load source
    #ldconst intr_ram, g2    # load address of new table
    #bal move_data # branch to move routine
# copy PRCB to RAM space, located at _prcb_ram
    #ldconst 176,g0 # load length of PRCB
    #ldconst 0, g4 # initialize offset to 0
    #ldconst prcb_ptr, g1 # load source
    #ldconst _prcb_ram, g2 # load destination
    #bal move_data # branch to move routine
 # fix up the PRCB to point to a new interrupt table
    # load the address
    ldconst 0xff000010, g5
    /*
    * the new address will be setup as we go through
    */
    ldconst reinitialize_iac, g6
    synmovq g5, g6
move_data:
    ldq (g1)[g4*1], g8  # load 4 words into g8
    stq g8, (g2)[g4*1]  # store to RAM block
    addi g4,16, g4      # increment index
    cmpibg  g0,g4, move_data # loop until done
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
_user_reserved_core:
_do_nothing_isr:
    ret
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
    .word 0x01202000 # 24 - interrupt stack pointer
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
    .word 0x01201000
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    .word 0 # Preserved
    FaultTableEntry reserved # entry 0
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved # process
    FaultTableEntry reserved
    FaultTableEntry reserved
    FaultTableEntry reserved
# We pass the fault data by grabbing it and passing it via g0 to the function itself

/* fault table */
.macro FaultEntry index, code=0x2, table=0x2bf
.word (\index << 2) | \code
.word \table
.endm
.macro ReservedFaultEntry
FaultEntry 0x10
.endm
    .globl  fault_table
    .align  8
fault_table:
    ReservedFaultEntry # override
    ReservedFaultEntry # trace
    ReservedFaultEntry # Operation
    ReservedFaultEntry # arithmetic
    ReservedFaultEntry # floating point
    ReservedFaultEntry # constraint
    ReservedFaultEntry # virtual memory
    ReservedFaultEntry # protection
    ReservedFaultEntry # Machine
    ReservedFaultEntry # structural
    ReservedFaultEntry # type
    ReservedFaultEntry
    ReservedFaultEntry # process
    ReservedFaultEntry # descriptor
    ReservedFaultEntry # event
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry

/* Initial interrupt table
 * in my board, there is only one interrupt routine so they will all map to the same ISR
 */

        .global     intr_table
        .align      6
intr_table:
        .word       0               # Pending Priorities    0
        .fill       8, 4, 0         # Pending Interrupts    4 + (0->7)*4
        .word _do_nothing_isr;           # interrupt table entry 8
        .word _do_nothing_isr;           # interrupt table entry 9
        .word _do_nothing_isr;           # interrupt table entry 10
        .word _do_nothing_isr;           # interrupt table entry 11
        .word _do_nothing_isr;           # interrupt table entry 12
        .word _do_nothing_isr;           # interrupt table entry 13
        .word _do_nothing_isr;           # interrupt table entry 14
        .word _do_nothing_isr;           # interrupt table entry 15
        .word _do_nothing_isr;           # interrupt table entry 16
        .word _do_nothing_isr;           # interrupt table entry 17
        .word _do_nothing_isr;           # interrupt table entry 18
        .word _do_nothing_isr;           # interrupt table entry 19
        .word _do_nothing_isr;           # interrupt table entry 20
        .word _do_nothing_isr;           # interrupt table entry 21
        .word _do_nothing_isr;           # interrupt table entry 22
        .word _do_nothing_isr;           # interrupt table entry 23
        .word _do_nothing_isr;           # interrupt table entry 24
        .word _do_nothing_isr;           # interrupt table entry 25
        .word _do_nothing_isr;           # interrupt table entry 26
        .word _do_nothing_isr;           # interrupt table entry 27
        .word _do_nothing_isr;           # interrupt table entry 28
        .word _do_nothing_isr;           # interrupt table entry 29
        .word _do_nothing_isr;           # interrupt table entry 30
        .word _do_nothing_isr;           # interrupt table entry 31
        .word _do_nothing_isr;           # interrupt table entry 32
        .word _do_nothing_isr;           # interrupt table entry 33
        .word _do_nothing_isr;           # interrupt table entry 34
        .word _do_nothing_isr;           # interrupt table entry 35
        .word _do_nothing_isr;           # interrupt table entry 36
        .word _do_nothing_isr;           # interrupt table entry 37
        .word _do_nothing_isr;           # interrupt table entry 38
        .word _do_nothing_isr;           # interrupt table entry 39
        .word _do_nothing_isr;           # interrupt table entry 40
        .word _do_nothing_isr;           # interrupt table entry 41
        .word _do_nothing_isr;           # interrupt table entry 42
        .word _do_nothing_isr;           # interrupt table entry 43
        .word _do_nothing_isr;           # interrupt table entry 44
        .word _do_nothing_isr;           # interrupt table entry 45
        .word _do_nothing_isr;           # interrupt table entry 46
        .word _do_nothing_isr;           # interrupt table entry 47
        .word _do_nothing_isr;           # interrupt table entry 48
        .word _do_nothing_isr;           # interrupt table entry 49
        .word _do_nothing_isr;           # interrupt table entry 50
        .word _do_nothing_isr;           # interrupt table entry 51
        .word _do_nothing_isr;           # interrupt table entry 52
        .word _do_nothing_isr;           # interrupt table entry 53
        .word _do_nothing_isr;           # interrupt table entry 54
        .word _do_nothing_isr;           # interrupt table entry 55
        .word _do_nothing_isr;           # interrupt table entry 56
        .word _do_nothing_isr;           # interrupt table entry 57
        .word _do_nothing_isr;           # interrupt table entry 58
        .word _do_nothing_isr;           # interrupt table entry 59
        .word _do_nothing_isr;           # interrupt table entry 60
        .word _do_nothing_isr;           # interrupt table entry 61
        .word _do_nothing_isr;           # interrupt table entry 62
        .word _do_nothing_isr;           # interrupt table entry 63
        .word _do_nothing_isr;           # interrupt table entry 64
        .word _do_nothing_isr;           # interrupt table entry 65
        .word _do_nothing_isr;           # interrupt table entry 66
        .word _do_nothing_isr;           # interrupt table entry 67
        .word _do_nothing_isr;           # interrupt table entry 68
        .word _do_nothing_isr;           # interrupt table entry 69
        .word _do_nothing_isr;           # interrupt table entry 70
        .word _do_nothing_isr;           # interrupt table entry 71
        .word _do_nothing_isr;           # interrupt table entry 72
        .word _do_nothing_isr;           # interrupt table entry 73
        .word _do_nothing_isr;           # interrupt table entry 74
        .word _do_nothing_isr;           # interrupt table entry 75
        .word _do_nothing_isr;           # interrupt table entry 76
        .word _do_nothing_isr;           # interrupt table entry 77
        .word _do_nothing_isr;           # interrupt table entry 78
        .word _do_nothing_isr;           # interrupt table entry 79
        .word _do_nothing_isr;           # interrupt table entry 80
        .word _do_nothing_isr;           # interrupt table entry 81
        .word _do_nothing_isr;           # interrupt table entry 82
        .word _do_nothing_isr;           # interrupt table entry 83
        .word _do_nothing_isr;           # interrupt table entry 84
        .word _do_nothing_isr;           # interrupt table entry 85
        .word _do_nothing_isr;           # interrupt table entry 86
        .word _do_nothing_isr;           # interrupt table entry 87
        .word _do_nothing_isr;           # interrupt table entry 88
        .word _do_nothing_isr;           # interrupt table entry 89
        .word _do_nothing_isr;           # interrupt table entry 90
        .word _do_nothing_isr;           # interrupt table entry 91
        .word _do_nothing_isr;           # interrupt table entry 92
        .word _do_nothing_isr;           # interrupt table entry 93
        .word _do_nothing_isr;           # interrupt table entry 94
        .word _do_nothing_isr;           # interrupt table entry 95
        .word _do_nothing_isr;           # interrupt table entry 96
        .word _do_nothing_isr;           # interrupt table entry 97
        .word _do_nothing_isr;           # interrupt table entry 98
        .word _do_nothing_isr;           # interrupt table entry 99
        .word _do_nothing_isr;           # interrupt table entry 100
        .word _do_nothing_isr;           # interrupt table entry 101
        .word _do_nothing_isr;           # interrupt table entry 102
        .word _do_nothing_isr;           # interrupt table entry 103
        .word _do_nothing_isr;           # interrupt table entry 104
        .word _do_nothing_isr;           # interrupt table entry 105
        .word _do_nothing_isr;           # interrupt table entry 106
        .word _do_nothing_isr;           # interrupt table entry 107
        .word _do_nothing_isr;           # interrupt table entry 108
        .word _do_nothing_isr;           # interrupt table entry 109
        .word _do_nothing_isr;           # interrupt table entry 110
        .word _do_nothing_isr;           # interrupt table entry 111
        .word _do_nothing_isr;           # interrupt table entry 112
        .word _do_nothing_isr;           # interrupt table entry 113
        .word _do_nothing_isr;           # interrupt table entry 114
        .word _do_nothing_isr;           # interrupt table entry 115
        .word _do_nothing_isr;           # interrupt table entry 116
        .word _do_nothing_isr;           # interrupt table entry 117
        .word _do_nothing_isr;           # interrupt table entry 118
        .word _do_nothing_isr;           # interrupt table entry 119
        .word _do_nothing_isr;           # interrupt table entry 120
        .word _do_nothing_isr;           # interrupt table entry 121
        .word _do_nothing_isr;           # interrupt table entry 122
        .word _do_nothing_isr;           # interrupt table entry 123
        .word _do_nothing_isr;           # interrupt table entry 124
        .word _do_nothing_isr;           # interrupt table entry 125
        .word _do_nothing_isr;           # interrupt table entry 126
        .word _do_nothing_isr;           # interrupt table entry 127
        .word _do_nothing_isr;           # interrupt table entry 128
        .word _do_nothing_isr;           # interrupt table entry 129
        .word _do_nothing_isr;           # interrupt table entry 130
        .word _do_nothing_isr;           # interrupt table entry 131
        .word _do_nothing_isr;           # interrupt table entry 132
        .word _do_nothing_isr;           # interrupt table entry 133
        .word _do_nothing_isr;           # interrupt table entry 134
        .word _do_nothing_isr;           # interrupt table entry 135
        .word _do_nothing_isr;           # interrupt table entry 136
        .word _do_nothing_isr;           # interrupt table entry 137
        .word _do_nothing_isr;           # interrupt table entry 138
        .word _do_nothing_isr;           # interrupt table entry 139
        .word _do_nothing_isr;           # interrupt table entry 140
        .word _do_nothing_isr;           # interrupt table entry 141
        .word _do_nothing_isr;           # interrupt table entry 142
        .word _do_nothing_isr;           # interrupt table entry 143
        .word _do_nothing_isr;           # interrupt table entry 144
        .word _do_nothing_isr;           # interrupt table entry 145
        .word _do_nothing_isr;           # interrupt table entry 146
        .word _do_nothing_isr;           # interrupt table entry 147
        .word _do_nothing_isr;           # interrupt table entry 148
        .word _do_nothing_isr;           # interrupt table entry 149
        .word _do_nothing_isr;           # interrupt table entry 150
        .word _do_nothing_isr;           # interrupt table entry 151
        .word _do_nothing_isr;           # interrupt table entry 152
        .word _do_nothing_isr;           # interrupt table entry 153
        .word _do_nothing_isr;           # interrupt table entry 154
        .word _do_nothing_isr;           # interrupt table entry 155
        .word _do_nothing_isr;           # interrupt table entry 156
        .word _do_nothing_isr;           # interrupt table entry 157
        .word _do_nothing_isr;           # interrupt table entry 158
        .word _do_nothing_isr;           # interrupt table entry 159
        .word _do_nothing_isr;           # interrupt table entry 160
        .word _do_nothing_isr;           # interrupt table entry 161
        .word _do_nothing_isr;           # interrupt table entry 162
        .word _do_nothing_isr;           # interrupt table entry 163
        .word _do_nothing_isr;           # interrupt table entry 164
        .word _do_nothing_isr;           # interrupt table entry 165
        .word _do_nothing_isr;           # interrupt table entry 166
        .word _do_nothing_isr;           # interrupt table entry 167
        .word _do_nothing_isr;           # interrupt table entry 168
        .word _do_nothing_isr;           # interrupt table entry 169
        .word _do_nothing_isr;           # interrupt table entry 170
        .word _do_nothing_isr;           # interrupt table entry 171
        .word _do_nothing_isr;           # interrupt table entry 172
        .word _do_nothing_isr;           # interrupt table entry 173
        .word _do_nothing_isr;           # interrupt table entry 174
        .word _do_nothing_isr;           # interrupt table entry 175
        .word _do_nothing_isr;           # interrupt table entry 176
        .word _do_nothing_isr;           # interrupt table entry 177
        .word _do_nothing_isr;           # interrupt table entry 178
        .word _do_nothing_isr;           # interrupt table entry 179
        .word _do_nothing_isr;           # interrupt table entry 180
        .word _do_nothing_isr;           # interrupt table entry 181
        .word _do_nothing_isr;           # interrupt table entry 182
        .word _do_nothing_isr;           # interrupt table entry 183
        .word _do_nothing_isr;           # interrupt table entry 184
        .word _do_nothing_isr;           # interrupt table entry 185
        .word _do_nothing_isr;           # interrupt table entry 186
        .word _do_nothing_isr;           # interrupt table entry 187
        .word _do_nothing_isr;           # interrupt table entry 188
        .word _do_nothing_isr;           # interrupt table entry 189
        .word _do_nothing_isr;           # interrupt table entry 190
        .word _do_nothing_isr;           # interrupt table entry 191
        .word _do_nothing_isr;           # interrupt table entry 192
        .word _do_nothing_isr;           # interrupt table entry 193
        .word _do_nothing_isr;           # interrupt table entry 194
        .word _do_nothing_isr;           # interrupt table entry 195
        .word _do_nothing_isr;           # interrupt table entry 196
        .word _do_nothing_isr;           # interrupt table entry 197
        .word _do_nothing_isr;           # interrupt table entry 198
        .word _do_nothing_isr;           # interrupt table entry 199
        .word _do_nothing_isr;           # interrupt table entry 200
        .word _do_nothing_isr;           # interrupt table entry 201
        .word _do_nothing_isr;           # interrupt table entry 202
        .word _do_nothing_isr;           # interrupt table entry 203
        .word _do_nothing_isr;           # interrupt table entry 204
        .word _do_nothing_isr;           # interrupt table entry 205
        .word _do_nothing_isr;           # interrupt table entry 206
        .word _do_nothing_isr;           # interrupt table entry 207
        .word _do_nothing_isr;           # interrupt table entry 208
        .word _do_nothing_isr;           # interrupt table entry 209
        .word _do_nothing_isr;           # interrupt table entry 210
        .word _do_nothing_isr;           # interrupt table entry 211
        .word _do_nothing_isr;           # interrupt table entry 212
        .word _do_nothing_isr;           # interrupt table entry 213
        .word _do_nothing_isr;           # interrupt table entry 214
        .word _do_nothing_isr;           # interrupt table entry 215
        .word _do_nothing_isr;           # interrupt table entry 216
        .word _do_nothing_isr;           # interrupt table entry 217
        .word _do_nothing_isr;           # interrupt table entry 218
        .word _do_nothing_isr;           # interrupt table entry 219
        .word _do_nothing_isr;           # interrupt table entry 220
        .word _do_nothing_isr;           # interrupt table entry 221
        .word _do_nothing_isr;           # interrupt table entry 222
        .word _do_nothing_isr;           # interrupt table entry 223
        .word _do_nothing_isr;           # interrupt table entry 224
        .word _do_nothing_isr;           # interrupt table entry 225
        .word _do_nothing_isr;           # interrupt table entry 226
        .word _do_nothing_isr;           # interrupt table entry 227
        .word _do_nothing_isr;           # interrupt table entry 228
        .word _do_nothing_isr;           # interrupt table entry 229
        .word _do_nothing_isr;           # interrupt table entry 230
        .word _do_nothing_isr;           # interrupt table entry 231
        .word _do_nothing_isr;           # interrupt table entry 232
        .word _do_nothing_isr;           # interrupt table entry 233
        .word _do_nothing_isr;           # interrupt table entry 234
        .word _do_nothing_isr;           # interrupt table entry 235
        .word _do_nothing_isr;           # interrupt table entry 236
        .word _do_nothing_isr;           # interrupt table entry 237
        .word _do_nothing_isr;           # interrupt table entry 238
        .word _do_nothing_isr;           # interrupt table entry 239
        .word _do_nothing_isr;           # interrupt table entry 240
        .word _do_nothing_isr;           # interrupt table entry 241
        .word _do_nothing_isr;           # interrupt table entry 242
        .word _do_nothing_isr;           # interrupt table entry 243
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;            # NMI Interrupt
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr;           # Reserved
        .word _do_nothing_isr ;           # interrupt table entry 252
        .word _do_nothing_isr ;           # interrupt table entry 253
        .word _do_nothing_isr ;           # interrupt table entry 254
        .word _do_nothing_isr ;           # interrupt table entry 255
