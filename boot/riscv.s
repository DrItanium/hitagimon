/*
i960SxChipset
Copyright (c) 2020-2025, Joshua Scoggins
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

/* RISCV32 emulator begin */
# rv32 -> i960 like found in the riscv teaching book
# Use i960 assembly as the microcode for this "simulator"
# pfp -> i960 previous frame pointer
# sp ->  i960 stack pointer
# rip -> i960 return instruction pointer
# r3 -> instruction contents
# r4 -> temporary
# r5 -> temporary
# r6 -> opcode function address
# r7 -> opcode subfunction address
# r8 -> opcode subsubfunction address
# r9 -> 
# r10 -> 
# r11 -> 
# r12 -> 
# r13 -> 
# r14 -> 
# r15 -> 
# g0 -> rs1
# g1 -> rs2
# g2 -> rd
# g3 -> immediate
# g4 -> 
# g5 -> 
# g6 -> 
# g7 -> 
# g8 -> 
# g9 -> 
# g10 -> 
# g11 -> 
# g12 -> GPR base address
# g13 -> PC (we can start at address zero now)
# g14 -> i960 link register
# fp -> i960 frame pointer

# we want to actually map the emulator code into the microcontroller itself
# that way, we can actually keep the lowest 64 megabytes completely free. 
.include "macros.s"
# taken from the isa overview
.set OPCODE_LOAD,      0b0000011
.set OPCODE_STORE,     0b0100011
.set OPCODE_MADD,      0b1000011
.set OPCODE_BRANCH,    0b1100011

.set OPCODE_LOAD_FP,   0b0000111
.set OPCODE_STORE_FP,  0b0100111
.set OPCODE_MSUB, 	   0b1000111
.set OPCODE_JALR,      0b1100111

.set OPCODE_CUSTOM_0,  0b0001011
.set OPCODE_CUSTOM_1,  0b0101011
.set OPCODE_NMSUB,     0b1001011
.set OPCODE_RESERVED,  0b1101011

.set OPCODE_MISC_MEM,  0b0001111
.set OPCODE_AMO,       0b0101111
.set OPCODE_NMADD,     0b1001111
.set OPCODE_JAL,       0b1101111

.set OPCODE_OP_IMM,    0b0010011
.set OPCODE_OP,        0b0110011
.set OPCODE_FP,        0b1010011
.set OPCODE_SYSTEM,    0b1110011

.set OPCODE_AUIPC,     0b0010111
.set OPCODE_LUI,       0b0110111
.set OPCODE_OP_V,      0b1010111
.set OPCODE_OP_VE,     0b1110111

.set OPCODE_OP_IMM_32, 0b0011011
.set OPCODE_OP_32,     0b0111011
.set OPCODE_CUSTOM_2,  0b1011011
.set OPCODE_CUSTOM_3,  0b1111011


.text
.align 4
.macro instruction_dispatch target, subtable=0, subsubtable=0, subsubsubtable=0
	.word \target, \subtable, \subsubtable, \subsubsubtable
.endm
.macro unimplemented_opcode 
	instruction_dispatch rv32_undefined_instruction, rv32_undefined_instruction
.endm
rv32_opcode_dispatch_table:
	instruction_dispatch rv32_load_primary, rv32_load_instruction_table
	unimplemented_opcode # load-fp
	unimplemented_opcode # custom0
	instruction_dispatch rv32_misc_mem
	instruction_dispatch rv32_op_imm
	instruction_dispatch rv32_auipc
	unimplemented_opcode # OP-IMM-32 (can be used for custom instructions in rv32 mode)
	unimplemented_opcode # 48b mode?
	instruction_dispatch rv32_store_primary, rv32_store_instruction_table
	unimplemented_opcode # we don't support madd right now
# this will hold the opcode dispatch table, aligned to be as easy to work on as possible
rv32_load_instruction_table:
	.word rv32_lb, rv32_lh, rv32_lw, rv32_undefined_instruction
	.word rv32_lbu, rv32_lhu, rv32_undefined_instruction, rv32_undefined_instruction
rv32_store_instruction_table:
	.word rv32_sb, rv32_sh, rv32_sw, rv32_undefined_instruction
	.word rv32_undefined_instruction, rv32_undefined_instruction
	.word rv32_undefined_instruction, rv32_undefined_instruction
.align 6
rv32_load_primary:
	mov r3, r4
	extract 12, 3, r4
	ld (r7)[r4*4], r4 # use the secondary address to save code space
	bx (r4)
rv32_op_imm:
	b next_instruction
rv32_misc_mem:
	b next_instruction
rv32_auipc:
	b next_instruction
rv32_lui:
	b next_instruction
rv32_jal:
	b instruction_decoder_body
rv32_jalr:
	b instruction_decoder_body
rv32_store_primary:
	mov r3, r4  # make a copy of r3
	extract 7, 5, r4 # get the imm[4:0] out
	shri 25, r3, r5	# grab imm[11:5] but make sure that immediates are sign-extended
	shlo 5, r5, r5    # move it into position
	or r4, r5, g3 	  # immediate has been computed
					  # compute rs1
	mov r3, g0 # copy r3 to g0
	extract 15, 5, g0 # get rs1
	mov r3, g1 # copy r3 to g1
	extract 20, 5, g1 # get rs2
	mov r3, r4
	extract 12, 3, r4 # now figure out where to go
	ld (r7)[r4*4], r4 # use the secondary address to save code space
	bx (r4)
rv32_lb:
rv32_lh:
rv32_lw:
rv32_lbu:
rv32_lhu:
rv32_sb:
rv32_sh:
rv32_sw:
rv32_undefined_instruction:
	b next_instruction
.align 6
.global riscv_emulator_start
riscv_emulator_start:
	mov 0, r3

	movq 0, r4
	movq 0, r8
	movq 0, r12
	movq 0, g0 
	movq 0, g4
	movq 0, g8
	movt 0, g12
	ldconst hart0_gpr_register_file, g12
instruction_decoder_body:
	ld 0(g13), r3 # load the current instruction
	mov r3, r4    # make a copy of it
	extract 0, 7, r4 # opcode extraction
	and 3, r4, r5 # check the lowest two bits
	cmpobne 3, r5, rv32_undefined_instruction # it is an illegal instruction
	shro 2, r3, r4 # remove the lowest two bits since we know it is 0b11
	and 31, r4, r4 # now get the remaining 5 bits to figure out where to dispatch to
	ldl rv32_opcode_dispatch_table[r4*8], r6 # load the two addresses necessary for execution
	bx (r6) # jump to r6, r7 has the secondary table
next_instruction:
	addo g13, 4, g13 # go to the next instruction
	b instruction_decoder_body 

.data
.align 6
hart0_gpr_register_file:
	.space 32 * 4
