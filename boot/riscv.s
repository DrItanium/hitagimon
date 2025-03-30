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
# r4 -> opcode
# r5 -> 
# r6 -> 
# r7 -> 
# r8 -> 
# r9 -> 
# r10 -> 
# r11 -> 
# r12 -> 
# r13 -> 
# r14 -> 
# r15 -> 
# g0 -> 
# g1 -> 
# g2 -> 
# g3 -> 
# g4 -> 
# g5 -> 
# g6 -> 
# g7 -> 
# g8 -> 
# g9 -> 
# g10 -> 
# g11 -> 
# g12 -> 
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
instruction_decoder_body:
	ld 0(g13), r3 # load the current instruction
	mov r3, r4    # make a copy of it
	extract 0, 7, r4 # opcode extraction
next_instruction:
	addo g13, 4, g13 # go to the next instruction
	b instruction_decoder_body 
rv32_auipc:
	b next_instruction
rv32_lui:
	b next_instruction
rv32_jal:
	b instruction_decoder_body
rv32_jalr:
	b instruction_decoder_body
rv32_beq:
	b instruction_decoder_body
rv32_bne:
	b instruction_decoder_body
rv32_blt:
	b instruction_decoder_body
rv32_bge:
	b instruction_decoder_body
rv32_bltu:
	b instruction_decoder_body
rv32_bgeu:
	b instruction_decoder_body
rv32_load_primary:
	mov r3, r4
	extract 12, 3, r4
	ld rv32_load_instruction_table[r4*4], r4
	b next_instruction
rv32_lb:
	b next_instruction
rv32_lh:
	b next_instruction
rv32_lw:
	b next_instruction
rv32_lbu:
	b next_instruction
rv32_lhu:
	b next_instruction
rv32_undefined_instruction:
	b next_instruction

.align 4
rv32_load_instruction_table:
	.word rv32_lb
	.word rv32_lh
	.word rv32_lw
	.word rv32_undefined_instruction
	.word rv32_lbu
	.word rv32_lhu
	.word rv32_undefined_instruction
	.word rv32_undefined_instruction
.data
.align 6
hart0_gpr_register_file:
	.space 32 * 4
hart0_fpr_register_file:
	.space 32 * 8
hart0_pc:
	.space 4
