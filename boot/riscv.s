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
# r3 -> 
# r4 -> 
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
# g12 -> PC
# g13 -> RISCV32 memory space start (i960 address)
# g14 -> i960 link register
# fp -> i960 frame pointer

# we want to actually map the emulator code into the microcontroller itself
# that way, we can actually keep the lowest 64 megabytes completely free. 
.include "macros.s"
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
	ldconst 0, g12 # setup PC
	ldconst __riscv_space_start__, g13 # setup the base address
	ldconst 0, g14
	b riscv_emulator_start
.data
.align 6
hart0_gpr_register_file:
	.space 32 * 4
hart0_fpr_register_file:
	.space 32 * 8
hart0_pc:
	.space 4
