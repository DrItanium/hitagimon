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
# r6 -> temporary
# r7 -> temporary
# r8 -> opcode function address
# r9 -> opcode subfunction address
# r10 -> opcode subsubfunction address
# r11 -> opcode subsubsubfunction address
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

.macro instruction_dispatch target, subtable=0, subsubtable=0, subsubsubtable=0
	.word \target, \subtable, \subsubtable, \subsubsubtable
.endm
.macro unimplemented_opcode 
	instruction_dispatch rv32_undefined_instruction
.endm

.macro extract4 bitpos, len, src, dest
	mov \src, \dest
	extract \bitpos, \len, \dest
.endm
.macro extract_funct3 rdest=r4, rinst=r3
	extract4 12, 4, \rinst, \rdest
.endm
.macro funct3_dispatch rtmp=r4, rtable=r9, rinst=r3
	extract_funct3 \rtmp, \rinst # now figure out where to go by getting funct3
	ld (\rtable)[\rtmp*4], \rtmp  # dispatch to address pointed by rtable
	bx (\rtmp)
.endm
.macro extract_rs1 dest=g0, rinst=r3
	extract4 15, 5, \rinst, \dest
.endm
.macro extract_rs2 dest=g1, rinst=r3
	extract4 20, 5, \rinst, \dest
.endm
.macro extract_rd dest=g2, rinst=r3
	extract4 7, 5, \rinst, \dest
.endm
.macro skip_if_rd_is_x0 dest, target=g2
	cmpobe 0, \target, \dest
.endm

.text
.align 6
# all 32 real entries for the 0b11 form
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
	unimplemented_opcode # store-fp
	unimplemented_opcode # custom-1
	unimplemented_opcode # AMO
	instruction_dispatch rv32_op_primary
	instruction_dispatch rv32_lui
	unimplemented_opcode # op-32
	unimplemented_opcode # 64b
	unimplemented_opcode # we don't support madd right now
	unimplemented_opcode # msub
	unimplemented_opcode # nmsub
	unimplemented_opcode # nmadd
	unimplemented_opcode # op-fp
	unimplemented_opcode # op-v
	unimplemented_opcode # custom-2/rv128
	unimplemented_opcode # 48b
	instruction_dispatch rv32_branch_primary
	instruction_dispatch rv32_jalr
	unimplemented_opcode # reserved
	instruction_dispatch rv32_jal
	instruction_dispatch rv32_system
	unimplemented_opcode # op-ve
	unimplemented_opcode # custom-3/rv128
	unimplemented_opcode # >=8b




# this will hold the opcode dispatch table, aligned to be as easy to work on as possible
.align 4
rv32_load_instruction_table:
	.word rv32_lb, rv32_lh, rv32_lw, rv32_undefined_instruction
	.word rv32_lbu, rv32_lhu, rv32_undefined_instruction, rv32_undefined_instruction
.align 4
rv32_store_instruction_table:
	.word rv32_sb, rv32_sh, rv32_sw, rv32_undefined_instruction
	.word rv32_undefined_instruction, rv32_undefined_instruction
	.word rv32_undefined_instruction, rv32_undefined_instruction

.align 6
# I have decided to ignore hint instructions completely.
# At some point, I may use them for something.
# some of these hint instructions are interesting, specifically
# ---- INSTRUCTION IMPLEMENTATIONS BEGIN ----
.macro extract_imm11_itype dest=g3, src=r3
	shri 20, \src, \dest # construct a sign extended version of imm[11:0]
.endm
# ADDI - Add Immediate
rv32_addi:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	addo g3, r4, r5    # add rs1 with the immediate
	st r5, (g12)[g2*4] # save to the register
1:
	b next_instruction
# SLTI - Set Less Than Immediate (place the value 1 in register rd if register
#		rs1 is less than the sign-extended immediate when both are treated as
#		signed numbers)
rv32_slti:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	cmpi r4, g3 	   # compare r4 to g3 
	testl r5		   # check and see if r4 < g3
	st r5, (g12)[g2*4] # save the result to the dest register
1:
	b next_instruction
rv32_sltiu:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	cmpo r4, g3 	   # compare r4 to g3 (ordinal)
	testl r5		   # check and see if r4 < g3
	st r5, (g12)[g2*4] # save the result to the dest register
1:
	b next_instruction
rv32_andi:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	and g3, r4, r5    # add rs1 with the immediate
	st r5, (g12)[g2*4] # save to the register
1:
	b next_instruction
rv32_ori:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	or g3, r4, r5    # add rs1 with the immediate
	st r5, (g12)[g2*4] # save to the register
1:
	b next_instruction
rv32_xori:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract_imm11_itype
	ld (g12)[g0*4], r4 # load rs1 contents
	xor g3, r4, r5    # add rs1 with the immediate
	st r5, (g12)[g2*4] # save to the register
1:
	b next_instruction
# SLLI - Shift Left Logical Immediate
rv32_slli:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract4 20, 5, r3, r4 # extract imm[4:0]
	ld (g12)[g0*4], r5
	shlo r4, r5, r6        # do the shift left
	st r6, (g12)[g2*4]	   # save the result
1:
	b next_instruction
# SRLI - Shift Right Logical Immediate
rv32_srli:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract4 20, 5, r3, r4 # extract imm[4:0]
	ld (g12)[g0*4], r5	   # get the contents of rs1
	shro r4, r5, r6        # do the shift right
	st r6, (g12)[g2*4]	   # save the result
1:
	b next_instruction
# SRAI - Shift Right Arithmetic Immediate
rv32_srai:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	extract4 20, 5, r3, r4 # extract imm[4:0]
	ld (g12)[g0*4], r5	   # get the contents of rs1
	shri r4, r5, r6        # do the shift right (but do the integer version)
	st r6, (g12)[g2*4]	   # save the result
1:
	b next_instruction
# AUIPC - Add Upper Immediate to PC
rv32_auipc:
	extract_rd 			               # where we are going to save things to
	skip_if_rd_is_x0 1f 			   # skip the actual act of saving if the destination is x0, this is a hint
	ldconst 0xFFFFF000, r4             # load a mask into memory
	and r3, r4, r5		               # construct the offset
	addo g13, r5, r6 	               # add it to the program counter
	st r6, (g12)[g2*4] 				   # save the result to a register
1:
	b next_instruction
# LUI - Load Upper Immediate
rv32_lui:
	extract_rd 						   
	skip_if_rd_is_x0 1f # skip if destination is x0 since it is a hint
	ldconst 0xFFFFF000, r4
	and r3, r4, r5
	st r5, (g12)[g2*4]
1:
	b next_instruction
# ADD - Add
rv32_add:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	addo r4, r5, r6    # do the addition operation, use ordinal form to prevent integer overflow fault
	st r6, (g12)[g2*4]
1:
	b next_instruction
# SLT - Set Less Than
rv32_slt:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	cmpi r4, r5		   # compare r4, r5
	testl r6		   # test to see if we got a less than
	st r6, (g12)[g2*4]
1:
	b next_instruction
rv32_sltu:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	cmpo r4, r5		   # compare r4, r5
	testl r6		   # test to see if we got a less than
	st r6, (g12)[g2*4]
1:
	b next_instruction
rv32_and:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	and r4, r5, r6    
	st r6, (g12)[g2*4]
1:
	b next_instruction
rv32_or:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	or r4, r5, r6    
	st r6, (g12)[g2*4]
1:
	b next_instruction
rv32_xor:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	xor r4, r5, r6    
	st r6, (g12)[g2*4]
1:
	b next_instruction
# SLL - Shift Left Logical
rv32_sll:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	shlo r4, r5, r6    
	st r6, (g12)[g2*4]
1:
	b next_instruction
# SRL - Shift Right Logical
rv32_srl:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	shro r4, r5, r6    
	st r6, (g12)[g2*4]
1:
	b next_instruction
# SUB - Subtract x[rs1] - x[rs2] -> x[rd]
rv32_sub:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	subo r5, r4, r6		# x[rd] = x[rs1] - x[rs2] 
	st r6, (g12)[g2*4]
1:
	b next_instruction
# SRA - Shift Right Arithmetic
rv32_sra:
	extract_rd
	skip_if_rd_is_x0 1f
	extract_rs1
	ld (g12)[g0*4], r4 # load rs1
	extract_rs2
	ld (g12)[g1*4], r5 # load rs2
	shri r5, r4, r6 
	st r6, (g12)[g2*4]
1:
	b next_instruction
rv32_jal:
	# jal has a strange immediate of [20|10:1|11|19:12] in that order
	# but since bit 20 is actually at position 31, it is trivial to just shift right integer
	# by 12 places to get the sign extension correct
	ldconst 0x80000000, r6  # we want to mask out all but the most significant bit
	and r6, r3, r4          # extract the upper most bit off by itself
	shri 12, r4, r4 		# then sign extend it by shifting 12 places down
	extract4 21, 10, r3, r5 # pull imm 10:1 and then correct it!
	shlo 1, r5, r5 			# shift left by one to make sure that we are aligned to 32-bit boundaries plus it is imm10:1
	chkbit 20, r3 			# extract bit 20 from the instruction (imm11)
	alterbit 11, r5, r5     # then shove the bit into position 11 of of the lower part
	# now onto imm[19:12]
	ldconst 0b00000000000011111111000000000000, r6 #  imm[19:12] mask
	and r6, r3, r7 			# imm[19:12] extracted by itself in place
	or r7, r5, r6			# merge [imm11:0] with imm[19:12]
	or r4, r6, g3			# this should get us our proper imm[20:1] value
	extract_rd
#	@todo implement the actual jump portion
	skip_if_rd_is_x0 1f # skip if destination is x0
	addo 4, g13, r4 		# the address to return to
	st r4, (g12)[g2*4]		# save the return address in the given register (usually, this is ra but for now, do not worry about that fact)
1:	
	addo g13, g3, g13  # jump to the relative offset
	b instruction_decoder_body

# JALR -> Jump and Link Register
rv32_jalr: 
	extract_funct3 r4
	cmpobne 0, r4, rv32_undefined_instruction # this may be wrong in the future
	extract_rs1
	extract_rd
	shri 20, r3, g3 # we want to make a sign extended version of imm[11:0]
	skip_if_rd_is_x0 1f # skip if destination is x0
	addo 4, g13, r4 # pc+4
	st r4, (g12)[g2*4]	# save the return address to wherever we need to go
1: 
	ld (g12)[g0*4], r4
	addo g3, r4, g13    # update PC
	# @todo generate an exception if the target address is not aligned to a four byte boundary
	b instruction_decoder_body

rv32_beq:
rv32_bne:
rv32_blt:
rv32_bltu:
rv32_bge:
rv32_bgeu:
rv32_lb:
rv32_lh:
rv32_lw:
rv32_lbu:
rv32_lhu:
	b next_instruction
rv32_fence:
	# do nothing right now
	b next_instruction
rv32_fence_tso:
	b next_instruction
rv32_ecall:
	b next_instruction
rv32_ebreak:
	b next_instruction
rv32_undefined_instruction:
	b next_instruction
# PRIMARY OPCODE: LOAD
rv32_load_primary:
	extract_rd
	extract_rs1
	shri 20, r3, g3 # compute the immediate with sign extension
	funct3_dispatch 
# PRIMARY OPCODE: STORE
rv32_store_primary:
	extract4 7, 5, r3, r4 # get imm[4:0]
	shri 25, r3, r5	# grab imm[11:5] but make sure that immediates are sign-extended
	shlo 5, r5, r5    # move it into position
	or r4, r5, g3 	  # immediate has been computed
					  # compute rs1
	extract_rs1		  # extract rs1 index (base)
	extract_rs2		  # extract rs2 index (src)
	funct3_dispatch
rv32_sb:
	# g0 -> base register index
	# g1 -> src register index
	# g3 -> immediate
	ld (g12)[g0*4], r4 # base
	ld (g12)[g1*4], r5 # src
	stob r5, (r4)[g3] # compute the address
	b next_instruction
rv32_sh:
	# g0 -> base register index
	# g1 -> src register index
	# g3 -> immediate
	ld (g12)[g0*4], r4 # base
	ld (g12)[g1*4], r5 # src
	stos r5, (r4)[g3] # compute the address
	b next_instruction
rv32_sw:
	# g0 -> base register index
	# g1 -> src register index
	# g3 -> immediate
	ld (g12)[g0*4], r4 # base
	ld (g12)[g1*4], r5 # src
	st r5, (r4)[g3] # compute the address
	b next_instruction
# PRIMARY OPCODE: SYSTEM
rv32_system:
	b next_instruction
# PRIMARY OPCODE: BRANCH
rv32_branch_primary:
	b next_instruction
# PRIMARY OPCODE: OP
rv32_op_primary:
	b next_instruction
# PRIMARY OPCODE: OP-IMM
rv32_op_imm:
	b next_instruction
# PRIMARY OPCODE: MISC-MEM
rv32_misc_mem:
	b next_instruction
# Here are some of the extensions I want to implement
# RV32M extension
# RV32F extension
# RV32D extension
# Zilsd (Load/Store pair for RV32)
# ld - Load doubleword to even/odd register pair, 32-bit encoding
# sd - Store doubleword from even/odd register pair, 32-bit encoding
# B Extension (bit manipulation) [Zba, Zbb, Zbs]
# Zba -> address generation
# Zbb -> basic bit manipulation
# Zbs -> single bit instructions
# Zicsr extension
# Zifencei extension?
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
	ldq rv32_opcode_dispatch_table[r4*16], r8 # load the two addresses necessary for execution
	bx (r8) # jump to r8, r9 has the secondary table, r10 has the tertiary table, r11 is the quaternary table
next_instruction:
	addo g13, 4, g13 # go to the next instruction
	b instruction_decoder_body 

.bss hart0_gpr_register_file, 128, 6
.bss hart0_fpr_register_file, 256, 6
