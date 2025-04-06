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
# r3 -> instruction // contents
# r4 -> t0 // temporary
# r5 -> t1  // temporary
# r6 -> t2  // temporary
# r7 -> t3  // temporary
# r8 -> rs1
# r9 -> rs2
# r10 -> rd
# r11 -> immediate
# r12 -> 
# r13 -> pc
# r14 -> arg0
# r15 -> 
# g0 -> x1
# g1 -> x2
# g2 -> x3
# g3 -> x4
# g4 -> x5
# g5 -> x6
# g6 -> x7
# g7 -> x8
# g8 -> x9
# g9 -> x10
# g10 -> x11
# g11 -> x12
# g12 -> x13
# g13 -> x14
# g14 -> link register
# fp -> i960 frame pointer

# we want to actually map the emulator code into the microcontroller itself
# that way, we can actually keep the lowest 64 megabytes completely free. 
.include "macros.s"
# taken from the isa overview
.set OPCODE_LOAD,      (0b0000011 >> 2)
.set OPCODE_STORE,     (0b0100011 >> 2)
.set OPCODE_MADD,      (0b1000011 >> 2)
.set OPCODE_BRANCH,    (0b1100011 >> 2)
.set OPCODE_LOAD_FP,   (0b0000111 >> 2)
.set OPCODE_STORE_FP,  (0b0100111 >> 2)
.set OPCODE_MSUB, 	   (0b1000111 >> 2)
.set OPCODE_JALR,      (0b1100111 >> 2)
.set OPCODE_CUSTOM_0,  (0b0001011 >> 2)
.set OPCODE_CUSTOM_1,  (0b0101011 >> 2)
.set OPCODE_NMSUB,     (0b1001011 >> 2)
.set OPCODE_RESERVED,  (0b1101011 >> 2)
.set OPCODE_MISC_MEM,  (0b0001111 >> 2)
.set OPCODE_AMO,       (0b0101111 >> 2)
.set OPCODE_NMADD,     (0b1001111 >> 2)
.set OPCODE_JAL,       (0b1101111 >> 2)
.set OPCODE_OP_IMM,    (0b0010011 >> 2)
.set OPCODE_OP,        (0b0110011 >> 2)
.set OPCODE_FP,        (0b1010011 >> 2)
.set OPCODE_SYSTEM,    (0b1110011 >> 2)
.set OPCODE_AUIPC,     (0b0010111 >> 2)
.set OPCODE_LUI,       (0b0110111 >> 2)
.set OPCODE_OP_V,      (0b1010111 >> 2)
.set OPCODE_OP_VE,     (0b1110111 >> 2)
.set OPCODE_OP_IMM_32, (0b0011011 >> 2)
.set OPCODE_OP_32,     (0b0111011 >> 2)
.set OPCODE_CUSTOM_2,  (0b1011011 >> 2)
.set OPCODE_CUSTOM_3,  (0b1111011 >> 2)
.set OPCODE_48B, 	   (0b0011111 >> 2)
.set OPCODE_64B, 	   (0b0111111 >> 2)
.set OPCODE_48B_2, 	   (0b1011111 >> 2)
.set OPCODE_80B,       (0b1111111 >> 2)

.macro extract4 bitpos, len, src, dest
	mov \src, \dest
	extract \bitpos, \len, \dest
.endm
.macro extract_funct3 rdest=t0, rinst=instruction
	shro 12, \rinst, \rdest
	and 7, \rdest, \rdest
.endm
.macro extract_rs1 dest=rs1, rinst=instruction
	shro 15, \rinst, \dest
	and 31, \dest, \dest
.endm
.macro extract_rs2 dest=rs2, rinst=instruction
	shro 20, \rinst, \dest
	and 31, \dest, \dest
.endm
.macro extract_rd dest=rd, rinst=instruction
	shro 7, \rinst, \dest
	and 31, \dest, \dest
.endm
.macro skip_if_rd_is_x0 dest, target=rd
	cmpobe 0, \target, \dest
.endm
.macro extract_imm11_itype dest=immediate, src=instruction
	shri 20, \src, \dest # construct a sign extended version of imm[11:0]
.endm

.macro store_and_return dest, src=r14
	mov \src, \dest
	bx (g14)
.endm

.text
# x0 - zero
# x1 - ra (g0)
# x2 - sp (g1)
# x3 - gp (g2)
# x4 -    (g3)
# x5 -    (g4)
# x6 -    (g5)
# x7 -    (g6)
# x8 -    (g7)
# x9 -    (g8)
# x10 -   (g9)
# x11 -   (g10)
# x12 -   (g11)
# x13 -   (g12)
# x14 -   (g13)
rv32_abi_store_register:
	# rd - index
	# r14 - value
	cmpobge 16, rd, 2f 
	bx 1f[rd*8]
1:
	store_and_return r14 # do nothing
	store_and_return g0 
	store_and_return g1 
	store_and_return g2 
	store_and_return g3 
	store_and_return g4 
	store_and_return g5 
	store_and_return g6 
	store_and_return g7 
	store_and_return g8 
	store_and_return g9 
	store_and_return g10 
	store_and_return g11 
	store_and_return g12 
	store_and_return g13 
2:
	st r15, hart0_gpr_register_file[r14*4]
	bx (g14)

rv32_abi_load_register:
	# r14 - index
	cmpobge 16, r14, 2f 
	bx 1f[r14*8]
1:
.macro load_and_return src, dest=r14
	mov \src, \dest
	bx (g14)
.endm
	load_and_return 0
	load_and_return g0
	load_and_return g1
	load_and_return g2
	load_and_return g3
	load_and_return g4
	load_and_return g5
	load_and_return g6
	load_and_return g7
	load_and_return g8
	load_and_return g9
	load_and_return g10
	load_and_return g11
	load_and_return g12
	load_and_return g13
2:
	ld hart0_gpr_register_file[r14*4], r14
	bx (g14)
.align 6
# I have decided to ignore hint instructions completely.
# At some point, I may use them for something.
# some of these hint instructions are interesting, specifically
# ---- INSTRUCTION IMPLEMENTATIONS BEGIN ----
# AUIPC - Add Upper Immediate to PC
rv32_auipc:
	extract_rd 			               # where we are going to save things to
	ldconst 0xFFFFF000, t0             # load a mask into memory
	and instruction, t0, t1		       # construct the offset
	addo pc, t1, r14 	               # add it to the program counter
	bal rv32_abi_store_register 	   # store it to the register file (this will cause x0 to be ignored)
	b next_instruction
# LUI - Load Upper Immediate
rv32_lui:
	extract_rd 						   
	shri 12, instruction, t0
	shro 12, t0, r14
	bal rv32_abi_store_register
	b next_instruction
rv32_jal:
	# jal has a strange immediate of [20|10:1|11|19:12] in that order
	# but since bit 20 is actually at position 31, it is trivial to just shift right integer
	# by 12 places to get the sign extension correct
	ldconst 0x80000000, t2  # we want to mask out all but the most significant bit
	and t2, instruction, t0          # extract the upper most bit off by itself
	shri 12, t0, t0 		# then sign extend it by shifting 12 places down
	extract4 21, 10, instruction, t1 # pull imm 10:1 and then correct it!
	shlo 1, t1, t1 			# shift left by one to make sure that we are aligned to 32-bit boundaries plus it is imm10:1
	chkbit 20, instruction 			# extract bit 20 from the instruction (imm11)
	alterbit 11, t1, t1     # then shove the bit into position 11 of of the lower part
	# now onto imm[19:12]
	ldconst 0b00000000000011111111000000000000, t2 #  imm[19:12] mask
	and t2, instruction, t3 			# imm[19:12] extracted by itself in place
	or t1, t3, t2			# merge [imm11:0] with imm[19:12]
	or t0, t2, immediate			# this should get us our proper imm[20:1] value
	extract_rd
	addo 4, pc, r14 		# the address to return to
	bal rv32_abi_store_register # if x0 then the store doesn't actually happen
	addo pc, immediate, pc  # jump to the relative offset
	b instruction_decoder_body

# JALR -> Jump and Link Register
rv32_jalr: 
	extract_funct3 t0
	cmpobne 0, t0, rv32_undefined_instruction # this may be wrong in the future
	extract_rs1
	extract_rd
	shri 20, instruction, immediate # we want to make a sign extended version of imm[11:0]
	addo 4, pc, r14 # pc+4
	bal rv32_abi_store_register # x0 will be ignored
	mov rs1, r14
	bal rv32_abi_load_register
	addo immediate, r14, pc    # update PC
	# @todo generate an exception if the target address is not aligned to a four byte boundary
	b instruction_decoder_body
# PRIMARY OPCODE: LOAD
rv32_load_primary:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	shri 20, instruction, immediate # compute the immediate with sign extension
	extract_funct3 t0, instruction # now figure out where to go by getting funct3
	bx rv32_load_instruction_table[t0*4]
rv32_load_instruction_table:
	b rv32_lb
	b rv32_lh
	b rv32_lw
	b rv32_undefined_instruction # ld from the Zilsd extension
	b rv32_lbu
	b rv32_lhu
	b rv32_undefined_instruction
	b rv32_undefined_instruction
rv32_lb:
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ldib (t0)[immediate], t1 # dest
	st t1, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_lh:
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ldis (t0)[immediate], t1 # dest
	st t1, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_lw:
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ld (t0)[immediate], t1 # dest
	st t1, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_lbu:
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ldob (t0)[immediate], t1 # dest
	st t1, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_lhu:
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ldos (t0)[immediate], t1 # dest
	st t1, hart0_gpr_register_file[rd*4]
	b next_instruction

# PRIMARY OPCODE: STORE
rv32_store_primary:
    #extract4 7, 5, instruction, t0 # get imm[4:0]
	shro 7, instruction, t0	# extract imm[4:0]
	and 31, t0, t0 			# mask it 
	shri 25, instruction, t1	# grab imm[11:5] but make sure that immediates are sign-extended
	shlo 5, t1, t1    # move it into position
	or t0, t1, immediate 	  # immediate has been computed
					  # compute rs1
	extract_rs1		  # extract rs1 index (base)
	extract_rs2		  # extract rs2 index (src)
	extract_funct3 t0, instruction # now figure out where to go by getting funct3
	bx rv32_store_instruction_table[t0*4]
rv32_store_instruction_table:
	b rv32_sb
	b rv32_sh
	b rv32_sw
	b rv32_undefined_instruction # sd from the Zilsd extension
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
rv32_sb:
	# rs1 -> base register index
	# rs2 -> src register index
	# g3 -> immediate
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ld hart0_gpr_register_file[rs2*4], t1 # src
	stob t1, (t0)[immediate] # compute the address
	b next_instruction
rv32_sh:
	# rs1 -> base register index
	# rs2 -> src register index
	# g3 -> immediate
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ld hart0_gpr_register_file[rs2*4], t1 # src
	stos t1, (t0)[immediate] # compute the address
	b next_instruction
rv32_sw:
	# rs1 -> base register index
	# rs2 -> src register index
	# g3 -> immediate
	ld hart0_gpr_register_file[rs1*4], t0 # base
	ld hart0_gpr_register_file[rs2*4], t1 # src
	st t1, (t0)[immediate] # compute the address
	b next_instruction
# PRIMARY OPCODE: SYSTEM
rv32_system:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	shri 20, instruction, immediate # compute the immediate with sign extension
	extract_funct3 t0, instruction
	bx rv32_system_instruction_table[t0*4]
rv32_system_instruction_table:
	b rv32_ecall # ecall/ebreak
	b rv32_undefined_instruction # csrrw
	b rv32_undefined_instruction # csrrs
	b rv32_undefined_instruction # csrrc
	b rv32_undefined_instruction # undefined
	b rv32_undefined_instruction # csrrsi
	b rv32_undefined_instruction # csrrwi
	b rv32_undefined_instruction # csrrsi
	b rv32_undefined_instruction # csrrci
rv32_ecall:
	bbs 20, instruction, rv32_ebreak
	b next_instruction
rv32_ebreak:
	b next_instruction
# PRIMARY OPCODE: OP
rv32_op_primary:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	shri 20, instruction, immediate # compute the immediate with sign extension
	extract_funct3 t0, instruction
	bx rv32_op_instruction_table[t0*4]
rv32_op_instruction_table:
	b rv32_add # add, sub, or mul handled here
	b rv32_sll # sll or mulh handled here
	b rv32_slt # slt or mulhsu
	b rv32_sltu # sltu or mulhu
	b rv32_xor # xor or div
	b rv32_srl # sra, srl or divu
	b rv32_or  # 'or' | rem
	b rv32_and # and  | remu
# ADD - Add (check the funct7 code)
rv32_add:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	bbs 30, instruction, rv32_sub # check funct7 to see if we should do a subtract instead
	addo t0, t1, t1    # do the addition operation, use ordinal form to prevent integer overflow fault
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# SUB - Subtract x[rs1] - x[rs2] -> x[rd]
rv32_sub:
	subo t1, t0, t2		# x[rd] = x[rs1] - x[rs2] 
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# SLT - Set Less Than
rv32_slt:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	cmpi t0, t1		   # compare t0, t1
	testl t2		   # test to see if we got a less than
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_sltu:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	cmpo t0, t1		   # compare t0, t1
	testl t2		   # test to see if we got a less than
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_and:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	and t0, t1, t2    
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_or:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	or t0, t1, t2    
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
rv32_xor:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	xor t0, t1, t2    
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# SLL - Shift Left Logical
rv32_sll:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	shlo t0, t1, t2    
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# SRL - Shift Right Logical
rv32_srl:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1
	extract_rs2
	ld hart0_gpr_register_file[rs2*4], t1 # load rs2
	bbs 30, instruction, rv32_sra
	shro t0, t1, t2    
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# SRA - Shift Right Arithmetic
rv32_sra:
	shri t1, t0, t2 
	st t2, hart0_gpr_register_file[rd*4]
	b next_instruction
# PRIMARY OPCODE: OP-IMM
rv32_op_imm:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	shri 20, instruction, immediate # compute the immediate with sign extension
	extract_funct3 t0, instruction
	bx rv32_op_imm_instruction_table[t0*4]
rv32_op_imm_instruction_table:
	b rv32_addi
	b rv32_shift_left_immediate_dispatch # 0b001
	b rv32_slti
	b rv32_sltiu
	b rv32_xori
	b rv32_shift_right_immediate_dispatch # 0b101
	b rv32_ori
	b rv32_andi
# ADDI - Add Immediate
rv32_addi:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	addo immediate, t0, t1    # add rs1 with the immediate
	st t1, hart0_gpr_register_file[rd*4] # save to the register
	b next_instruction
# SLTI - Set Less Than Immediate (place the value 1 in register rd if register
#		rs1 is less than the sign-extended immediate when both are treated as
#		signed numbers)
rv32_slti:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	cmpi t0, immediate 	   # compare t0 to immediate 
	testl t1		   # check and see if t0 < immediate
	st t1, hart0_gpr_register_file[rd*4] # save the result to the dest register
	b next_instruction
rv32_sltiu:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	cmpo t0, immediate 	   # compare t0 to immediate (ordinal)
	testl t1		   # check and see if t0 < immediate
	st t1, hart0_gpr_register_file[rd*4] # save the result to the dest register
	b next_instruction
rv32_andi:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	and immediate, t0, t1    # add rs1 with the immediate
	st t1, hart0_gpr_register_file[rd*4] # save to the register
	b next_instruction
rv32_ori:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	or immediate, t0, t1    # add rs1 with the immediate
	st t1, hart0_gpr_register_file[rd*4] # save to the register
	b next_instruction
rv32_xori:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract_imm11_itype
	ld hart0_gpr_register_file[rs1*4], t0 # load rs1 contents
	xor immediate, t0, t1    # add rs1 with the immediate
	st t1, hart0_gpr_register_file[rd*4] # save to the register
	b next_instruction
rv32_shift_right_immediate_dispatch:
	bbs 30, instruction, rv32_srai
	b rv32_srli
rv32_shift_left_immediate_dispatch:
	bbc 30, instruction, rv32_slli
	b rv32_undefined_instruction
# SLLI - Shift Left Logical Immediate
rv32_slli:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract4 20, 5, instruction, t0 # extract imm[4:0]
	ld hart0_gpr_register_file[rs1*4], t1
	shlo t0, t1, t2        # do the shift left
	st t2, hart0_gpr_register_file[rd*4]	   # save the result
	b next_instruction
# SRLI - Shift Right Logical Immediate
rv32_srli:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract4 20, 5, instruction, t0 # extract imm[4:0]
	ld hart0_gpr_register_file[rs1*4], t1	   # get the contents of rs1
	shro t0, t1, t2        # do the shift right
	st t2, hart0_gpr_register_file[rd*4]	   # save the result
	b next_instruction
# SRAI - Shift Right Arithmetic Immediate
rv32_srai:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	extract4 20, 5, instruction, t0 # extract imm[4:0]
	ld hart0_gpr_register_file[rs1*4], t1	   # get the contents of rs1
	shri t0, t1, t2        # do the shift right (but do the integer version)
	st t2, hart0_gpr_register_file[rd*4]	   # save the result
	b next_instruction
# PRIMARY OPCODE: MISC-MEM
rv32_misc_mem:
	extract_rd
	skip_if_rd_is_x0 next_instruction
	extract_rs1
	shri 20, instruction, immediate # compute the immediate with sign extension
	extract_funct3 t0, instruction
	bx rv32_misc_mem_instruction_table[t0*4]
rv32_misc_mem_instruction_table:
	b rv32_fence
	b rv32_undefined_instruction # fence.i
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
rv32_fence:
	# do nothing right now
	ldconst 0x8330000F, t0
	cmpobe t0, instruction, rv32_fence_tso
	extract_rd
	extract_rs1
	# @todo implement fence instruction
	b next_instruction
rv32_fence_tso:
	# @todo implement fence.tso instruction
	b next_instruction
# PRIMARY OPCODE: BRANCH
rv32_branch_primary:
# imm[12|10:5]...imm[4:1|11]
	extract4 25, 6, instruction, t0 # imm10:5
	shlo 6, t0, t0 
	extract4 8, 4, instruction, t1 # imm4:1, skip imm11
	shlo 1, t1, t1
	or t0, t1, t0 # combine these two together
	chkbit 7, instruction # check imm11
	alterbit 11, t0, t0 # shift that over
	bbc 31, instruction, 1f # if most significant bit of instruction is clear then no need to shift over
	ldconst -4096, t1
	or t0, t1, t0 # combine with t0 to make negative
1:
	clrbit 0, t0, immediate # clear imm0 to do the proper alignment
	extract_rs1
	extract_rs2
	extract_funct3 t0, instruction
	bx rv32_branch_instruction_table[t0*4]
rv32_branch_instruction_table:
	b rv32_beq
	b rv32_bne
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_blt
	b rv32_bge
	b rv32_bltu
	b rv32_bgeu
rv32_beq:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpibe t0, t1, rv32_branch_taken
	b next_instruction
rv32_bne:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpibne t0, t1, rv32_branch_taken
	b next_instruction
rv32_blt:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpibl t0, t1, rv32_branch_taken
	b next_instruction
rv32_bltu:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpobl t0, t1, rv32_branch_taken
	b next_instruction
rv32_bge:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpibge t0, t1, rv32_branch_taken
	b next_instruction
rv32_bgeu:
	ld hart0_gpr_register_file[rs1*4], t0 # src1
	ld hart0_gpr_register_file[rs2*4], t1 # src2
	cmpobge t0, t1, rv32_branch_taken
	b next_instruction
rv32_branch_taken:
	addo pc, immediate, pc  
	b instruction_decoder_body
	
# Here are some of the extensions I want to implement
# RV32M extension
# RV32F extension
# RV32D extension
# Zilsd (Load/Store pair for RV32)
# ld - Load doubleword to even/odd register pair, 32-bit encoding
# sd - Store doubleword from even/odd register pair, 32-bit encoding
# B Extension (bit manipulation) [Zba, Zbb, Zbs]
# Zba -> address generation
# SH1ADD: Shift left by 1 and add
# lda (rs2)[rs1*2], rd
# SH2ADD: Shift left by 2 and add
# lda (rs2)[rs1*4], rd
# SH3ADD: Shift left by 3 and add
# lda (rs2)[rs1*8], rd
# Zbb -> basic bit manipulation
# logical with negate
# ANDN -> and with inverted operand
# andnot/notand depending on what is needed
# ORN -> or with inverted operand
# ornot/notor depending on what is needed
# XNOR -> exclusive nor
# use the xnor instruction
# Zbs -> single bit instructions
# bclr -> single-bit clear | clrbit
# bclri -> single-bit clear (immediate) | clrbit
# bext -> single-bit extract | chkbit + teste
# bexti -> single-bit extract (immediate) | chkbit + teste
# binv -> single-bit invert | notbit 
# binvi -> single-bit invert (immediate) | notbit
# bset -> single-bit set | setbit
# bseti -> single-bit set (immediate) | setbit
# Zicsr extension
# Zifencei extension?
# dispatch tables start
.align 6
rv32_direct_execution_dispatch_table:
# we cache the jump tables into the instruction cache by using jump instructions
# remember, the i960Sx does not have a data cache!
	b rv32_load_primary
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_misc_mem

	b rv32_op_imm
	b rv32_auipc
	b rv32_undefined_instruction
	b rv32_undefined_instruction

	b rv32_store_primary
	b rv32_lui
	b rv32_undefined_instruction
	b rv32_undefined_instruction

	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction

	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction

	b rv32_branch_primary
	b rv32_jalr
	b rv32_undefined_instruction
	b rv32_jal

	b rv32_system
	b rv32_undefined_instruction
	b rv32_undefined_instruction
	b rv32_undefined_instruction
.align 6
.global riscv_emulator_start
riscv_emulator_start:
	# clear all temporaries ahead of time
	mov 0, r3 
	movq 0, r4
	movq 0, r8
	movq 0, r12
	movq 0, g0 
	movq 0, g4
	movq 0, g8
	movt 0, g12
	b instruction_decoder_body
rv32_undefined_instruction:
next_instruction:
	addo pc, 4, pc
instruction_decoder_body:
	ld 0(pc), instruction # load the current instruction
	mov instruction, t0    # make a copy of it
	extract 0, 7, t0 # opcode extraction
	and 3, t0, t1 # check the lowest two bits
	cmpobne 3, t1, rv32_undefined_instruction # it is an illegal instruction
	shro 2, instruction, t0 # remove the lowest two bits since we know it is 0b11
	and 31, t0, t0 # now get the remaining 5 bits to figure out where to dispatch to
	# the lack of an onboard data cache means that constantly accessing the lookup table is actually extremely inefficient
	# the i960 has to talk to the AVR/RP2350 chip when it wants to access table data
	# instead, we should allow the dispatch table to be encoded into the onboard instruction cache by using instructions like cmpobe
	# however, this will introduce quite a bit of overhead for compare and dispatch
	bx rv32_direct_execution_dispatch_table[t0*4]
.bss hart0_gpr_register_file, (32*4), 6
