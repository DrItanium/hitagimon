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
/*
   The actual system that the i960Sx runs in is a little strange.
   It is a combination of an ATMEGA2560 (the 2560) and ATMEGA4808 (the 4808).
   There is 64 megabytes of PSRAM (Over a 5MHz SPI link) and a 1 Megabyte cache
   (two 512k*8bit SRAM chips). 

   The 4808 generates the clock signals for the entire system. Including:
   - 20MHz clock (from the internal oscillator) that drives the 2560 and the i960 (if configured as such)
   - 10MHz clock (from the internal oscillator fed into a CCL divider circuit)
   - 5MHz clock (from the 10MHz signal fed into another CCL divider circuit)

   In all cases, the 2560 will run at 20MHz. The i960Sx runs with a CLK2 of
   either 20 or 10MHz depending on a jumper. This translates to a CLK1 value of
   10 or 5MHz respectively which is the actual clock rate of the i960. I have been running
   the i960 at 5MHz CLK1 to make sure that the 2560 has the ability to execute
   up to four instructions for each i960 cycle. If I raise the CLK1 to 10MHz
   then it drops to two instructions instead. 

   This clock arrangement allows for all of the devices in the system to run clock synchronized. 

   The 2560 is responsible for:

   - Responding to memory requests by the i960 (this is the primary task)
   -- Includes detecting a new address state (uses one of the external interrupt lines but does not trigger an interrupt)
   -- Signalling ready uses some external hardware to allow toggling a pin to quickly tell the i960 precisely to continue execution
   -- Ready signalling also has a feedback pulse to tell the 2560 when to continue with execution (plus some timing chicanery)
   -- Some of the peripherals of the 2560 are exposed to the i960 through a MMIO scheme
   - Handling the SDCard
   -- This is used to install the initial memory image that the i960 runs
   - Interfacing with the parallel cache and PSRAM (EBI and SPI)
   -- 64 Megabytes of PSRAM are spread across eight different chips
   -- The Parallel cache is used to increase cache hit rate (more on this later since it is one of the major factors of why this emulator is coded how it is)
   - I2C access
   - SDCard Access
   - Serial Console Access

   For the riscv32 emulator, there are aspects of this design that have to be
   taken into account.  First, the i960Sx only has 512 bytes of instruction
   cache. There is no data cache at all! So, constantly accessing main memory
   is very expensive. The i960 instruction set has ways to load/store up to four
   registers worth of data at a time but that will incur an initial overhead. Thus, having jump tables
   is fine as long as you are jumping _to_ the table instead of loading the contents of the table into
   register. The former can actually be cached on chip since it is treated as code. 

   The parallel cache of the 2560 is a hybrid approach where the data lines and
   lowest five address lines of the 2560's EBI are connected to two 512k*8 SRAM
   chips. The lower four address lines are connected to A0-A3 with the remaining
   address line used to choose the SRAM chip to access. This means that there is a 32-byte window
   visible at any time to the 2560. The remaining address lines that the SRAM chips have are 
   connected directly to the i960's Address Lines. This greatly accelerates lookup time of cache
   lines by having the i960 automatically select the 32-byte window that the 2560 can access at any
   given time. This allows for a 16-byte cache line plus extra tracking data for each line. 

   A comparison is still needed to see if there is a cache miss. If there is,
   then the current cache line is saved back to PSRAM and the new cache line is
   loaded into the parallel cache. If the cache line on the chip has not been modified then
   only the new cache line is loaded in. The design of the cache is a direct
   mapped one but we at least have access to 512k out of the 1MB that is provided for this effort.
   Not all of the space is used either so it is flexible. 

   After the check, the 16-byte cache line is walked by the AVR for
   reads/writes during the memory transaction. The spin up time was around
   2.2us the last time I looked which isn't horrible but is still quite some
   overhead. The i960 will stall while it waits for its bus transactions to be
   completed.

   While we cannot keep the entire RV32 emulator state on chip, it is possible
   to hold onto a lot of it using the instruction cache and the 27 registers
   that are generically available (pfp, sp, rip, g14, and fp all have hardware
   requirements).  Right now, I have x1-x14 cached on chip (g0-g13) [remember
   x0 doesn't need to be cached].Thu Thus x15-x31 are saved to the register file
   space allocated in .bss.  Using the bss section allows me to always be able
   to access the state of the current hart and prevent conflicts with other
   harts (if I wanted to simulate more of them).

   Eventually, I want to do some register analysis to figure out which
   registers are the most frequently accessed.  This analysis will allow me to
   accelerate the most common operations. 

   So an instruction requiring more cycles but saving space is sometimes a
   better fit for the limited instruction cache. 

   I currently load each riscv instruction at the start of execution and then operate on it without
   going to main memory (unless I need to get register contents not directly mapped). This is actually
   a _very_ bad idea because I am constantly hitting main memory.

   Ideally, we want to load four instructions at a time since it only takes up one load instruction
   and makes it possible to not only identify 

   Each of these instructions need to be processed individually. In some cases, these individual 
   instructions can be fused into more complex instructions!

   The other thing that we have to contend with is the fact that there is a
   30-bit encoding space in the rv32 instructions. I do not support compressed instructions
   right now to keep the design as simple as possible. It also simplifies the design of this
   emulator as well:
   1. Check the lowest two bits of the instruction
   2. If they are not 0b11 then error out
   3. Otherwise, take the opcode field and shift right by two (creating a 32 entry distance)
   4. Jump to the branch statement found in the 32 entry jump table

   This increases flexibility while not nuking performance.

   Then we perform other actions to carry out the instruction to execute. 

   If I were to expand the first table to be based off of the lowest 12 or 15
   bits then it becomes possible to increase performance at the cost of increase program size.
   I could directly operate on specific combinations. The rs1 and rs2 fields
   would still be dynamically accessed...

   Right now, the idea to keep as much on chip as possible. Even having a faster memory subsystem
   would not totally alleviate this problem since the i960 itself has to perform the translation. 

   By using four registers for processing instructions, I would be able to stay off the memory bus
   for quite a long time. With the exception of cases where registers being accesssed are those
   which are on the stack...

   Since the C/C++ compiler is so janky, this emulator provides me the ability to use a widely
   supported instruction set that basically will never die.

   If we read four instructions at a time then we have some things to keep in mind:

   1. The idea of sequential execution can get very... interesting. The program counter will need
      to be advanced properly.
   2. Branches (and jumps) will require that we restart the four instructions
      to execute at the new destination
   3. When we run out of instructions to process, the next four instructions need to be loaded
   4. Keeping track of which instruction we are processing may require link register use
*/

/* 
   4/10/2025
	After re-reading the MC, Kx, and Sx manuals; I better understand how the
	scoreboarding system is setup. Register usage is being constantly tracked
	by the CPU's Instruction Execution Unit (IEU). 

	From the 80960KB Programmer's Reference Manual (note: I fixed some examples as they were broken...):

	"""
		The register scoreboard provides scoreboarding for the global and local reigsters. When, one 
		or more registers are being used in an operation, they are marked as in use. The register
		scoreboarding mechanism allows the processor to continue executing subsequent instructions,
		as long as those instructions do not require the contents of the scoreboarded registers.
	
		A typical even that would cause scoreboarding is a load operation. For a load from memory,
		the contents of the affected registers are not valid until the BCL fetches the data and the 
		registers are loaded. For example, consider the sequence:

		ld (g1), g0     # NOTE: The arguments were originally swapped
		addi g2, g3, g4
		addi g5, g4, g6
		subi g0, g6, g6

		Here, when the BCL initiates the ld operation, register g0 is scoreboarded. As long as
		subsequent instructions do not require the contents of g0, the ID continues to dispatch
		instructions. For example, the two addi instructions above are executed while the BCL is
		fetching the data for g0. If g0 is not loaded by the time the subi instruction is ready to
		be executed, the IEU delays execution of the instructions until the
		loading of g0 has been completed.
	"""

	This means it is far more important to organize the emulator code to not be
	stalled by the load operations. We should be actually starting the load and
	then continue execution while we wait. 

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
.macro load_and_return src, dest=r14
	mov \src, \dest
	bx (g14)
.endm

.text
# x0 - zero gcc never alloc'd
# x1 - ra (memory) gcc alloc: 15
# x2 - sp (memory) gcc never alloc'd
# x3 - gp (memory) gcc never alloc'd
# x4 - tp (memory) gcc never alloc'd
# x5 - t0 (memory) gcc alloc: 13
# x6 - t1 (memory) gcc alloc: 8
# x7 - t2 (memory) gcc alloc: 14
# x8 - s0 (memory) gcc alloc: 16
# x9 - s1 (memory) gcc alloc: 17
# x10 - a0 (memory) gcc alloc: 5
# x11 - a1 (memory) gcc alloc: 4
# x12 - a2 (memory) gcc alloc: 3
# x13 - a3 (memory) gcc alloc: 2
# x14 - a4 (memory) gcc alloc: 1
# x15 - a5 (memory) gcc alloc: 0
# x16 - a6 (memory)  gcc alloc: 6
# x17 - a7 (memory) gcc alloc: 7
# x18 - s2 (memory) gcc alloc: 18
# x19 - s3 (memory) gcc alloc: 19
# x20 - s4 (memory) gcc alloc: 20
# x21 - s5 (memory) gcc alloc: 21
# x22 - s6 (memory) gcc alloc: 22
# x23 - s7 (memory) gcc alloc: 23
# x24 - s8 (memory) gcc alloc: 24
# x25 - s9 (memory) gcc alloc: 25
# x26 - s10 (memory) gcc alloc: 26
# x27 - s11 (memory) gcc alloc: 27
# x28 - t3 (memory) gcc alloc: 9
# x29 - t4 (memory) gcc alloc: 10
# x30 - t5 (memory) gcc alloc: 11
# x31 - t6 (memory) gcc alloc: 12
# f0 - 
# f1 - 
# f2 - 
# f3 - 
# f4 - 
# f5 - 
# f6 - 
# f7 - 
# f8 - 
# f9 - 
# f10 - 
# f11 - 
# f12 - 
# f13 - 
# f14 - 
# f15 - 
# f16 - 
# f17 - 
# f18 - 
# f19 - 
# f20 - 
# f21 - 
# f22 - 
# f23 - 
# f24 - 
# f25 - 
# f26 - 
# f27 - 
# f28 - 
# f29 - 
# f30 - 
# f31 - 
rv32_abi_store_register:
	# rd - index
	# r14 - value
	cmpobe 0, rd, 1f # skip over if it is x0
	st r14, hart0_gprs[rd*4]
1:
	bx (g14)
rv32_abi_load_register_rs2:
	mov rs2, r14
	cmpobe 0, rs2, 1f # skip if x0
	ld hart0_gprs[rs2*4], r14
1:
	bx (g14)
rv32_abi_load_register_rs1:
	mov rs1, r14
	cmpobe 0, rs1, 1f # skip if x0
	ld hart0_gprs[rs1*4], r14
1:
	bx (g14)

rv32_abi_load_register:
	# r14 - index
	cmpobe 0, r14, 1f # skip if x0
	ld hart0_gprs[r14*4], r14
1:
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
	b rv32_save_r14_to_register_file   # save the result
# LUI - Load Upper Immediate
rv32_lui:
	extract_rd 						   
	shri 12, instruction, t0
	shro 12, t0, r14
	b rv32_save_r14_to_register_file   # save the result
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
	bal rv32_abi_load_register_rs1
	extract_rd
	shri 20, instruction, immediate # we want to make a sign extended version of imm[11:0]
	addo immediate, r14, rs1    # compute pc right now and stash in rs1
	addo 4, pc, r14 # pc+4		
	bal rv32_abi_store_register # x0 will be ignored
	mov rs1, pc # update pc after the store is completed
	# @todo what about unaligned accesses?
	b instruction_decoder_body
# PRIMARY OPCODE: LOAD
rv32_load_primary:
	extract_rs1
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
	bal rv32_abi_load_register_rs1 # base
	shri 20, instruction, immediate # compute the immediate with sign extension
	ldib (r14)[immediate], r14 # dest
	extract_rd
	b rv32_save_r14_to_register_file   # save the result
rv32_lh:
	bal rv32_abi_load_register_rs1 # base
	shri 20, instruction, immediate # compute the immediate with sign extension
	ldis (r14)[immediate], r14 # dest
	extract_rd
	b rv32_save_r14_to_register_file   # save the result
rv32_lw:
	bal rv32_abi_load_register_rs1 # base
	shri 20, instruction, immediate # compute the immediate with sign extension
	ld (r14)[immediate], r14 # dest
	extract_rd
	b rv32_save_r14_to_register_file   # save the result
rv32_lbu:
	bal rv32_abi_load_register_rs1 # base
	shri 20, instruction, immediate # compute the immediate with sign extension
	ldob (r14)[immediate], r14 # dest
	extract_rd
	b rv32_save_r14_to_register_file   # save the result
rv32_lhu:
	bal rv32_abi_load_register_rs1 # base
	shri 20, instruction, immediate # compute the immediate with sign extension
	ldos (r14)[immediate], r14 # dest
	extract_rd
	b rv32_save_r14_to_register_file   # save the result

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
	bal rv32_abi_load_register_rs1 # base
	mov r14, t0
	bal rv32_abi_load_register_rs2 # src
	stob r14, (t0)[immediate] # compute the address
	b next_instruction
rv32_sh:
	# rs1 -> base register index
	# rs2 -> src register index
	# g3 -> immediate
	bal rv32_abi_load_register_rs1 # base
	mov r14, t0
	bal rv32_abi_load_register_rs2 # src
	stos r14, (t0)[immediate] # compute the address
	b next_instruction
rv32_sw:
	# rs1 -> base register index
	# rs2 -> src register index
	# g3 -> immediate
	bal rv32_abi_load_register_rs1 # base
	mov r14, t0
	bal rv32_abi_load_register_rs2 # src
	st r14, (t0)[immediate] # compute the address
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
	# @todo implement ecall mechanism
	b next_instruction
rv32_ebreak:
	# @todo implement ebreak mechanism
	b next_instruction
# PRIMARY OPCODE: OP
rv32_op_primary:
	extract_rd
	extract_rs1
	extract_rs2
	shro 25, instruction, immediate # extract funct7
	extract_funct3 t0, instruction
	cmpobe 1, immediate, 1f
	bx rv32_op_instruction_table[t0*4]
1: # for the M extension
	bx rv32_op_instruction_table2[t0*4]
rv32_op_instruction_table:
	b rv32_add # add, sub, or mul handled here
	b rv32_sll # sll or mulh handled here
	b rv32_slt # slt or mulhsu
	b rv32_sltu # sltu or mulhu
	b rv32_xor # xor or div
	b rv32_srl # sra, srl or divu
	b rv32_or  # 'or' | rem
	b rv32_and # and  | remu
rv32_op_instruction_table2:
	b rv32_mul
	b rv32_mulh
	b rv32_mulhsu
	b rv32_mulhu
	b rv32_div
	b rv32_divu
	b rv32_rem
	b rv32_remu
# ADD - Add (check the funct7 code)
rv32_add:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	bbs 30, instruction, rv32_sub
	addi t0, r14, r14
	b rv32_save_r14_to_register_file   # save the result
# SUB - Subtract x[rs1] - x[rs2] -> x[rd]
rv32_sub:
	subi r14, t0, r14		# x[rd] = x[rs1] - x[rs2] 
	b rv32_save_r14_to_register_file   # save the result
rv32_mul:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	muli r14, t0, r14
	b rv32_save_r14_to_register_file
rv32_mulh:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	emul r14, t0, r14 # r15 and r14 will be overwritten with this
	mov r15, r14      # move upper half to r14
	b rv32_save_r14_to_register_file
rv32_mulhsu:
	b next_instruction
rv32_mulhu:
	b next_instruction
rv32_div:
	b next_instruction
rv32_divu:
	b next_instruction
rv32_rem:
	b next_instruction
rv32_remu:
	b next_instruction
	

# SLT - Set Less Than
rv32_slt:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	cmpi t0, r14		   # compare t0, t1
	testl r14		   # test to see if we got a less than
	b rv32_save_r14_to_register_file   # save the result
rv32_sltu:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	cmpo t0, r14		   # compare t0, t1
	testl r14		   # test to see if we got a less than
	b rv32_save_r14_to_register_file   # save the result
rv32_and:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	and t0, r14, r14    
	b rv32_save_r14_to_register_file   # save the result
rv32_or:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	or t0, r14, r14    
	b rv32_save_r14_to_register_file   # save the result
rv32_xor:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	xor t0, r14, r14    
	b rv32_save_r14_to_register_file   # save the result
# SLL - Shift Left Logical
rv32_sll:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	and 31, r14, t1 # the lowest 5 bits
	shlo t1, t0, r14
	b rv32_save_r14_to_register_file   # save the result
# SRL - Shift Right Logical
# rs1 -> value
# rs2 -> shift amount
rv32_srl:
	bal rv32_abi_load_register_rs1
	mov r14, t0
	bal rv32_abi_load_register_rs2
	and 31, r14, t1 # the lowest 5 bits
	bbs 30, instruction, rv32_sra
	shro t1, t0, r14
	b rv32_save_r14_to_register_file
# SRA - Shift Right Arithmetic
rv32_sra:
	shri t1, t0, r14
	b rv32_save_r14_to_register_file
# PRIMARY OPCODE: OP-IMM
rv32_op_imm:
	extract_rd
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
	bal rv32_abi_load_register_rs1 # rs1 contents
	addi immediate, r14, r14    # add rs1 with the immediate
	b rv32_save_r14_to_register_file
# SLTI - Set Less Than Immediate (place the value 1 in register rd if register
#		rs1 is less than the sign-extended immediate when both are treated as
#		signed numbers)
rv32_slti:
	bal rv32_abi_load_register_rs1 # rs1 contents
	cmpi r14, immediate 	   # compare t0 to immediate 
	testl r14		   # check and see if t0 < immediate
	b rv32_save_r14_to_register_file
rv32_sltiu:
	bal rv32_abi_load_register_rs1 # rs1 contents
	cmpo r14, immediate 	   # compare t0 to immediate (ordinal)
	testl r14		   # check and see if t0 < immediate
	b rv32_save_r14_to_register_file
rv32_andi:
	bal rv32_abi_load_register_rs1 # rs1 contents
	and immediate, r14, r14    # add rs1 with the immediate
	b rv32_save_r14_to_register_file
rv32_ori:
	bal rv32_abi_load_register_rs1 # rs1 contents
	or immediate, r14, r14    # add rs1 with the immediate
	b rv32_save_r14_to_register_file
rv32_xori:
	bal rv32_abi_load_register_rs1 # rs1 contents
	xor immediate, r14, r14    # add rs1 with the immediate
	b rv32_save_r14_to_register_file
rv32_shift_right_immediate_dispatch:
	and 31, immediate, t0 # extract imm[4:0]
	bal rv32_abi_load_register_rs1
	bbs 30, instruction, rv32_srai
# SRLI - Shift Right Logical Immediate
	shro t0, r14, r14
	b rv32_save_r14_to_register_file
# SRAI - Shift Right Arithmetic Immediate
rv32_srai:
	shri t0, r14, r14
	b rv32_save_r14_to_register_file
rv32_shift_left_immediate_dispatch:
	bbs 30, instruction, rv32_undefined_instruction
# SLLI - Shift Left Logical Immediate
	and 31, immediate, t0 # extract imm[4:0]

	bal rv32_abi_load_register_rs1
	shlo t0, r14, r14        # do the shift left
	b rv32_save_r14_to_register_file

# PRIMARY OPCODE: MISC-MEM
rv32_misc_mem:
	extract_rd
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
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpibe t0, r14, rv32_branch_taken
	b next_instruction
rv32_bne:
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpibne t0, r14, rv32_branch_taken
	b next_instruction
rv32_blt:
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpibl t0, r14, rv32_branch_taken
	b next_instruction
rv32_bltu:
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpobl t0, r14, rv32_branch_taken
	b next_instruction
rv32_bge:
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpibge t0, r14, rv32_branch_taken
	b next_instruction
rv32_bgeu:
	bal rv32_abi_load_register_rs1 # rs1 contents
	mov r14, t0					   # 
	bal rv32_abi_load_register_rs2 # rs2 contents
	cmpobge t0, r14, rv32_branch_taken
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
# leave fp alone
# turn off interger overflow faults to make it easier to better adapt to how rv32 does things
	modac 0, 0, r3 # get the contents of ac
	setbit 12, r3, r3 # mask integer overflow faults
	setbit 15, r3, r3 # no imprecise faults (doesn't really do anything on i960Sx but make sure)
	modac r3, r3, r3 # update arithmetic controls
	b instruction_decoder_body

rv32_save_r14_to_register_file:
	bal rv32_abi_store_register 	   # store it to the register file (this will cause x0 to be ignored)
	b next_instruction
rv32_undefined_instruction:
next_instruction:
	addo pc, 4, pc
instruction_decoder_body:
	# normally the idea would be to read each instruction individually but this is _very_ expensive overall when talking to the 
	# AVR chipset. Each time a request is made, you pay at least a 2.2us wait for the transaction to spin up. It is even longer
	# on a cache miss. 
	# 
	# 
	ld 0(pc), instruction # load the current instruction, this is a single point of stalling...
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
rv32_direct_execution_dispatch_table:
# we cache the jump tables into the instruction cache by using jump instructions
# remember, the i960Sx does not have a data cache!
	b rv32_load_primary
	b rv32_undefined_instruction # load-fp
	b rv32_undefined_instruction # custom-0
	b rv32_misc_mem
	b rv32_op_imm 
	b rv32_auipc
	b rv32_undefined_instruction # op-imm-32
	b rv32_undefined_instruction # 48b
	b rv32_store_primary		 
	b rv32_undefined_instruction # store-fp
	b rv32_undefined_instruction # custom-1
	b rv32_undefined_instruction # amo
	b rv32_op_primary 
	b rv32_lui
	b rv32_undefined_instruction # op-32
	b rv32_undefined_instruction # 64b
	b rv32_undefined_instruction # madd
	b rv32_undefined_instruction # msub
	b rv32_undefined_instruction # nmadd
	b rv32_undefined_instruction # op-fp
	b rv32_undefined_instruction # op-v
	b rv32_undefined_instruction # custom-2/rv128
	b rv32_undefined_instruction # 48b
	b rv32_branch_primary
	b rv32_jalr
	b rv32_undefined_instruction # reserved
	b rv32_jal
	b rv32_system
	b rv32_undefined_instruction # op-ve
	b rv32_undefined_instruction # custom-3/rv128
	b rv32_undefined_instruction # >=80b
	.bss hart0_gprs, (32*4), 6
	.bss hart0_fprs, (32*8), 6

