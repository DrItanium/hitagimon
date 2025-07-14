/*
hitagimon
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

.global InterruptContextEnter
# in order to properly emulate the software stack that grows downward, we need to know how we 
# here to prevent damage to the interrupt and supervisor call stacks

# we will always need to know where the top of the stack is for each environment. 
# If we have multiple stacks to worry about then it becomes necessary to
# determine if a top of stack update will be necessary during the execution of the stack


InterruptContextEnter:
	# remember that sp is the hardware stack not the software stack
	# first, we need to stash the global registers here, this is an interrupt
	# so there is no direct state to worry about. Saving this information here
    # on the hardware stack means that we are creating a check point that can be easily used
	ldconst 64, r3 
	addo sp, r3, sp
	stq g0, -64(sp)
	stq g4, -48(sp)
	stq g8, -32(sp)
	stt g12, -16(sp)
	# now that the globals are saved, we need to check and see if the software stack should come from memory or not... 
	ldt -16(fp), r4 # now get the contents of the interrupt record from the stack
	# r4 -> saved process controls
	# r5 -> saved arithmetic controls
	# r6 -> the interrupt vector number
	# r7 -> unused and free
	bbs 13, r4, 1f # inspect bit 13 of saved process controls, if the saved process controls is 1 then we were already in an interrupted context
	# exec -> interrupt
	# g12 -> execution environment
	# g13 -> rv32 stack pointer
	ld 12(g12), g13
1:  # we are already on the interrupt stack

	# invocation logic goes here
	# at this point we restore our globals from the previous event
	# ideally, at this point our software stack will be back to where g13
    # started previously was so there is no need to worry about it. 
	ldq -64(sp), g0
	ldq -48(sp), g4
	ldq -32(sp), g8
	ldt -16(sp), g12
	ret # drop back to previous stack/context (equivalent to mret/sret etc)


# there are several cases to worry about with emulating an rv32 style setup
# 1. Later on, it may be desired to have separate user/kernel/interrupt stacks per execution environment
#    At that point, it will become necessary to figure out how to keep the other call stacks safe to allow for pausing
# -- However, that may not be necessary when dealing with interrupts. Either we are servicing a new interrupt from user/kernel context
# 	 or we are already in kernel context. The former we will be starting from scratch. 
#    In the later the stack is already setup.
#
# All of the functions which an interrupt context could call do not care where the stack is located or what it is. So "user" code is fine. 
# the question is if I need to preserve the top of the stack and save that to the context...

	.global FaultContextEnter
# faults are basically system exceptions
# I will need to figure out how RV32 handles these
FaultContextEnter:
#	ldconst 64, r3
#	addo sp, r3, sp
#	stq g0, -64(sp)
#	stq g4, -48(sp)
#	stq g8, -32(sp)
#	stt g12, -16(sp)
#
#	ldq -64(sp), g0
#	ldq -48(sp), g4
#	ldq -32(sp), g8
#	ldt -16(sp), g12
	ret
	
.global SystemCallEnter
# system calls are special because the global arguments need to passed and
# returned so it will be necessary to only save the necessary pieces that are not the argument registers
# what needs to happen with system calls is that we save the current stack contents followed by a balx over to the actual function itself
# what we need to check and see is if we have performed a full switch via the pfp (r0)
SystemCallEnter:
	ldconst 64, r3  # even though we probably won't use all of it make sure that the stack frame has enough space for our purposes
					# we will be wasting space overall
	addo sp, r3, sp # allocate space on the hardware stack frame
					# registers g6-g14 need to be saved to the stack, 
					# the lower eight globals are mapped to the argument registers (a0-a7, although technically we really only have access to a0-a5 from rv32e)
					# we want easy restore so use the alignment from the full 16 register saves
	stl g6, -48(sp) # save to the lower half of the 16-byte allocation because lazy
	stq g8, -32(sp) # save g8-g11
	stt g12, -16(sp) # stash the rest of the globals (minus g15/fp since it is safe to leave around)
	# actual code goes here
	ldl -48(sp), g6 
	ldq -32(sp), g8
	ldt -16(sp), g12
	ret	# return from system call

# The i960's hardware stack is practically useless for riscv32 like emulation
# because it allocates stack frames backwards compared to most other
# architectures. It goes from low addresses to high addresses which sucks for several reasons:
# 1) Downward growing stacks are more natural in design and most other architectures use it
# 2) Detecting stack and heap overflows requires more checks and placement work
# 3) When saving registers to the stack you first have to allocate the space on the hardware stack and then use negative offsets on loads and stores which is less efficient

	/*
* While the first point is more of a convention, it does make it harder to
* reason about converting assembly from other architectures over so bugs can
* present themselves in a given translation. But it isn't just that, the
* natural downward growth of the stack in other architectures allows for easy
* detection of heap and stack overflows. The two structures grow towards each
* other and it becomes trivial to detect the overflow when they collide.
* With the hardware stack growing towards high addresses it becomes necessary
* to either place the stack in a location that will hit the bottom part of the
* heap or vice versa. The problem with that design is only once of them can be
* efficiently detected. The heap has to be modified to grow downward towards
* the stack.
* 
* It also doesn't take into account that the three different stacks need to
* also be positioned such that they can't collide into each other. Although
* this is an issue that will be encountered on any target.
* 
* The final issue with having an upward growing stack is one that is somewhat
* unique to the i960. When performing a call, you just get a new stack frame
* (even if you don't want one [the idea is to use bal/balx instead]) and you
* have to add to the stack pointer to allocate space for use. Failure to do
* this will result in data corruption as the i960 doesn't know the data above
* the stack pointer is important. Thus it will not factor it into its stack
* offset calculation on the next call/callx operation. 
*
* So at the top of most i960 routines, you will first see the stack get bumped
* up first to increase the size of the frame. It makes sure that
* any interrupts, faults, or system calls will honor the frame contents. 
*
* Then the function is free to save the contents of registers to the stack and
* this is where the biggest problem is. Since the stack frame has been
* allocated, it is necessary to use negative offsets to refer to elements on
* the stack. This results in two word instructions being used instead of one
* word instructions for saving and restoring values from the stack. Generally,
* this isn't usually a problem but can result in problems with instruction
* cache misalignment where the processor may have to stall if the second word
* of the instruction is in the next cache line. If it is a cache line miss then
* it will require several things to happen:
* 1) A new bus request will need to be posted to load the 16-byte cache line
* 2) If the bus queue is full then the processor will just stall out and wait
* until a free entry in the queue is available.
* 3) If the bus queue isn't full then the posting will go ahead but will still
* stall until it can get the line into the instruction cache. Then execution
* will continue.
*
* This is a huge problem that is inherent in the i960 design itself. Use of
* local registers is meant to mitigate some of this but if you're using
* bal/balx then saving and restoring registers will just be happening. 
*
* It is possible to use the frame pointer instead to do a positive offset but
* you will need to make sure that you add 64 to every offset to skip past the
* local register area of the stack frame. It isn't perfect but potentially
* better. You are also at the mercy of the assembler to properly encode MEMA
* instructions if you are within 4k of the frame pointer start. If the stack
* increases beyond that then you are back to this same problem. You could also
* use the abase+index*2^scale form to save a word but that requires careful
* manipulation of the registers which increases the number of instructions.
*
* The simplest and most straightforward path to using the stack requires that
* you use double width instructions. I think this is why the quad/triple/long
* versions of load, store, and move exist. Being able to operate on 16/12/8 bytes at
* a time means that any instruction cache misses are not as severe. 
*
*/
