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
FaultContextEnter:
	ldconst 64, r3
	addo sp, r3, sp
	stq g0, -64(sp)
	stq g4, -48(sp)
	stq g8, -32(sp)
	stt g12, -16(sp)

	ldq -64(sp), g0
	ldq -48(sp), g4
	ldq -32(sp), g8
	ldt -16(sp), g12
	ret
	
