# hitagimon
# Copyright (c) 2020-2026, Joshua Scoggins
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
# ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

.global ordinalRegisterToRegisterMoveTest
.global longOrdinalRegisterToRegisterMoveTest
.global tripleOrdinalRegisterToRegisterMoveTest
.global quadOrdinalRegisterToRegisterMoveTest
.text
.align 4
# we want this code to be as tight as possible
ordinalRegisterToRegisterMoveTest:
	# g0 - number of times to perform the move
	mov 0, r4
	ldconst 0xFDEC, r5 # we are going to move from r5 to r6
	ldconst 0, r6 # clear r6
0:
	mov r5, r6 # just keep doing this over and over
	addo r4, 1, r4
	cmpo r4, g0
# there is intentional dead space here
	bne 0b 
	ret

longOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
	ldconst 0x01234567, r4
	ldconst 0x89ABCDEF, r5
	movl 0, r6
0:
	movl r4, r6
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
tripleOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
	ldconst 0x01234567, r4
	ldconst 0x89ABCDEF, r5
	ldconst 0x0F1E2D3C, r6
	ldconst 0, r7
0:
	movt r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

quadOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
	ldconst 0x01234567, r4
	ldconst 0x89ABCDEF, r5
	ldconst 0x0F1E2D3C, r6
	ldconst 0x4B5A6978, r7
0:
	movq r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

.global moveRealTest0
.global moveRealTest1
.global moveRealTest2
.global moveRealTest3

# determine how fast movr 0.0f, r4 is
moveRealTest0:
	# g0 - count
	mov 0, r3
0:
	movr 0.0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr 0.0f, fp0 is
moveRealTest1:
	# g0 - count
	mov 0, r3
0:
	movr 0.0, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
