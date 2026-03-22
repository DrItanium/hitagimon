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
	mov r5, r6 # just keep doing this over and over
	cmpdeco 0, g0, g0 # decrement g0
	bne ordinalRegisterToRegisterMoveTest
	ret

longOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
0:
	movl r4, r6
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
tripleOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
0:
	movt r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

quadOrdinalRegisterToRegisterMoveTest:
	mov 0, r3
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

# determine how fast movr r4, fp0 is
moveRealTest2:
	# g0 - count
	mov 0, r3
0:
	movr r4, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr fp0, r4 is
moveRealTest3:
	# g0 - count
	mov 0, r3
0:
	movr fp0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
.global moveRealTest4
# determine how fast movr fp0, fp1 is
moveRealTest4:
	# g0 - count
	mov 0, r3
0:
	movr fp0, fp1
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

.global moveLongRealTest0
.global moveLongRealTest1
.global moveLongRealTest2
.global moveLongRealTest3

# determine how fast movr 0.0f, r4 is
moveLongRealTest0:
	# g0 - count
	mov 0, r3
0:
	movrl 0.0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr 0.0f, fp0 is
moveLongRealTest1:
	# g0 - count
	mov 0, r3
0:
	movrl 0.0, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr r4, fp0 is
moveLongRealTest2:
	# g0 - count
	mov 0, r3
0:
	movrl r4, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr fp0, r4 is
moveLongRealTest3:
	# g0 - count
	mov 0, r3
0:
	movrl fp0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
.global moveLongRealTest4
# determine how fast movr fp0, fp1 is
moveLongRealTest4:
	# g0 - count
	mov 0, r3
0:
	movrl fp0, fp1
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

.global moveExtendedRealTest0
.global moveExtendedRealTest1
.global moveExtendedRealTest2
.global moveExtendedRealTest3

# determine how fast movr 0.0f, r4 is
moveExtendedRealTest0:
	# g0 - count
	mov 0, r3
0:
	movre 0.0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr 0.0f, fp0 is
moveExtendedRealTest1:
	# g0 - count
	mov 0, r3
0:
	movre 0.0, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr r4, fp0 is
moveExtendedRealTest2:
	# g0 - count
	mov 0, r3
0:
	movre r4, fp0
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

# determine how fast movr fp0, r4 is
moveExtendedRealTest3:
	# g0 - count
	mov 0, r3
0:
	movre fp0, r4
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
.global moveExtendedRealTest4
# determine how fast movr fp0, fp1 is
moveExtendedRealTest4:
	# g0 - count
	mov 0, r3
0:
	movre fp0, fp1
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

.global moveRealTest5
.global moveLongRealTest5
.global moveExtendedRealTest5


moveExtendedRealTest5:
	# g0 - count
	mov 0, r3
0:
	movre r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
# determine how fast movre r4, r8 is
moveLongRealTest5:
	# g0 - count
	mov 0, r3
0:
	movrl r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret

moveRealTest5:
	# g0 - count
	mov 0, r3
0:
	movr r4, r8
	addo r3, 1, r3
	cmpo r3, g0
	bne 0b
	ret
