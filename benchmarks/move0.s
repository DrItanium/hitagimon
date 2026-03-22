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

.text
.align 4
# this code needs to be as tight as possible
.macro DefSingleInstructionOperation name, operation, arg0, arg1
.global \name
\name:
	\operation \arg0 , \arg1
	cmpdeco 0, g0, g0
	bne \name
	ret
.endm

DefSingleInstructionOperation ordinalRegisterToRegisterMoveTest, mov, r4, r8
DefSingleInstructionOperation longOrdinalRegisterToRegisterMoveTest, movl, r4, r8
DefSingleInstructionOperation tripleOrdinalRegisterToRegisterMoveTest, movt, r4, r8
DefSingleInstructionOperation quadOrdinalRegisterToRegisterMoveTest, movq, r4, r8
DefSingleInstructionOperation moveRealTest0, movr, 0.0, r4
DefSingleInstructionOperation moveRealTest1, movr, 0.0, fp0
DefSingleInstructionOperation moveRealTest2, movr, r4, fp0
DefSingleInstructionOperation moveRealTest3, movr, fp0, r4
DefSingleInstructionOperation moveRealTest4, movr, fp0, fp1
DefSingleInstructionOperation moveRealTest5, movr, r4, r8

DefSingleInstructionOperation moveLongRealTest0, movrl, 0.0, r4
DefSingleInstructionOperation moveLongRealTest1, movrl, 0.0, fp0
DefSingleInstructionOperation moveLongRealTest2, movrl, r4, fp0
DefSingleInstructionOperation moveLongRealTest3, movrl, fp0, r4
DefSingleInstructionOperation moveLongRealTest4, movrl, fp0, fp1
DefSingleInstructionOperation moveLongRealTest5, movrl, r4, r8

DefSingleInstructionOperation moveExtendedRealTest0, movre, 0.0, r4
DefSingleInstructionOperation moveExtendedRealTest1, movre, 0.0, fp0
DefSingleInstructionOperation moveExtendedRealTest2, movre, r4, fp0
DefSingleInstructionOperation moveExtendedRealTest3, movre, fp0, r4
DefSingleInstructionOperation moveExtendedRealTest4, movre, fp0, fp1
DefSingleInstructionOperation moveExtendedRealTest5, movre, r4, r8



