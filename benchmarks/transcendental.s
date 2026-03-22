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

# simple tests around floating point transcendental functions

.include "boot/macros.s"
.text

DefSingleInstructionOperation testCosine0, cosr, 0.0, r4
DefSingleInstructionOperation testCosine1, cosr, 0.0, fp0
DefSingleInstructionOperation testCosine2, cosr, r4, r4
DefSingleInstructionOperation testCosine3, cosr, fp0, fp0
DefSingleInstructionOperation testCosine4, cosr, r4, fp0
DefSingleInstructionOperation testCosine5, cosr, fp0, r4
DefSingleInstructionOperation testLongCosine0, cosrl, 0.0, r4
DefSingleInstructionOperation testLongCosine1, cosrl, 0.0, fp0
DefSingleInstructionOperation testLongCosine2, cosrl, r4, r4
DefSingleInstructionOperation testLongCosine3, cosrl, fp0, fp0
DefSingleInstructionOperation testLongCosine4, cosrl, r4, fp0
DefSingleInstructionOperation testLongCosine5, cosrl, fp0, r4

DefSingleInstructionOperation testSine0, sinr, 0.0, r4
DefSingleInstructionOperation testSine1, sinr, 0.0, fp0
DefSingleInstructionOperation testSine2, sinr, r4, r4
DefSingleInstructionOperation testSine3, sinr, fp0, fp0
DefSingleInstructionOperation testSine4, sinr, r4, fp0
DefSingleInstructionOperation testSine5, sinr, fp0, r4
DefSingleInstructionOperation testLongSine0, sinrl, 0.0, r4
DefSingleInstructionOperation testLongSine1, sinrl, 0.0, fp0
DefSingleInstructionOperation testLongSine2, sinrl, r4, r4
DefSingleInstructionOperation testLongSine3, sinrl, fp0, fp0
DefSingleInstructionOperation testLongSine4, sinrl, r4, fp0
DefSingleInstructionOperation testLongSine5, sinrl, fp0, r4

DefSingleInstructionOperation testTangent0, tanr, 0.0, r4
DefSingleInstructionOperation testTangent1, tanr, 0.0, fp0
DefSingleInstructionOperation testTangent2, tanr, r4, r4
DefSingleInstructionOperation testTangent3, tanr, fp0, fp0
DefSingleInstructionOperation testTangent4, tanr, r4, fp0
DefSingleInstructionOperation testTangent5, tanr, fp0, r4
DefSingleInstructionOperation testLongTangent0, tanrl, 0.0, r4
DefSingleInstructionOperation testLongTangent1, tanrl, 0.0, fp0
DefSingleInstructionOperation testLongTangent2, tanrl, r4, r4
DefSingleInstructionOperation testLongTangent3, tanrl, fp0, fp0
DefSingleInstructionOperation testLongTangent4, tanrl, r4, fp0
DefSingleInstructionOperation testLongTangent5, tanrl, fp0, r4

DefSingleInstructionOperation testSquareRoot0, sqrtr, 0.0, r4
DefSingleInstructionOperation testSquareRoot1, sqrtr, 0.0, fp0
DefSingleInstructionOperation testSquareRoot2, sqrtr, r4, r4
DefSingleInstructionOperation testSquareRoot3, sqrtr, fp0, fp0
DefSingleInstructionOperation testSquareRoot4, sqrtr, r4, fp0
DefSingleInstructionOperation testSquareRoot5, sqrtr, fp0, r4
DefSingleInstructionOperation testLongSquareRoot0, sqrtrl, 0.0, r4
DefSingleInstructionOperation testLongSquareRoot1, sqrtrl, 0.0, fp0
DefSingleInstructionOperation testLongSquareRoot2, sqrtrl, r4, r4
DefSingleInstructionOperation testLongSquareRoot3, sqrtrl, fp0, fp0
DefSingleInstructionOperation testLongSquareRoot4, sqrtrl, r4, fp0
DefSingleInstructionOperation testLongSquareRoot5, sqrtrl, fp0, r4

DefSingleInstructionOperation testExponent0, expr, 0.0, r4
DefSingleInstructionOperation testExponent1, expr, 0.0, fp0
DefSingleInstructionOperation testExponent2, expr, r4, r4
DefSingleInstructionOperation testExponent3, expr, fp0, fp0
DefSingleInstructionOperation testExponent4, expr, r4, fp0
DefSingleInstructionOperation testExponent5, expr, fp0, r4
DefSingleInstructionOperation testLongExponent0, exprl, 0.0, r4
DefSingleInstructionOperation testLongExponent1, exprl, 0.0, fp0
DefSingleInstructionOperation testLongExponent2, exprl, r4, r4
DefSingleInstructionOperation testLongExponent3, exprl, fp0, fp0
DefSingleInstructionOperation testLongExponent4, exprl, r4, fp0
DefSingleInstructionOperation testLongExponent5, exprl, fp0, r4

.global circleWalkCosineReal0
.global circleWalkCosineReal1
.global circleWalkCosineLongReal0
.global circleWalkCosineLongReal1

.global circleWalkSineReal0
.global circleWalkSineReal1
.global circleWalkSineLongReal0
.global circleWalkSineLongReal1

.global circleWalkTangentReal0
.global circleWalkTangentReal1
.global circleWalkTangentLongReal0
.global circleWalkTangentLongReal1

.global circleWalkSquareRootReal0
.global circleWalkSquareRootReal1
.global circleWalkSquareRootLongReal0
.global circleWalkSquareRootLongReal1

.global circleWalkRoundReal0
.global circleWalkRoundReal1
.global circleWalkRoundLongReal0
.global circleWalkRoundLongReal1

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkCosineReal0:
	# g0 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, r3 # convert it to a real
	movr 0.0, r4 # load r4
0:
	cosr r4, r5 # gpr to gpr operation
	cmpr r4, r3 
	addr r4, g0, r4 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkCosineReal1:
	# g0 - increment amount (real)
	movr g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movr 0.0, fp2 
0:
	cosr fp2, fp3 
	cmpr fp2, fp1
	addr fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkCosineLongReal0:
	# g0,g1 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, fp0 # convert it to a long real via fp0
	movrl fp0, r4 # then stash the result into r4 so now we have 360.0000 in long real form
	movrl 0.0, r6 # load r4
0:
	cosrl r6, r8 # gpr to gpr operation
	cmprl r6, r4 # compare 
	addrl r6, g0, r6 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkCosineLongReal1:
	# g0,g1 - increment amount (real)
	movrl g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movrl 0.0, fp2 
0:
	cosrl fp2, fp3 
	cmprl fp2, fp1
	addrl fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkSineReal0:
	# g0 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, r3 # convert it to a real
	movr 0.0, r4 # load r4
0:
	sinr r4, r5 # gpr to gpr operation
	cmpr r4, r3 
	addr r4, g0, r4 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkSineReal1:
	# g0 - increment amount (real)
	movr g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movr 0.0, fp2 
0:
	sinr fp2, fp3 
	cmpr fp2, fp1
	addr fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkSineLongReal0:
	# g0,g1 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, fp0 # convert it to a long real via fp0
	movrl fp0, r4 # then stash the result into r4 so now we have 360.0000 in long real form
	movrl 0.0, r6 # load r4
0:
	sinrl r6, r8 # gpr to gpr operation
	cmprl r6, r4 # compare 
	addrl r6, g0, r6 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkSineLongReal1:
	# g0,g1 - increment amount (real)
	movrl g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movrl 0.0, fp2 
0:
	sinrl fp2, fp3 
	cmprl fp2, fp1
	addrl fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkTangentReal0:
	# g0 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, r3 # convert it to a real
	movr 0.0, r4 # load r4
0:
	tanr r4, r5 # gpr to gpr operation
	cmpr r4, r3 
	addr r4, g0, r4 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkTangentReal1:
	# g0 - increment amount (real)
	movr g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movr 0.0, fp2 
0:
	tanr fp2, fp3 
	cmpr fp2, fp1
	addr fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkTangentLongReal0:
	# g0,g1 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, fp0 # convert it to a long real via fp0
	movrl fp0, r4 # then stash the result into r4 so now we have 360.0000 in long real form
	movrl 0.0, r6 # load r4
0:
	tanrl r6, r8 # gpr to gpr operation
	cmprl r6, r4 # compare 
	addrl r6, g0, r6 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkTangentLongReal1:
	# g0,g1 - increment amount (real)
	movrl g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movrl 0.0, fp2 
0:
	tanrl fp2, fp3 
	cmprl fp2, fp1
	addrl fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkSquareRootReal0:
	# g0 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, r3 # convert it to a real
	movr 0.0, r4 # load r4
0:
	sqrtr r4, r5 # gpr to gpr operation
	cmpr r4, r3 
	addr r4, g0, r4 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkSquareRootReal1:
	# g0 - increment amount (real)
	movr g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movr 0.0, fp2 
0:
	sqrtr fp2, fp3 
	cmpr fp2, fp1
	addr fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkSquareRootLongReal0:
	# g0,g1 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, fp0 # convert it to a long real via fp0
	movrl fp0, r4 # then stash the result into r4 so now we have 360.0000 in long real form
	movrl 0.0, r6 # load r4
0:
	sqrtrl r6, r8 # gpr to gpr operation
	cmprl r6, r4 # compare 
	addrl r6, g0, r6 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkSquareRootLongReal1:
	# g0,g1 - increment amount (real)
	movrl g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movrl 0.0, fp2 
0:
	sqrtrl fp2, fp3 
	cmprl fp2, fp1
	addrl fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkRoundReal0:
	# g0 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, r3 # convert it to a real
	movr 0.0, r4 # load r4
0:
	roundr r4, r5 # gpr to gpr operation
	cmpr r4, r3 
	addr r4, g0, r4 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkRoundReal1:
	# g0 - increment amount (real)
	movr g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movr 0.0, fp2 
0:
	roundr fp2, fp3 
	cmpr fp2, fp1
	addr fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using just gprs
circleWalkRoundLongReal0:
	# g0,g1 - increment amount (real)
	ldconst 360, r3 # end circle
	cvtir r3, fp0 # convert it to a long real via fp0
	movrl fp0, r4 # then stash the result into r4 so now we have 360.0000 in long real form
	movrl 0.0, r6 # load r4
0:
	roundrl r6, r8 # gpr to gpr operation
	cmprl r6, r4 # compare 
	addrl r6, g0, r6 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret

# how long does it take to walk through 0-360 degrees in custom increment amount using fprs
circleWalkRoundLongReal1:
	# g0,g1 - increment amount (real)
	movrl g0, fp0
	ldconst 360, r3 # end circle
	cvtir r3, fp1 # convert it to a real
	movrl 0.0, fp2 
0:
	roundrl fp2, fp3 
	cmprl fp2, fp1
	addrl fp2, fp0, fp2 # increment and use compare overlays to prevent stalling
	bl 0b # since floating point isn't exact we can just use less than 360
	ret
