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

