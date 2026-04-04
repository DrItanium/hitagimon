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

# load/store tests
.include "boot/macros.s"
.data
.align 4
TestStorage:
.word 0
.word 0
.word 0
.word 0

.text
# these routines are useful for analyzing system bus memory latency
DefSingleInstructionOperation loadQuadTest0, ldq, TestStorage, r4
DefSingleInstructionOperation loadTripleTest0, ldt, TestStorage, r4
DefSingleInstructionOperation loadLongTest0, ldl, TestStorage, r4
DefSingleInstructionOperation loadTest0, ld, TestStorage, r4
DefSingleInstructionOperation loadShortTest0, ldos, TestStorage, r4
DefSingleInstructionOperation loadShortTest1, ldis, TestStorage, r4
DefSingleInstructionOperation loadByteTest0, ldob, TestStorage, r4
DefSingleInstructionOperation loadByteTest1, ldib, TestStorage, r4
