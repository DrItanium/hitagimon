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
.equ SRAM2BaseAddress, 0xFE010000
.equ SRAM1BaseAddress, 0xFE000800
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
DefSingleInstructionOperation loadByteTest0, ldob, TestStorage, r4

DefSingleInstructionOperation storeQuadTest0, stq, r4, TestStorage
DefSingleInstructionOperation storeTripleTest0, stt, r4, TestStorage
DefSingleInstructionOperation storeLongTest0, stl, r4, TestStorage
DefSingleInstructionOperation storeTest0, st, r4, TestStorage
DefSingleInstructionOperation storeShortTest0, stos, r4, TestStorage
DefSingleInstructionOperation storeByteTest0, stob, r4, TestStorage

DefSingleInstructionOperation loadQuadTest1, ldq, SRAM1BaseAddress, r4
DefSingleInstructionOperation loadTripleTest1, ldt, SRAM1BaseAddress, r4
DefSingleInstructionOperation loadLongTest1, ldl, SRAM1BaseAddress, r4
DefSingleInstructionOperation loadTest1, ld, SRAM1BaseAddress, r4
DefSingleInstructionOperation loadShortTest1, ldos, SRAM1BaseAddress, r4
DefSingleInstructionOperation loadByteTest1, ldob, SRAM1BaseAddress, r4

DefSingleInstructionOperation storeQuadTest1, stq, r4, SRAM1BaseAddress
DefSingleInstructionOperation storeTripleTest1, stt, r4, SRAM1BaseAddress
DefSingleInstructionOperation storeLongTest1, stl, r4, SRAM1BaseAddress
DefSingleInstructionOperation storeTest1, st, r4, SRAM1BaseAddress
DefSingleInstructionOperation storeShortTest1, stos, r4, SRAM1BaseAddress
DefSingleInstructionOperation storeByteTest1, stob, r4, SRAM1BaseAddress

DefSingleInstructionOperation loadQuadTest2, ldq, SRAM2BaseAddress, r4
DefSingleInstructionOperation loadTripleTest2, ldt, SRAM2BaseAddress, r4
DefSingleInstructionOperation loadLongTest2, ldl, SRAM2BaseAddress, r4
DefSingleInstructionOperation loadTest2, ld, SRAM2BaseAddress, r4
DefSingleInstructionOperation loadShortTest2, ldos, SRAM2BaseAddress, r4
DefSingleInstructionOperation loadByteTest2, ldob, SRAM2BaseAddress, r4

DefSingleInstructionOperation storeQuadTest2, stq, r4, SRAM2BaseAddress
DefSingleInstructionOperation storeTripleTest2, stt, r4, SRAM2BaseAddress
DefSingleInstructionOperation storeLongTest2, stl, r4, SRAM2BaseAddress
DefSingleInstructionOperation storeTest2, st, r4, SRAM2BaseAddress
DefSingleInstructionOperation storeShortTest2, stos, r4, SRAM2BaseAddress
DefSingleInstructionOperation storeByteTest2, stob, r4, SRAM2BaseAddress
