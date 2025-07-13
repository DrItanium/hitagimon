/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
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
/* fault table */
# We can specify the table to look into
# 0x2bf is the fault procedure table
# 0x27f could be used to access the system procedure table (but this will result in problems where tracing is concerned)
# 0 is used for local calls (which bypasses the trace infinite loop problem)
# This fault table is the important part, it is a dispatch table that describes where to go
.macro FaultEntry index, code=0x2, table=0x2bf
.word (\index << 2) | \code
.word \table
.endm
.macro ReservedFaultEntry
FaultEntry 0x10
.endm
    .globl  fault_table
    .align  8
fault_table:
    FaultEntry 0  # override
    FaultEntry 1  # trace
    FaultEntry 2  # Operation
    FaultEntry 3  # arithmetic
    FaultEntry 4  # floating point
    FaultEntry 5  # constraint
    FaultEntry 6  # virtual memory
    FaultEntry 7  # protection
    FaultEntry 8  # Machine
    FaultEntry 9  # structural
    FaultEntry 0xa # type
    ReservedFaultEntry
    FaultEntry 0xb # process
    FaultEntry 0xc # descriptor
    FaultEntry 0xd # event
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
    ReservedFaultEntry
