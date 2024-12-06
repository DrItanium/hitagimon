/*
hitagimon
Copyright (c) 2020-2024, Joshua Scoggins
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
.global u64_add_via_addc
.global s64_add_via_addc
.global u64_subtract_via_subc
.global s64_subtract_via_subc
.global u64_subtract_via_subc_v2
.global s64_subtract_via_subc_v2
.global s64_subtract_via_subc_v3
.global u64_subtract_via_subc_v3
.global u32_rotate
.global u32_emul
.global xnor64
.global nand64
.global nor64
.global scanbyte32
.global spanbit32
.global scanbit32
.global extract32

s64_add_via_addc:
u64_add_via_addc:
    # g0,g1 - a
    # g2,g3 - b
    cmpo 1, 0
    addc g0, g2, g0
    addc g1, g3, g1
    ret

s64_subtract_via_subc:
u64_subtract_via_subc:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g3, g1, g1
    subc g2, g0, g0
    ret

s64_subtract_via_subc_v2:
u64_subtract_via_subc_v2:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g2, g0, g0
    subc g3, g1, g1
    ret

s64_subtract_via_subc_v3:
u64_subtract_via_subc_v3:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g3, g1, g1
    subc g2, g0, g0
    ret

.global s64_subtract_via_subc_v4
.global u64_subtract_via_subc_v4
s64_subtract_via_subc_v4:
u64_subtract_via_subc_v4:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # a - b ->
    cmpo 1, 0
    subc g1, g3, g1
    subc g0, g2, g0
    ret

.global s64_subtract_via_subc_v5
.global u64_subtract_via_subc_v5
s64_subtract_via_subc_v5:
u64_subtract_via_subc_v5:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # a - b ->
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g1, g3, g1
    subc g0, g2, g0
    ret

.global s64_subtract_via_subc_v6
.global u64_subtract_via_subc_v6
s64_subtract_via_subc_v6:
u64_subtract_via_subc_v6:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g0, g2, g0
    subc g1, g3, g1
    ret

.global s64_subtract_via_subc_v7
.global u64_subtract_via_subc_v7
s64_subtract_via_subc_v7:
u64_subtract_via_subc_v7:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # this is the most reliable method to be honest!
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g2, g0, g0 # start at the lower half and move up
    subc g3, g1, g1 # Now to the upper part
    ret

u32_rotate:
    # g0 -> src
    # g1 -> len
    rotate g1, g0, g0
    ret

u32_emul:
    # g0 -> src1
    # g1 -> src2
    emul g0, g1, g0
    ret

xnor64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    xnor g0, g2, g0
    xnor g1, g3, g1
    ret
nand64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    nand g0, g2, g0
    nand g1, g3, g1
    ret

nor64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    nor g0, g2, g0
    nor g1, g3, g1
    ret

scanbyte32:
    # g0 -> src1
    # g1 -> src2
    scanbyte g0, g1
    teste g0 # convert to boolean/int
    ret

spanbit32:
    # g0 -> src
    spanbit g0, g0
    ret
scanbit32:
    scanbit g0, g0
    ret
extract32:
    # g0 -> src/dest
    # g1 -> bitpos
    # g2 -> len
    extract g1, g2, g0
    ret

.global modify32
modify32:
    # g0 -> value (src/dest)
    # g1 -> src
    # g2 -> mask
    modify g2, g1, g0
    ret