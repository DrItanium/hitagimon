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
.global _u64_add_via_addc
.global _s64_add_via_addc
.global _u64_subtract_via_subc
.global _s64_subtract_via_subc
.global _u64_subtract_via_subc_v2
.global _s64_subtract_via_subc_v2
.global _u32_rotate
.global _u32_emul
.global _xnor64
.global _nand64
.global _nor64
.global _scanbyte32
.global _spanbit32
.global _scanbit32
.global _extract32

_s64_add_via_addc:
_u64_add_via_addc:
    # g0,g1 - a
    # g2,g3 - b
    cmpo 1, 0
    addc g0, g2, g0
    addc g1, g3, g1
    ret

_s64_subtract_via_subc:
_u64_subtract_via_subc:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g3, g1, g1
    subc g2, g0, g0
    ret

_s64_subtract_via_subc_v2:
_u64_subtract_via_subc_v2:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g2, g0, g0
    subc g3, g1, g1
    ret

.global _s64_subtract_via_subc_v3
.global _u64_subtract_via_subc_v3
_s64_subtract_via_subc_v3:
_u64_subtract_via_subc_v3:
    # g0,g1 - a (src2)
    # g2,g3 - b (src1)
    # a -b ->
    cmpo 1, 0
    subc g3, g1, g1
    subc g2, g0, g0
    ret

.global _s64_subtract_via_subc_v4
.global _u64_subtract_via_subc_v4
_s64_subtract_via_subc_v4:
_u64_subtract_via_subc_v4:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # a - b ->
    cmpo 1, 0
    subc g1, g3, g1
    subc g0, g2, g0
    ret

.global _s64_subtract_via_subc_v5
.global _u64_subtract_via_subc_v5
_s64_subtract_via_subc_v5:
_u64_subtract_via_subc_v5:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # a - b ->
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g1, g3, g1
    subc g0, g2, g0
    ret

.global _s64_subtract_via_subc_v6
.global _u64_subtract_via_subc_v6
_s64_subtract_via_subc_v6:
_u64_subtract_via_subc_v6:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g0, g2, g0
    subc g1, g3, g1
    ret

.global _s64_subtract_via_subc_v7
.global _u64_subtract_via_subc_v7
_s64_subtract_via_subc_v7:
_u64_subtract_via_subc_v7:
    # g0,g1 - a (src1)
    # g2,g3 - b (src2)
    # this is the most reliable method to be honest!
    cmpo 0, 0 # do not clear the carry flag but set it instead
    subc g2, g0, g0 # start at the lower half and move up
    subc g3, g1, g1 # Now to the upper part
    ret

_u32_rotate:
    # g0 -> src
    # g1 -> len
    rotate g1, g0, g0
    ret

_u32_emul:
    # g0 -> src1
    # g1 -> src2
    emul g0, g1, g0
    ret

_xnor64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    xnor g0, g2, g0
    xnor g1, g3, g1
    ret
_nand64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    nand g0, g2, g0
    nand g1, g3, g1
    ret

_nor64:
    # g0, g1 -> src1
    # g2, g3 -> src2
    nor g0, g2, g0
    nor g1, g3, g1
    ret

_scanbyte32:
    # g0 -> src1
    # g1 -> src2
    scanbyte g0, g1
    teste g0 # convert to boolean/int
    ret

_spanbit32:
    # g0 -> src
    spanbit g0, g0
    ret
_scanbit32:
    scanbit g0, g0
    ret
_extract32:
    # g0 -> src/dest
    # g1 -> bitpos
    # g2 -> len
    extract g1, g2, g0
    ret

.global _modify32
_modify32:
    # g0 -> value (src/dest)
    # g1 -> src
    # g2 -> mask
    modify g2, g1, g0
    ret



.global _cos
_cos:
    cosrl g0, g0
    ret

.global _sin
_sin:
    sinrl g0, g0
    ret

.global _tan
_tan:
    tanrl g0, g0
    ret

.global _sqrt
_sqrt:
    sqrtrl g0, g0
    ret

.global _cosf
_cosf:
    cosr g0, g0
    ret

.global _sinf
_sinf:
    sinr g0, g0
    ret

.global _tanf
_tanf:
    tanr g0, g0
    ret

.global _sqrtf
_sqrtf:
    sqrtr g0, g0
    ret

.global _atan2
_atan2:
    # g0 -> y
    # g1 -> x
    atanrl g2, g0, g0
    ret
.global _atan2f
_atan2f:
    # g0 -> y
    # g1 -> x
    atanr g2, g0, g0
    ret