/*
hitagimon
Copyright (c) 2020-2022, Joshua Scoggins
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
.macro clear_g14
        ldconst 0, g14 # c compiler expects g14 to be 0
.endm
.macro save_globals
/* -- We allocate a spot for a "register holder" on the stack
 *    and store data to that spot. We will take advantage of
 *    the fact that this will be allocated at the first spot on the stack
 */
        ldconst 64, r4
        addo    sp, r4, sp
        stq     g0, -64(sp)
        stq     g4, -48(sp)
        stq     g8, -32(sp)
        stt     g12, -16(sp)
 .endm
 .macro restore_globals
        ldq     -64(sp), g0
        ldq     -48(sp), g4
        ldq     -32(sp), g8
        ldt     -16(sp), g12
 .endm

.macro c_call function
    clear_g14
    call \function
.endm

.macro c_callx function
    clear_g14
    callx \function
.endm

.macro def_system_call index,name
.text
.align 4
.global _\()\name
_\()\name:
lda \index, g13
calls g13
ret
.endm

.macro DeclareSegment a, b, c, d
.word \a
.word \b
.word \c
.word \d
.endm
.macro NullSegment
.space 16
.endm

.macro SegmentSelector base
.word ((\base)<<6) | 0x3f
.endm

.macro SimpleRegion address
DeclareSegment 0, 0, \address, 0x00fc00a3
.endm

.macro PagedRegion address, size
.space 8
.word \address
.word ((\size) << 18) | 0x5
.endm

.macro BipagedRegion address, size
.space 8
.word \address
.word ((\size) << 18) | 0x7
.endm

.macro PageEntry addr
.word ((\addr) | 0xc7)
.endm

.macro SmallSegmentTable addr
.space 8
.word \addr
.word (0x3f << 18) | 0xfb
.endm

.macro PortSegment addr
DeclareSegment 0, 0, \addr, 0x204000fb
.endm

.macro DefInterruptHandler name,toCall
.global \name
\name:
    save_globals
    c_call _vect_\toCall
    restore_globals
    ret
.endm

# branch if most significant bit clear
.macro bmsbc src, dest
	bbc 31, \src, \dest
.endm

# branch if most significant bit set
.macro bmsbs src, dest
	bbs 31, \src, \dest
.endm

# branch if least significant bit clear
.macro blsbc src, dest
	bbc 0, \src, \dest
.endm

# branch if least significant bit set
.macro blsbs src, dest
	bbs 0, \src, \dest
.endm

# compare integer and branch if equal zero
.macro cmpibez src, dest
	cmpibe 0, \src, \dest
.endm

# compare ordinal and branch if not equal zero 
.macro cmpobez src, dest
	cmpobe 0, \src, \dest
.endm

.macro nandl src1l, src1h, src2l, src2h, destl, desth
	nand \src1l, \src2l, \destl
	nand \src1h, \src2h, \desth
.endm

.macro norl src1l, src1h, src2l, src2h, destl, desth
	nor \src1l, \src2l, \destl
	nor \src1h, \src2h, \desth
.endm

.macro andl src1l, src1h, src2l, src2h, destl, desth
	and \src1l, \src2l, \destl
	and \src1h, \src2h, \desth
.endm
.macro orl src1l, src1h, src2l, src2h, destl, desth
	or \src1l, \src2l, \destl
	or \src1h, \src2h, \desth
.endm
.macro xorl src1l, src1h, src2l, src2h, destl, desth
	xor \src1l, \src2l, \destl
	xor \src1h, \src2h, \desth
.endm
.macro xnorl src1l, src1h, src2l, src2h, destl, desth
	xnor \src1l, \src2l, \destl
	xnor \src1h, \src2h, \desth
.endm

.macro notl srcl, srch, destl, desth
	not \srcl, \destl
	not \srch, \desth
.endm

.macro set_carry_flag
    cmpo 0, 0 # do not clear the carry flag but set it instead
.endm

.macro clear_carry_flag
	cmpo 1, 0 # clear the carry flag
.endm

.macro addlo src1l, src1h, src2l, src2h, destl, desth
	clear_carry_flag
	addc \src1l, \src2l, \destl
	addc \src1h, \src2h, \desth
.endm

.macro addli src1l, src1h, src2l, src2h, destl, desth
	addlo \src1l, \src1h, \src2l, \src2h, \destl, \desth
.endm

#    cmpo 0, 0 # do not clear the carry flag but set it instead
#    subc g2, g0, g0 # start at the lower half and move up
#    subc g3, g1, g1 # Now to the upper part


.macro sublo src1l, src1h, src2l, src2h, destl, desth
	set_carry_flag
	subc \src2l, \src1l, \destl
	subc \src2h, \src1h, \desth
.endm

.macro subli src1l, src1h, src2l, src2h, destl, desth
	sublo \src1l, \src1h, \src2l, \src2h, \destl, \desth
.endm

.macro cmpytestx cmpKind, testKind, src1, src2, dest
	cmp\()\cmpKind \src1, \src2
	test\()\testKind \dest
.endm

.macro cmpotestx testKind, src1, src2, dest
	cmpytestx o, \testKind, \src1, \src2, \dest
.endm

.macro cmpitestx testKind, src1, src2, dest
	cmpytestx i, \testKind, \src1, \src2, \dest
.endm

.macro cmpotestl src1, src2, dest
	cmpotestx l, \src1, \src2, \dest
.endm

.macro cmpitestl src1, src2, dest
	cmpitestx l, \src1, \src2, \dest
.endm
.macro cmpoteste src1, src2, dest
	cmpotestx e, \src1, \src2, \dest
.endm

.macro cmpiteste src1, src2, dest
	cmpitestx e, \src1, \src2, \dest
.endm
.macro cmpotestne src1, src2, dest
	cmpotestx ne, \src1, \src2, \dest
.endm

.macro cmpitestne src1, src2, dest
	cmpitestx ne, \src1, \src2, \dest
.endm
# @todo implement more cmpxtesty operations
# @todo implement more compare with zero and branch instructions
