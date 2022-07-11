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

/* taken from the mon960 source code */
_sendIACCommand:
/* G0 is the address to send the iac to (if it is 0, use 0xff000010).  */
/* G1 is the address of the IAC (four words).  Copy the IAC to ensure  */
/* that it is aligned. */
	mov	sp, g2
	addo	0x10, sp, sp
	ldq	(g1), r4

	lda	0xff000010, r3
	cmpobne	0, g0, 1f		# If the specified address is 0,
	mov	r3, g0			# use 0xff000010.

1:	cmpobne	g0, r3, 2f		# If the destination is 0xff000010,

	ldconst	0x93000000, r3		# and it is a reinitialize processor
	cmpobne	r4, r3, 2f		# IAC,

	cmpobne	0, r7, 2f		# and the destination address is 0,
	lda	3f, r7			# set the destination address to 3f.
	flushreg

2:	stq	r4, (g2)
	synmovq g0, g2
3:	ret

#extern "C" uint32_t hitagi_readInterruptState();
#extern "C" void hitagi_writeInterruptState(uint32_t value);
_hitagi_readInterruptState:
    lda 0xFF000004, r5
    synld r5, g0
    ret

_hitagi_writeInterruptState:
    lda 0xFF000004, r5
    synmov r5, g0
    ret
