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
.text
.include "macros.s"
DefInterruptHandler _isr0, INT0
DefInterruptHandler _isr1, INT1
DefInterruptHandler _isr2, INT2
DefInterruptHandler _isr3, INT3
DefInterruptHandler _isr_dmachan0, DMACHANNEL0
DefInterruptHandler _isr_dmachan1, DMACHANNEL1
DefInterruptHandler _isr_dmachan2, DMACHANNEL2
DefInterruptHandler _isr_dmachan3, DMACHANNEL3
DefInterruptHandler _isr_dmachan4, DMACHANNEL4
DefInterruptHandler _isr_dmachan5, DMACHANNEL5
DefInterruptHandler _isr_dmachan6, DMACHANNEL6
DefInterruptHandler _isr_dmachan7, DMACHANNEL7
DefInterruptHandler _isr_dmachan8, DMACHANNEL8
DefInterruptHandler _isr_dmachan9, DMACHANNEL9
DefInterruptHandler _isr_dmachan10, DMACHANNEL10
DefInterruptHandler _isr_dmachan11, DMACHANNEL11
DefInterruptHandler _isr_dmachan12, DMACHANNEL12
DefInterruptHandler _isr_dmachan13, DMACHANNEL13
DefInterruptHandler _isr_dmachan14, DMACHANNEL14
DefInterruptHandler _isr_dmachan15, DMACHANNEL15
.global _do_nothing_isr
_do_nothing_isr:
        ret