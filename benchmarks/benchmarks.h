/*
hitagimon
Copyright (c) 2020-2026, Joshua Scoggins
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

#ifndef HITAGIMON_BENCHMARKS_H__
#define HITAGIMON_BENCHMARKS_H__
// Interface to assembly routines to test aspects of different i960 concepts
#include <cstdint>
#ifdef __cplusplus
extern "C" {
#endif

uint32_t ordinalRegisterToRegisterMoveTest(uint32_t count);
uint32_t longOrdinalRegisterToRegisterMoveTest(uint32_t count);
uint32_t tripleOrdinalRegisterToRegisterMoveTest(uint32_t count);
uint32_t quadOrdinalRegisterToRegisterMoveTest(uint32_t count);
uint32_t moveRealTest0(uint32_t count);
uint32_t moveRealTest1(uint32_t count);
uint32_t moveRealTest2(uint32_t count);
uint32_t moveRealTest3(uint32_t count);
uint32_t moveRealTest4(uint32_t count);
uint32_t moveRealTest5(uint32_t count);
uint32_t moveLongRealTest0(uint32_t count);
uint32_t moveLongRealTest1(uint32_t count);
uint32_t moveLongRealTest2(uint32_t count);
uint32_t moveLongRealTest3(uint32_t count);
uint32_t moveLongRealTest4(uint32_t count);
uint32_t moveLongRealTest5(uint32_t count);
uint32_t moveExtendedRealTest0(uint32_t count);
uint32_t moveExtendedRealTest1(uint32_t count);
uint32_t moveExtendedRealTest2(uint32_t count);
uint32_t moveExtendedRealTest3(uint32_t count);
uint32_t moveExtendedRealTest4(uint32_t count);
uint32_t moveExtendedRealTest5(uint32_t count);
uint32_t testCosine0(uint32_t count);
uint32_t testCosine1(uint32_t count);
uint32_t testCosine2(uint32_t count);
uint32_t testCosine3(uint32_t count);
uint32_t testCosine4(uint32_t count);
uint32_t testCosine5(uint32_t count);
uint32_t testLongCosine0(uint32_t count);
uint32_t testLongCosine1(uint32_t count);
uint32_t testLongCosine2(uint32_t count);
uint32_t testLongCosine3(uint32_t count);
uint32_t testLongCosine4(uint32_t count);
uint32_t testLongCosine5(uint32_t count);
uint32_t testSine0(uint32_t count);
uint32_t testSine1(uint32_t count);
uint32_t testSine2(uint32_t count);
uint32_t testSine3(uint32_t count);
uint32_t testSine4(uint32_t count);
uint32_t testSine5(uint32_t count);
uint32_t testLongSine0(uint32_t count);
uint32_t testLongSine1(uint32_t count);
uint32_t testLongSine2(uint32_t count);
uint32_t testLongSine3(uint32_t count);
uint32_t testLongSine4(uint32_t count);
uint32_t testLongSine5(uint32_t count);
uint32_t testTangent0(uint32_t count);
uint32_t testTangent1(uint32_t count);
uint32_t testTangent2(uint32_t count);
uint32_t testTangent3(uint32_t count);
uint32_t testTangent4(uint32_t count);
uint32_t testTangent5(uint32_t count);
uint32_t testLongTangent0(uint32_t count);
uint32_t testLongTangent1(uint32_t count);
uint32_t testLongTangent2(uint32_t count);
uint32_t testLongTangent3(uint32_t count);
uint32_t testLongTangent4(uint32_t count);
uint32_t testLongTangent5(uint32_t count);
uint32_t testSquareRoot0(uint32_t count);
uint32_t testSquareRoot1(uint32_t count);
uint32_t testSquareRoot2(uint32_t count);
uint32_t testSquareRoot3(uint32_t count);
uint32_t testSquareRoot4(uint32_t count);
uint32_t testSquareRoot5(uint32_t count);
uint32_t testLongSquareRoot0(uint32_t count);
uint32_t testLongSquareRoot1(uint32_t count);
uint32_t testLongSquareRoot2(uint32_t count);
uint32_t testLongSquareRoot3(uint32_t count);
uint32_t testLongSquareRoot4(uint32_t count);
uint32_t testLongSquareRoot5(uint32_t count);
uint32_t computeRawNullTime();
#ifdef __cplusplus
}
#endif
#endif //HITAGIMON_BENCHMARKS_H__
