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

void ordinalRegisterToRegisterMoveTest(uint32_t count);
void longOrdinalRegisterToRegisterMoveTest(uint32_t count);
void tripleOrdinalRegisterToRegisterMoveTest(uint32_t count);
void quadOrdinalRegisterToRegisterMoveTest(uint32_t count);
void moveRealTest0(uint32_t count);
void moveRealTest1(uint32_t count);
void moveRealTest2(uint32_t count);
void moveRealTest3(uint32_t count);
void moveRealTest4(uint32_t count);
void moveRealTest5(uint32_t count);
void moveLongRealTest0(uint32_t count);
void moveLongRealTest1(uint32_t count);
void moveLongRealTest2(uint32_t count);
void moveLongRealTest3(uint32_t count);
void moveLongRealTest4(uint32_t count);
void moveLongRealTest5(uint32_t count);
void moveExtendedRealTest0(uint32_t count);
void moveExtendedRealTest1(uint32_t count);
void moveExtendedRealTest2(uint32_t count);
void moveExtendedRealTest3(uint32_t count);
void moveExtendedRealTest4(uint32_t count);
void moveExtendedRealTest5(uint32_t count);
void testCosine0(uint32_t count);
void testCosine1(uint32_t count);
void testCosine2(uint32_t count);
void testCosine3(uint32_t count);
void testCosine4(uint32_t count);
void testCosine5(uint32_t count);
void testLongCosine0(uint32_t count);
void testLongCosine1(uint32_t count);
void testLongCosine2(uint32_t count);
void testLongCosine3(uint32_t count);
void testLongCosine4(uint32_t count);
void testLongCosine5(uint32_t count);
void testSine0(uint32_t count);
void testSine1(uint32_t count);
void testSine2(uint32_t count);
void testSine3(uint32_t count);
void testSine4(uint32_t count);
void testSine5(uint32_t count);
void testLongSine0(uint32_t count);
void testLongSine1(uint32_t count);
void testLongSine2(uint32_t count);
void testLongSine3(uint32_t count);
void testLongSine4(uint32_t count);
void testLongSine5(uint32_t count);
void testTangent0(uint32_t count);
void testTangent1(uint32_t count);
void testTangent2(uint32_t count);
void testTangent3(uint32_t count);
void testTangent4(uint32_t count);
void testTangent5(uint32_t count);
void testLongTangent0(uint32_t count);
void testLongTangent1(uint32_t count);
void testLongTangent2(uint32_t count);
void testLongTangent3(uint32_t count);
void testLongTangent4(uint32_t count);
void testLongTangent5(uint32_t count);
void testSquareRoot0(uint32_t count);
void testSquareRoot1(uint32_t count);
void testSquareRoot2(uint32_t count);
void testSquareRoot3(uint32_t count);
void testSquareRoot4(uint32_t count);
void testSquareRoot5(uint32_t count);
void testLongSquareRoot0(uint32_t count);
void testLongSquareRoot1(uint32_t count);
void testLongSquareRoot2(uint32_t count);
void testLongSquareRoot3(uint32_t count);
void testLongSquareRoot4(uint32_t count);
void testLongSquareRoot5(uint32_t count);
void testExponent0(uint32_t count);
void testExponent1(uint32_t count);
void testExponent2(uint32_t count);
void testExponent3(uint32_t count);
void testExponent4(uint32_t count);
void testExponent5(uint32_t count);
void testLongExponent0(uint32_t count);
void testLongExponent1(uint32_t count);
void testLongExponent2(uint32_t count);
void testLongExponent3(uint32_t count);
void testLongExponent4(uint32_t count);
void testLongExponent5(uint32_t count);
uint32_t computeRawNullTime();
void circleWalkCosineReal0(float increment);
void circleWalkCosineReal1(float increment);
void circleWalkCosineLongReal0(double increment);
void circleWalkCosineLongReal1(double increment);
void circleWalkSineReal0(float increment);
void circleWalkSineReal1(float increment);
void circleWalkSineLongReal0(double increment);
void circleWalkSineLongReal1(double increment);
void circleWalkTangentReal0(float increment);
void circleWalkTangentReal1(float increment);
void circleWalkTangentLongReal0(double increment);
void circleWalkTangentLongReal1(double increment);
void circleWalkSquareRootReal0(float increment);
void circleWalkSquareRootReal1(float increment);
void circleWalkSquareRootLongReal0(double increment);
void circleWalkSquareRootLongReal1(double increment);
void circleWalkRoundReal0(float increment);
void circleWalkRoundReal1(float increment);
void circleWalkRoundLongReal0(double increment);
void circleWalkRoundLongReal1(double increment);
// load/store from PSRAM
void loadQuadTest0(uint32_t count); // ldl
void loadTripleTest0(uint32_t count); // ldt
void loadLongTest0(uint32_t count); // ldl
void loadTest0(uint32_t count); // ld
void loadShortTest0(uint32_t count); // ldos
void loadByteTest0(uint32_t count); // ldob
void storeQuadTest0(uint32_t count); // stl
void storeTripleTest0(uint32_t count); // stt
void storeLongTest0(uint32_t count); // stl
void storeTest0(uint32_t count); // st
void storeShortTest0(uint32_t count); // stos
void storeByteTest0(uint32_t count); // stob

// load/store from SRAM1 (fastmem)
void loadQuadTest1(uint32_t count); // ldl
void loadTripleTest1(uint32_t count); // ldt
void loadLongTest1(uint32_t count); // ldl
void loadTest1(uint32_t count); // ld
void loadShortTest1(uint32_t count); // ldos
void loadByteTest1(uint32_t count); // ldob
void storeQuadTest1(uint32_t count); // stl
void storeTripleTest1(uint32_t count); // stt
void storeLongTest1(uint32_t count); // stl
void storeTest1(uint32_t count); // st
void storeShortTest1(uint32_t count); // stos
void storeByteTest1(uint32_t count); // stob

// load/store from SRAM2 (DMAEXT)
void loadQuadTest2(uint32_t count); // ldl
void loadTripleTest2(uint32_t count); // ldt
void loadLongTest2(uint32_t count); // ldl
void loadTest2(uint32_t count); // ld
void loadShortTest2(uint32_t count); // ldos
void loadByteTest2(uint32_t count); // ldob
void storeQuadTest2(uint32_t count); // stl
void storeTripleTest2(uint32_t count); // stt
void storeLongTest2(uint32_t count); // stl
void storeTest2(uint32_t count); // st
void storeShortTest2(uint32_t count); // stos
void storeByteTest2(uint32_t count); // stob
                                     
// load/store from unmapped 
void loadQuadTest3(uint32_t count); // ldl
void loadTripleTest3(uint32_t count); // ldt
void loadLongTest3(uint32_t count); // ldl
void loadTest3(uint32_t count); // ld
void loadShortTest3(uint32_t count); // ldos
void loadByteTest3(uint32_t count); // ldob
void storeQuadTest3(uint32_t count); // stl
void storeTripleTest3(uint32_t count); // stt
void storeLongTest3(uint32_t count); // stl
void storeTest3(uint32_t count); // st
void storeShortTest3(uint32_t count); // stos
void storeByteTest3(uint32_t count); // stob
// skip the integer versions...
#ifdef __cplusplus
}
#endif
#endif //HITAGIMON_BENCHMARKS_H__
