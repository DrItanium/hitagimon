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

#include "benchmarks.h"
#include "cortex/IODevice.h"
#include <iostream>


namespace GraphicsInterface = cortex::ChipsetBasicFunctions::Display;
namespace RandomInterface = cortex::ChipsetBasicFunctions::Random;

void 
mandlebrot(uint32_t iterations, uint32_t loops, uint32_t bits) noexcept {
    static const int16_t pixelWidth  = GraphicsInterface::width();  // TFT dimensions
    static const int16_t pixelHeight  = GraphicsInterface::height();  // TFT dimensions
    float centerReal  = -0.6, // Image center point in complex plane
          centerImag  =  0.0,
          rangeReal   =  3.0, // Image coverage in complex plane
          rangeImag   =  3.0; 
    int64_t       n, a, b, a2, b2, posReal;
    for (uint32_t q = 0; q < loops; ++q) {
        auto startReal   = (int64_t)((centerReal - rangeReal * 0.5)   * (float)(1 << bits)),
             startImag   = (int64_t)((centerImag + rangeImag * 0.5)   * (float)(1 << bits)),
             incReal     = (int64_t)((rangeReal / (float)pixelWidth)  * (float)(1 << bits)),
             incImag     = (int64_t)((rangeImag / (float)pixelHeight) * (float)(1 << bits));

        auto posImag = startImag;
        for (int y = 0; y < pixelHeight; ++y) {
            posReal = startReal;
            for (int x = 0; x < pixelWidth; ++x) {
                a = posReal;
                b = posImag;
                for (n = iterations; n > 0 ; --n) {
                    a2 = (a * a) >> bits;
                    b2 = (b * b) >> bits;
                    if ((a2 + b2) >= (4 << bits)) {
                        break;
                    }
                    b  = posImag + ((a * b) >> (bits - 1));
                    a  = posReal + a2 - b2;
                }
                GraphicsInterface::drawPixel(x, y, (n * 29)<<8 | (n * 67)); // takes 500ms with individual pixel writes
                posReal += incReal;
            }
            posImag -= incImag;
        }

        rangeReal *= 0.95;
        rangeImag *= 0.95;
    }
}

void 
mandlebrotBuffer(uint32_t iterations, uint32_t loops, uint32_t bits) noexcept {
    static const int16_t pixelWidth  = GraphicsInterface::width();  // TFT dimensions
    static const int16_t pixelHeight  = GraphicsInterface::height();  // TFT dimensions
    float centerReal  = -0.6, // Image center point in complex plane
          centerImag  =  0.0,
          rangeReal   =  3.0, // Image coverage in complex plane
          rangeImag   =  3.0; 
    int64_t       n, a, b, a2, b2, posReal;
    auto& buffer = cortex::DisplayMemory();
    for (uint32_t q = 0; q < loops; ++q) {
        int32_t startReal   = (int64_t)((centerReal - rangeReal * 0.5)   * (float)(1 << bits)),
                startImag   = (int64_t)((centerImag + rangeImag * 0.5)   * (float)(1 << bits)),
                incReal     = (int64_t)((rangeReal / (float)pixelWidth)  * (float)(1 << bits)),
                incImag     = (int64_t)((rangeImag / (float)pixelHeight) * (float)(1 << bits));

        uint32_t startTime = millis();
        int64_t posImag = startImag;
        for (int y = 0; y < pixelHeight; y++) {
            posReal = startReal;
            for (int x = 0; x < pixelWidth; x++) {
                a = posReal;
                b = posImag;
                for (n = iterations; n > 0 ; n--) {
                    a2 = (a * a) >> bits;
                    b2 = (b * b) >> bits;
                    if ((a2 + b2) >= (4 << bits)) {
                        break;
                    }
                    b  = posImag + ((a * b) >> (bits - 1));
                    a  = posReal + a2 - b2;
                }
                buffer.shorts[y * pixelWidth + x] = (n * 29) << 8 | (n * 67);
                posReal += incReal;
            }
            posImag -= incImag;
        }
        uint32_t elapsedTime = millis()-startTime;
        std::cout << "Took " << std::dec << elapsedTime << " ms" << std::endl;
        GraphicsInterface::updateDisplay(0, 0, pixelWidth, pixelHeight);
        rangeReal *= 0.95;
        rangeImag *= 0.95;
    }
}
