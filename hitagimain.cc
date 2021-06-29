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
//
// Created by jwscoggins on 6/7/21.
//
#include <stdint.h>
#include "IODevice.h"
#include <string>
#include "EnvironmentInterface.h"
#include "LowLevelInterface.h"

uint64_t delay(uint64_t count) {
    uint64_t value = 0;
    for (uint64_t i = 0; i < count; ++i) {
        ++value;
    }
    return value;
}

int main() {
    ChipsetBasicFunctions& theChipset = getBasicChipsetInterface();
    BuiltinTFTDisplay& theDisplay = getDisplay();
    theChipset.writeLine("HITAGIMON");
    printf("Built on %s at %s\n", __DATE__, __TIME__);
    printf("--------------------------------------------\n\n\n\n");
    printf("Sizeof(int) = %d\n", sizeof(int));
    printf("Setting up Environment Variables....{\n");
    printf("}...Done\n");
    printf("STARTING UP DOOM!!!\n");
    theDisplay.clearScreen();
    while (true) {
        dummy_call();
    }
    return 0;
}

