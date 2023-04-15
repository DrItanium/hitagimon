/*
hitagimon
Copyright (c) 2023, Joshua Scoggins
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
// Created by jwscoggins on 4/15/23.
//

#include "Arduino.h"
#include "Stream.h"

#define PARSE_TIMEOUT 1000 // default number of milli-seconds to wait

int
Stream::timedRead() noexcept {
    startMillis_ = millis();
    do {
        int c = read();
        if (c >= 0) {
            return c;
        }
    } while (millis() - startMillis_ < timeout_);
    return -1; // indicates timeout
}

int
Stream::timedPeek() noexcept {
    startMillis_ = millis();
    do {
        int c = peek();
        if (c >= 0) {
            return c;
        }
    } while (millis() - startMillis_ < timeout_);
    return -1; // timeout happened
}

int
Stream::peekNextDigit(LookaheadMode lookahead, bool detectDecimal) {
    while (true) {
        int c = timedPeek();
        if (c < 0 ||
            c == '-' ||
            (c >= '0' && c <= '9') ||
            (detectDecimal && c == '.')) {
            return c;
        }
        switch (lookahead) {
            case SKIP_NONE:
                return -1; // fail code
            case SKIP_WHITESPACE:
                switch (c) {
                    case ' ':
                    case '\t':
                    case '\r':
                    case '\n':
                        break;
                    default:
                        return -1; // fail code
                }
                break;
            case SKIP_ALL:
                break;
        }
        read(); // discard non-numeric?
    }
}