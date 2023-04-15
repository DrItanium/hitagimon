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

// public methods

bool
Stream::find(char* target) noexcept {
    return findUntil(target, strlen(target), nullptr, 0);
}

bool
Stream::find(char* target, size_t length) noexcept {
    return findUntil(target, length, nullptr, 0);
}

bool
Stream::findUntil(char* target, char* terminator) noexcept {
    return findUntil(target, strlen(target), terminator, strlen(terminator));
}

bool
Stream::findUntil(char* target, size_t targetLen, char* terminator, size_t termLen) noexcept {
    if (!terminator) {
        MultiTarget t[1] = { { target, targetLen, 0 } };
        // my guess is to make sure that we do the right thing with boolean convertibility
        return findMulti(t, 1) == 0 ? true : false;
    } else {
        MultiTarget t[2] = { { target, targetLen, 0 }, { terminator, termLen, 0} };
        // my guess is to make sure that we do the right thing with boolean convertibility
        return findMulti(t, 2) == 0 ? true : false;
    }
}

long
Stream::parseInt(LookaheadMode lookahead, char ignore) noexcept {
    bool isNegative = false;
    long value = 0;
    int c = peekNextDigit(lookahead, false);
    // ignore non numeric leading characters
    if (c < 0) {
        return 0; // zero returned if timeout
    }
    do {
       if (c == ignore) {
           // do nothing
       } else if (c == '-') {
          isNegative = true;
       } else if (c >= '0' && c <= '9') {
           // c is a digit
           value = value * 10 + c - '0';
       }
       read();
       c = timedPeek();
    } while ((c >= '0' && c <= '9') || c == ignore);
    if (isNegative) {
        value = -value;
    }
    return value;
}

float
Stream::parseFloat(LookaheadMode lookahead, char ignore) noexcept {
   bool isNegative = false;
   bool isFraction = false;
   long value = 0;
   float fraction = 1.0;
   int c = peekNextDigit(lookahead, true);
   if (c < 0) {
       return 0; // zero returned if timeout
   }
    do {
        if (c == ignore) {
            // do nothing
        } else if (c == '-') {
            isNegative = true;
        } else if (c == '.') {
            isFraction = true;
        } else if (c >= '0' && c <= '9') {
            // c is a digit
            value = value * 10 + c - '0';
            if (isFraction) {
                fraction *= 0.1;
            }
        }
        read(); // consume the character we got with peek
        c = timedPeek();
    } while ((c >= '0' && c <= '9') || (c == '.' && !isFraction) || c == ignore);
    if (isNegative) {
        value = -value;
    }
    if (isFraction) {
        return value * fraction;
    } else {
        return value;
    }
}

size_t
Stream::readBytes(char* buffer, size_t length) {
    size_t count = 0;
    while (count < length)  {
        int c = timedRead();
        if (c < 0) {
            break;
        }
        *buffer++ = (char)c;
        ++count;
    }
    return count;
}
