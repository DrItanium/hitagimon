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
// Created by jwscoggins on 4/14/23.
//

#ifndef HITAGIMON_STREAM_H
#define HITAGIMON_STREAM_H

#include <inttypes.h>
#include <cortex/ModernCpp.h>
#include "Print.h"


enum LookaheadMode {
    SKIP_ALL, // All invalid characters are ignored
    SKIP_NONE, // Nothing is skipped, and the stream is not touched unless the first waiting character is valid
    SKIP_WHITESPACE,  // Only tabs, spaces, line feeds & carriage returns are skipped
};
#define NO_IGNORE_CHAR '\x01' // a char not found in a valid ascii numeric field

class Stream : public Print {

public:
    virtual int available() noexcept = 0;
    virtual int read() noexcept = 0;
    virtual int peek() noexcept = 0;
    Stream() : timeout_(1000), startMillis_(0) { }
    virtual ~Stream() { }
    void setTimeout(unsigned long timeout) noexcept;
    constexpr unsigned long getTimeout() const noexcept { return timeout_; }
    bool find(char* target) noexcept;
    bool find(uint8_t* target) noexcept { return find ((char*)target); }
    bool find(char* target, size_t length) noexcept;
    bool find(uint8_t* target, size_t length) noexcept { return find((char*)target, length); }
protected:
    int timedRead() noexcept;
    int timedPeek() noexcept;
    /**
     * @brief find the next digit but do not modify the stream
     * @param lookahead The lookahead style
     * @param detectDecimal should we detect decimal values
     * @return the next numeric digit in the stream or -1 if timeout
     */
    int peekNextDigit(LookaheadMode lookahead, bool detectDecimal) noexcept;
protected:
    unsigned long timeout_; // number of milliseconds to wait for the next char before aborting timed read
    unsigned long startMillis_; // used for timeout measurement
};
#undef NO_IGNORE_CHAR
#endif //HITAGIMON_STREAM_H
