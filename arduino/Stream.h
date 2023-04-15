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
    /**
     * @brief All invalid characters are ignored
     */
    SKIP_ALL,
    /**
     * @brief Nothing is skipped, and the stream is not touched unless the first waiting character is valid
     */
    SKIP_NONE,
    /**
     * @brief Only tabs, spaces, line feeds, and carriage returns are skipped
     */
    SKIP_WHITESPACE,
};
/**
 * @brief A character not found in a valid ascii numeric field
 */
#define NO_IGNORE_CHAR '\x01'

class Stream : public Print {

public:
    virtual int available() noexcept = 0;
    virtual int read() noexcept = 0;
    virtual int peek() noexcept = 0;
    Stream() : timeout_(1000), startMillis_(0) { }
    virtual ~Stream() { }
    // parsing methods
    /**
     * @brief Set the maximum number of milliseconds to wait
     * @param timeout The new number of milliseconds to wait for
     */
    inline void setTimeout(unsigned long timeout) noexcept { timeout_ = timeout; }
    constexpr unsigned long getTimeout() const noexcept { return timeout_; }
    bool find(char* target) noexcept;
    bool find(uint8_t* target) noexcept { return find ((char*)target); }
    bool find(char* target, size_t length) noexcept;
    bool find(uint8_t* target, size_t length) noexcept { return find((char*)target, length); }

    bool find (char target) { return find (&target, 1); }

    bool findUntil(char* target, char* terminator);
    bool findUntil(uint8_t * target, char* terminator) { return findUntil((char* )target, terminator); }

    bool findUntil(char* target, size_t targetLen, char* terminator, size_t termLen);
    bool findUntil(uint8_t * target, size_t targetLen, char* terminator, size_t termLen) { return findUntil((char* )target, targetLen, terminator, termLen); }

    /**
     * @brief Returns the first valid (long) integer value from the current position.
     * lookahead determines how parseInt lookahead in the stream.
     * @param lookahead What kind of lookahead to use
     * @param ignore the characters to be skipped in the stream
     * @return the first valid (long) integer
     */
    long parseInt(LookaheadMode lookahead = SKIP_ALL, char ignore = NO_IGNORE_CHAR) noexcept;

    /**
     * @brief Float version of parseInt
     * @param lookahead how to perform the character lookahead
     * @param ignore The character to ignore outright
     * @return The first valid float
     */
    float parseFloat(LookaheadMode lookahead = SKIP_ALL, char ignore = NO_IGNORE_CHAR) noexcept;

    /**
     * @brief Read chars from stream into buffer until length is hit or a timeout is reached
     * @param buffer  the buffer to read into
     * @param length the number of characters to read max
     * @return The actual number of characters read (possible to be different from length)
     */
    size_t readBytes(char* buffer, size_t length);
    size_t readBytes(uint8_t* buffer, size_t length) { return readBytes((char*)buffer, length); }

    /**
     * @brief read bytes into a stream until length has been hit, a timeout has been reached, or the provided terminator character is found
     * @param terminator The character to terminate reading if encountered
     * @param buffer The buffer to read into
     * @param length The number of characters to read max
     * @return The actual number of characters read into the buffer
     */
    size_t readBytesUntil(char terminator, char* buffer, size_t length);
    size_t readBytesUntil(char terminator, uint8_t* buffer, size_t length) { return readBytesUntil(terminator, (char*)buffer, length); }

    String readString();
    String readStringUntil(char terminator);
protected:
    // internal methods for keeping the public API simple
    long parseInt(char ignore) { return parseInt(SKIP_ALL, ignore); }
    float parseFloat(char ignore) { return parseFloat(SKIP_ALL, ignore); }

    /**
     * @brief Allows one to search for an arbitrary number of strings, this is a single search criteria
     */
    struct MultiTarget {
        /**
         * @brief The string being searched for
         */
        const char* str;
        /**
         * @brief Length of the string being searched for
         */
        size_t len;
        /**
         * @brief Index used by the search routine
         */
        size_t index;
    };

    /**
     * @brief Search for an arbitrary number of strings
     * @param targets The targets to search through
     * @param tCount the number of targets
     * @return The index of the target that is found first or -1 if timeout occurs
     */
    int findMulti(MultiTarget* targets, int tCount);

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
