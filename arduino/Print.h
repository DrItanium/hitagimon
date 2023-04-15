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
// Created by jwscoggins on 4/13/23.
//

#ifndef HITAGIMON_PRINT_H
#define HITAGIMON_PRINT_H

#include <inttypes.h>
#include <stdio.h>
#include <stdarg.h>
#include <cortex/ModernCpp.h>
#include "WString.h"
#include "Printable.h"

#define DEC 10
#define HEX 16
#define OCT 8
#ifdef BIN
#undef BIN
#endif
#define BIN 2

class Print
{
public:
    Print() : writeError_(0) { }
    virtual ~Print()  { }
    inline int getWriteError() const { return writeError_; }
    inline void clearWriteError() { setWriteError(0); }

    virtual size_t write(uint8_t) = 0;
    virtual size_t write(const uint8_t* buffer, size_t size);
    inline size_t write(const char* str) {
        if (str == NULL) {
           return 0;
        } else {
            return write((const uint8_t*)str, strlen(str));
        }
    }
    inline size_t write(const char* buffer, size_t size) {
        return write((const uint8_t*) buffer, size);
    }
    virtual int availableForWrite() {
        return 0;
    }
    virtual void flush() { }
    size_t print(const __FlashStringHelper*);
    size_t print(const char[]);
    size_t print(char);
    size_t print(unsigned char, int = DEC);
    size_t print(int, int = DEC);
    size_t print(unsigned int, int = DEC);
    size_t print(long, int = DEC);
    size_t print(unsigned long, int = DEC);
    size_t print(double , int = 2);
    size_t print(const Printable&);

    size_t println(const __FlashStringHelper*);
    size_t println(const char[]);
    size_t println(char);
    size_t println(unsigned char, int = DEC);
    size_t println(int, int = DEC);
    size_t println(unsigned int, int = DEC);
    size_t println(long, int = DEC);
    size_t println(unsigned long, int = DEC);
    size_t println(double , int = 2);
    size_t println(const Printable&);
    size_t println();
private:
    size_t printNumber(unsigned long, uint8_t);
    size_t printFloat(double, uint8_t);


protected:
    void setWriteError(int err = 1) {
        writeError_ = err;
    }
private:
    int writeError_;

};

#endif //HITAGIMON_PRINT_H
