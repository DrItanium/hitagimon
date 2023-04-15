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

#include "WString.h"
#include "avr/dtostrf.h"

String::String(const char* cstr) : buffer_(NULL), capacity_(0), len_(0) {
    if (cstr) {
        copy(cstr, strlen(cstr));
    }
}

String::String(const String& value) : buffer_(NULL), capacity_(0), len_(0) {
    *this = value;
}

String::String(const __FlashStringHelper* pstr) : buffer_(NULL), capacity_(0), len_(0){
    *this = pstr;
}

String::String(char c) : buffer_(NULL), capacity_(0), len_(0){
    char buf[2];
    buf[0] = c;
    buf[1] = 0;
    *this = buf;
}
String::String(unsigned char value, unsigned char base) : buffer_(NULL), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned char)];
    utoa(value, buf, base);
    *this = buf;
}

String::String(int value, unsigned char base) : buffer_(NULL), capacity_(0), len_(0){
    char buf[2 + 8 * sizeof(int)];
    itoa(value, buf, base);
    *this = buf;
}

String::String(unsigned int value, unsigned char base) : buffer_(NULL), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned int)];
    utoa(value, buf, base);
    *this = buf;
}
String::String(long value, unsigned char base) : buffer_(NULL), capacity_(0), len_(0){
    char buf[2 + 8 * sizeof(long)];
    itoa(value, buf, base);
    *this = buf;
}

String::String(unsigned long value, unsigned char base) : buffer_(NULL), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned long)];
    utoa(value, buf, base);
    *this = buf;
}

String::String(float value, unsigned char places) : buffer_(NULL), capacity_(0), len_(0){
    char buf[33];
    *this = dtostrf(value, (places + 2) , places, buf);
}

String::String(double value, unsigned char places) : buffer_(NULL), capacity_(0), len_(0){
    char buf[33];
    *this = dtostrf(value, (places + 2) , places, buf);
}

String::~String()
{
    free(buffer_);
}


// memory management related routines
void
String::invalidate() {
    if (buffer_) {
        free(buffer_);
    }
    buffer_ = NULL;
    capacity_ = 0;
    len_ = 0;
}

unsigned char
String::reserve(unsigned int size) {
    if (buffer_ && capacity_ >= size) {
        return 1;
    }
    if (changeBuffer(size)) {
        if (len_ == 0) {
            buffer_[0] = 0;
        }
        return 1;
    }
    return 0;
}

unsigned char
String::changeBuffer(unsigned int maxLen) {
    char* newBuffer = (char*)realloc(buffer_, maxLen + 1);
    if (newBuffer) {
       buffer_ = newBuffer;
       capacity_ = maxLen;
       return 1;
    }
    return 0;
}

String&
String::copy(const char* cstr, unsigned int length) {
    if (!reserve(length)) {
        invalidate();
        return *this;
    }
    len_ = length;
    strcpy(buffer_, cstr);
    return *this;
}

String&
String::copy(const __FlashStringHelper* cstr, unsigned int length) {
    if (!reserve(length)) {
        invalidate();
        return *this;
    }
    len_ = length;
    strcpy_P(buffer_, (PGM_P)cstr);
    return *this;
}
// modification operations
void
String::replace(char find, char replace) {
    if (!buffer_) {
        return;
    }
    for (char* p = buffer_; *p; ++p) {
        if (*p == find) {
            *p = replace;
        }
    }
}
void
String::replace(const String &find, const String &replace) {
    if (len_ == 0 || find.len_ == 0) {
        return;
    }
    int diff = replace.len_ - find.len_;
    char* readFrom = buffer_;
    char* foundAt = NULL;
    if (diff == 0) {
        while ((foundAt = strstr(readFrom, find.buffer_)) != NULL)  {
            memcpy(foundAt, replace.buffer_, replace.len_);
            readFrom = foundAt + replace.len_;
        }
    } else if (diff < 0) {
        char* writeTo = buffer_;
        while ((foundAt = strstr(readFrom, find.buffer_)) != NULL)  {
            unsigned int n = foundAt - readFrom;
            memcpy(writeTo, readFrom, n);
            writeTo += n;
            memcpy(writeTo, replace.buffer_, replace.len_);
            writeTo += replace.len_;
            readFrom = foundAt + find.len_;
            len_ += diff;
        }
        strcpy(writeTo, readFrom);
    } else {
        unsigned int size = len_; // compute size needed for the result
        while ((foundAt = strstr(readFrom, find.buffer_)) != NULL)  {
            readFrom = foundAt + find.len_;
            size += diff;
        }
        if (size == len_) {
            return;
        }
        if (size > capacity_ && !changeBuffer((size))) {
            return;
        }
        int index = len_ - 1;
        while (index >= 0 && (index = lastIndexOf(find, index)) >= 0) {
            readFrom = buffer_ + index + find.len_;
            memmove(readFrom + diff, readFrom, len_ - (readFrom - buffer_));
            len_ += diff;
            buffer_[len_] = 0;
            memcpy(buffer_ + index, replace.buffer_, replace.len_);
            --index;
        }
    }
}
void
String::remove(unsigned int index) {
    // pass the biggest integer as the count.
    // This will force the other remove method to truncate the string
    remove(index, (unsigned int)-1);
}
void
String::remove(unsigned int index, unsigned int count) {
    if (index >= len_) {
        return;
    }
    if (count <= 0) {
        return;
    }
    if (count > len_ - index) {
        count = len_ - index;
    }
    char* writeTo = buffer_ + index;
    len_ -= count;
    strncpy(writeTo, buffer_ + index + count, len_ - index);
    buffer_[len_] = 0;
}
void
String::toLowerCase() {
    if (!buffer_)  {
        return;
    } else {
        for (char* p = buffer_; *p; ++p) {
            *p = tolower(*p);
        }
    }
}
void
String::toUpperCase() {
   if (!buffer_)  {
       return;
   } else {
       for (char* p = buffer_; *p; ++p) {
           *p = toupper(*p);
       }
   }
}
void
String::trim() {
    if (!buffer_ || len_ == 0) {
        return;
    } else {
        char* start = buffer_;
        while (isspace(*start)) {
            ++start;
        }
        char* end = buffer_ + len_ - 1;
        while (isspace(*end) && end >= start) {
            --end;
        }
        len_ = end + 1 - start;
        if (start > buffer_) {
            memcpy(buffer_, start, len_);
        }
        buffer_[len_] = 0;
    }

}
// conversion operations
long
String::toInt() const {
   if (buffer_) {
       return atol(buffer_);
   } else {
       return 0;
   }
}

float
String::toFloat() const {
    return static_cast<float>(toDouble());
}

double
String::toDouble() const {
    if (buffer_) {
        return atof(buffer_);
    } else {
        return 0;
    }
}