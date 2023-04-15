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

String::String(const char* cstr) : buffer_(nullptr), capacity_(0), len_(0) {
    if (cstr) {
        copy(cstr, strlen(cstr));
    }
}

String::String(const String& value) : buffer_(nullptr), capacity_(0), len_(0) {
    *this = value;
}

String::String(const __FlashStringHelper* pstr) : buffer_(nullptr), capacity_(0), len_(0){
    *this = pstr;
}

String::String(char c) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[2];
    buf[0] = c;
    buf[1] = 0;
    *this = buf;
}
String::String(unsigned char value, unsigned char base) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned char)];
    utoa(value, buf, base);
    *this = buf;
}

String::String(int value, unsigned char base) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[2 + 8 * sizeof(int)];
    itoa(value, buf, base);
    *this = buf;
}

String::String(unsigned int value, unsigned char base) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned int)];
    utoa(value, buf, base);
    *this = buf;
}
String::String(long value, unsigned char base) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[2 + 8 * sizeof(long)];
    itoa(value, buf, base);
    *this = buf;
}

String::String(unsigned long value, unsigned char base) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[1 + 8 * sizeof(unsigned long)];
    utoa(value, buf, base);
    *this = buf;
}

String::String(float value, unsigned char places) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[33];
    *this = dtostrf(value, (places + 2) , places, buf);
}

String::String(double value, unsigned char places) : buffer_(nullptr), capacity_(0), len_(0){
    char buf[33];
    *this = dtostrf(value, (places + 2) , places, buf);
}

String::~String()
{
    free(buffer_);
}


// memory management related routines
void
String::invalidate() noexcept {
    if (buffer_) {
        free(buffer_);
    }
    buffer_ = nullptr;
    capacity_ = 0;
    len_ = 0;
}

unsigned char
String::reserve(unsigned int size) noexcept {
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
String::changeBuffer(unsigned int maxLen) noexcept {
    char* newBuffer = (char*)realloc(buffer_, maxLen + 1);
    if (newBuffer) {
       buffer_ = newBuffer;
       capacity_ = maxLen;
       return 1;
    }
    return 0;
}

String&
String::copy(const char* cstr, unsigned int length) noexcept {
    if (!reserve(length)) {
        invalidate();
        return *this;
    }
    len_ = length;
    strcpy(buffer_, cstr);
    return *this;
}

String&
String::copy(const __FlashStringHelper* cstr, unsigned int length) noexcept {
    if (!reserve(length)) {
        invalidate();
        return *this;
    }
    len_ = length;
    strcpy_P(buffer_, (PGM_P)cstr);
    return *this;
}
// character access
char
String::charAt(unsigned int loc) const noexcept {
    return operator[](loc);
}

void
String::setCharAt(unsigned int loc, char c) noexcept {
    if (loc < len_) {
        buffer_[loc] = c;
    }
}
char&
String::operator[](unsigned int index) noexcept {
    // provide a writable cell in the case where we are out of range of the actual string
    // this includes the case where the buffer is non existent
    static char dummyWritableChar;
    // perform a range check
    if (index >= len_ || !buffer_) {
        dummyWritableChar = 0;
        return dummyWritableChar;
    }
    return buffer_[index];
}
char
String::operator[](unsigned int index) const noexcept {
    if (index >= len_ || !buffer_) {
        return 0;
    }
    return buffer_[index];
}

void
String::getBytes(unsigned char* buf, unsigned int bufSize, unsigned int index) const noexcept {
    if (!bufSize || !buf) {
        return;
    }
    if (index >= len_) {
        buf[0] = 0;
        return;
    }
    unsigned int n = bufSize - 1;
    if (n > len_ - index) {
        n = len_ - index;
    }
    strncpy((char*)buf, buffer_ + index, n);
    buf[n] = 0;
}
// search
int
String::indexOf(char c) const noexcept {
    return indexOf(c, 0);
}
int
String::indexOf(char ch, unsigned int from) const noexcept {
    if (from >= len_) {
        return -1;
    }
    const char* temp = strchr(buffer_ + from, ch);
    if (!temp) {
        return -1;
    } else {
        return temp - buffer_;
    }
}
int
String::indexOf(const String& s2) const noexcept {
    return indexOf(s2, 0);
}
int
String::indexOf(const String& s2, unsigned int from) const noexcept {
    if (from >= len_) {
        return -1;
    }
    const char* found = strstr(buffer_ + from, s2.buffer_);
    if (!found) {
        return -1;
    } else {
        return found - buffer_;
    }
}
int
String::lastIndexOf(char ch) const noexcept {
    return lastIndexOf(ch, len_ - 1);
}
int
String::lastIndexOf(char ch, unsigned int fromIndex) const noexcept {
    if (fromIndex >= len_) {
        return -1;
    }
    char tempChar = buffer_[fromIndex + 1];
    buffer_[fromIndex +1] = '\0';
    char* temp = strrchr(buffer_, ch);
    buffer_[fromIndex + 1] = tempChar;
    if (!temp) {
        return -1;
    } else {
        return temp - buffer_;
    }

}
int
String::lastIndexOf(const String &str) const noexcept {
    return lastIndexOf(str, len_- str.len_);
}
int
String::lastIndexOf(const String& s2, unsigned int fromIndex) const noexcept {
    if (s2.len_ == 0 || len_ == 0 || s2.len_ > len_) {
        return -1;
    }
    if (fromIndex >= len_) {
        fromIndex = len_ - 1;
    }
    int found = -1;
    for (char* p = buffer_; p <= buffer_; ++p) {
        p = strstr(p, s2.buffer_);
        if (!p) {
            break;
        }
        if ((unsigned int)(p - buffer_) <= fromIndex) {
            found = p - buffer_;
        }
    }
    return found;
}
String
String::substring(unsigned int left, unsigned int right) const noexcept {
    if (left > right) {
        // swap the two elements and continue on
        unsigned int temp = right;
        right = left;
        left = temp;
    }

    String out;
    if (left >= len_) {
        return out;
    }
    if (right > len_) {
        right = len_;
    }
    char temp = buffer_[right]; // save the replaced character
    buffer_[right] = '\0';
    out = buffer_ + left;
    buffer_[right] = temp;
    return out;
}
// modification operations
void
String::replace(char find, char replace) noexcept {
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
String::replace(const String &find, const String &replace) noexcept {
    if (len_ == 0 || find.len_ == 0) {
        return;
    }
    int diff = replace.len_ - find.len_;
    char* readFrom = buffer_;
    char* foundAt = nullptr;
    if (diff == 0) {
        while ((foundAt = strstr(readFrom, find.buffer_)) != nullptr)  {
            memcpy(foundAt, replace.buffer_, replace.len_);
            readFrom = foundAt + replace.len_;
        }
    } else if (diff < 0) {
        char* writeTo = buffer_;
        while ((foundAt = strstr(readFrom, find.buffer_)) != nullptr)  {
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
        while ((foundAt = strstr(readFrom, find.buffer_)) != nullptr)  {
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
String::remove(unsigned int index) noexcept {
    // pass the biggest integer as the count.
    // This will force the other remove method to truncate the string
    remove(index, (unsigned int)-1);
}
void
String::remove(unsigned int index, unsigned int count) noexcept {
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
String::toLowerCase() noexcept {
    if (!buffer_)  {
        return;
    } else {
        for (char* p = buffer_; *p; ++p) {
            *p = tolower(*p);
        }
    }
}
void
String::toUpperCase() noexcept {
   if (!buffer_)  {
       return;
   } else {
       for (char* p = buffer_; *p; ++p) {
           *p = toupper(*p);
       }
   }
}
void
String::trim() noexcept {
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
String::toInt() const noexcept {
   if (buffer_) {
       return atol(buffer_);
   } else {
       return 0;
   }
}

float
String::toFloat() const noexcept {
    return static_cast<float>(toDouble());
}

double
String::toDouble() const noexcept {
    if (buffer_) {
        return atof(buffer_);
    } else {
        return 0;
    }
}