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

#ifndef HITAGIMON_WSTRING_H
#define HITAGIMON_WSTRING_H
#ifdef __cplusplus
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "avr/pgmspace.h"
#include <cortex/ModernCpp.h>

class __FlashStringHelper;
#define F(string_literal) (reinterpret_cast<const __FlashStringHelper*>(PSTR(string_literal)))

/// @brief An inherited class for holding the result of a concatenation. These result objects are assumed to be writable by subsequent concatenations
class StringSumHelper;

class String {
   // use a function pointer ot allow for "if (s)" without the complications of operator bool().

   typedef void (String::*StringIfHelperType)() const noexcept;
   void StringIfHelper() const noexcept { };
public:
    String(const char* cstr = "");
    String(const String& str);
    String(const __FlashStringHelper* str);
    explicit String(char c);
    explicit String(unsigned char, unsigned char base = 10);
    explicit String(int, unsigned char base = 10);
    explicit String(unsigned int, unsigned char base = 10);
    explicit String(long, unsigned char base = 10);
    explicit String(unsigned long, unsigned char base = 10);
    explicit String(float, unsigned char decimalPlaces = 2);
    explicit String(double, unsigned char decimalPlaces = 2);
    ~String();

    // memory management routines

    /**
     * @brief Reserve some amount of space or validate an invalid string (when passing zero)
     * @param size The number cells to allocate, if zero then it will validate an invalid string
     * @return true on success, false on failure (in which case, the string is left unchanged)
     */
    unsigned char reserve(unsigned int size);

    inline constexpr unsigned int length() const noexcept { return len_; }
    String& operator=(const String& rhs) noexcept;
    String& operator=(const char* cstr) noexcept;
    String& operator=(const __FlashStringHelper* str) noexcept;

    unsigned char concat(const String& str) noexcept;
    unsigned char concat(const char* str) noexcept;
    unsigned char concat(char c) noexcept;
    unsigned char concat(unsigned char c) noexcept;
    unsigned char concat(int num) noexcept;
    unsigned char concat(unsigned int num) noexcept;
    unsigned char concat(long num) noexcept;
    unsigned char concat(unsigned long num) noexcept;
    unsigned char concat(float num) noexcept;
    unsigned char concat(double num) noexcept;
    unsigned char concat(const __FlashStringHelper* str) noexcept;

#define X(input) \
    inline String& operator+=(input rhs) noexcept { concat(rhs); return (*this); } \
    friend StringSumHelper& operator+(const StringSumHelper& lhs, input rhs ) noexcept

    X(const String&);
    X(const char*);
    X(char);
    X(unsigned char);
    X(int);
    X(unsigned int);
    X(long);
    X(unsigned long);
    X(float);
    X(double);
    X(const __FlashStringHelper*);
#undef X

    operator StringIfHelperType() const noexcept { return buffer_ ? &String::StringIfHelper : 0; }
    int compareTo(const String& s) const noexcept;
    unsigned char equals(const String& s) const noexcept;
    unsigned char equals(const char* s) const noexcept;
    unsigned char operator==(const String& rhs) const noexcept { return equals(rhs); }
    unsigned char operator==(const char* rhs) const noexcept { return equals(rhs); }
    unsigned char operator!=(const String& rhs) const noexcept { return !equals(rhs); }
    unsigned char operator!=(const char* rhs) const noexcept { return !equals(rhs); }
    unsigned char operator<(const String& rhs) const noexcept;
    unsigned char operator>(const String& rhs) const noexcept;
    unsigned char operator<=(const String& rhs) const noexcept;
    unsigned char operator>=(const String& rhs) const noexcept;
    unsigned char equalsIgnoreCase(const String& s) const  noexcept;
    unsigned char startsWith(const String& s) const  noexcept;
    unsigned char startsWith(const String& s, unsigned int offset) const  noexcept;
    unsigned char endsWith(const String& suffix) const noexcept;

    // character accessors
    char charAt(unsigned int index) const noexcept;
    void setCharAt(unsigned int index, char c) noexcept;
    char operator[](unsigned int index) const noexcept;
    char& operator[](unsigned int index) noexcept;
    void getBytes(unsigned char* buf, unsigned int bufSize, unsigned int index = 0) const noexcept;
    inline void toCharArray(unsigned char* buf, unsigned int bufSize, unsigned int index = 0) const noexcept {
        getBytes((unsigned char*) buf, bufSize, index);
    }
    constexpr const char* c_str() const noexcept { return buffer_; }
    char* begin() noexcept { return buffer_; }
    char* end() noexcept { return buffer_ + length(); }
    const char* begin() const noexcept { return c_str(); }
    const char* end() const noexcept { return c_str() + length(); }
    inline const char* cbegin() const noexcept { return begin(); }
    inline const char* cend() const noexcept { return end(); }

    // search routines
    int indexOf(char ch) const noexcept;
    int indexOf(char ch, unsigned int fromIndex) const noexcept;
    int indexOf(const String& str) const noexcept;
    int indexOf(const String& str, unsigned int fromIndex) const noexcept;
    int lastIndexOf(char ch) const noexcept;
    int lastIndexOf(char ch, unsigned int fromIndex) const noexcept;
    int lastIndexOf(const String& str) const noexcept;
    int lastIndexOf(const String& str, unsigned int fromIndex) const noexcept;
    String substring(unsigned int beginIndex) const noexcept { return substring(beginIndex, len_); }
    String substring(unsigned int beginIndex, unsigned int endIndex) const noexcept;


    // modification
    void replace(char find, char replace) noexcept;
    void replace(const String& find, const String& replace) noexcept;
    void remove(unsigned int index) noexcept;
    void remove(unsigned int index, unsigned int count) noexcept;
    void toLowerCase() noexcept;
    void toUpperCase() noexcept;
    void trim() noexcept;

    long toInt() const noexcept;
    float toFloat() const noexcept;
    double toDouble() const noexcept;
protected:
    char* buffer_;
    unsigned int capacity_;
    unsigned int len_;
protected:
    void invalidate() noexcept;
    unsigned char changeBuffer(unsigned int maxStrLength) noexcept;
    unsigned char concat(const char* cstr, unsigned int length) noexcept;

    String& copy(const char* cstr, unsigned int length) noexcept;
    String& copy(const __FlashStringHelper* pstr, unsigned int length) noexcept;
};

class StringSumHelper : public String {
public:
#define X(type) StringSumHelper(type s) : String(s) { }
    X(const String&);
    X(const char*);
    X(char);
    X(unsigned char);
    X(int);
    X(unsigned int);
    X(long);
    X(unsigned long);
    X(float);
    X(double);
#undef X
};
#endif // __cplusplus
#endif //HITAGIMON_WSTRING_H
