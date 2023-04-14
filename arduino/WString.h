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

class __FlashStringHelper;
#define F(string_literal) (reinterpret_cast<const __FlashStringHelper*>(PSTR(string_literal)))

/// @brief An inherited class for holding the result of a concatenation. These result objects are assumed to be writable by subsequent concatenations
class StringSumHelper;

class String {
   // use a function pointer ot allow for "if (s)" without the complications of operator bool().

   typedef void (String::*StringIfHelperType)() const;
   void StringIfHelper() const { };
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

    inline unsigned int length() const { return len_; }
    String& operator=(const String& rhs);
    String& operator=(const char* cstr);
    String& operator=(const __FlashStringHelper* str);

    unsigned char concat(const String& str);
    unsigned char concat(const char* str);
    unsigned char concat(char c);
    unsigned char concat(unsigned char c);
    unsigned char concat(int num);
    unsigned char concat(unsigned int num);
    unsigned char concat(long num);
    unsigned char concat(unsigned long num);
    unsigned char concat(float num);
    unsigned char concat(double num);
    unsigned char concat(const __FlashStringHelper* str);

#define X(input) \
    inline String& operator+=(input rhs) { concat(rhs); return (*this); } \
    friend StringSumHelper& operator+(const StringSumHelper& lhs, input rhs )

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

    operator StringIfHelperType() const { return buffer_ ? &String::StringIfHelper : 0; }
    int compareTo(const String& s) const;
    unsigned char equals(const String& s) const;
    unsigned char equals(const char* s) const;
    unsigned char operator==(const String& rhs) const { return equals(rhs); }
    unsigned char operator==(const char* rhs) const { return equals(rhs); }
    unsigned char operator!=(const String& rhs) const { return !equals(rhs); }
    unsigned char operator!=(const char* rhs) const { return !equals(rhs); }
    unsigned char operator<(const String& rhs) const;
    unsigned char operator>(const String& rhs) const;
    unsigned char operator<=(const String& rhs) const;
    unsigned char operator>=(const String& rhs) const;
    unsigned char equalsIgnoreCase(const String& s) const ;
    unsigned char startsWith(const String& s) const ;
    unsigned char startsWith(const String& s, unsigned int offset) const ;
    unsigned char endsWith(const String& suffix) const;

    // character accessors
    char charAt(unsigned int index) const;
    void setCharAt(unsigned int index, char c);
    char operator[](unsigned int index) const;
    char& operator[](unsigned int index);
    void getBytes(unsigned char* buf, unsigned int bufSize, unsigned int index = 0) const;
    inline void toCharArray(unsigned char* buf, unsigned int bufSize, unsigned int index = 0) const {
        getBytes((unsigned char*) buf, bufSize, index);
    }
    const char* c_str() const { return buffer_; }
    char* begin() { return buffer_; }
    char* end() { return buffer_ + length(); }
    const char* begin() const { return c_str(); }
    const char* end() const { return c_str() + length(); }

    // search routines
    int indexOf(char ch) const;
    int indexOf(char ch, unsigned int fromIndex) const;
    int indexOf(const String& str) const;
    int indexOf(const String& str, unsigned int fromIndex) const;
    int lastIndexOf(char ch) const;
    int lastIndexOf(char ch, unsigned int fromIndex) const;
    int lastIndexOf(const String& str) const;
    int lastIndexOf(const String& str, unsigned int fromIndex) const;
    String substring(unsigned int beginIndex) const { return substring(beginIndex, len_); }
    String substring(unsigned int beginIndex, unsigned int endIndex) const;


    // modification
    void replace(char find, char replace);
    void replace(const String& find, const String& replace);
    void remove(unsigned int index);
    void remove(unsigned int index, unsigned int count);
    void toLowerCase();
    void toUpperCase();
    void trim();

    long toInt() const;
    float toFloat() const;
    double toDouble() const;
protected:
    char* buffer_;
    unsigned int capacity_;
    unsigned int len_;
protected:
    void init();
    void invalidate();
    unsigned char changeBuffer(unsigned int maxStrLength);
    unsigned char concat(const char* cstr, unsigned int length);

    String& copy(const char* cstr, unsigned int length);
    String& copy(const __FlashStringHelper* pstr, unsigned int length);
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
