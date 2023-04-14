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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "Arduino.h"
#include "Print.h"

size_t
Print::write(const uint8_t* buffer, size_t size) {
    size_t n = 0;
    while (size--) {
        if (write(*buffer++)) {
            ++n;
        } else {
            break;
        }
    }
    return n;
}

size_t
Print::print(const char str[]) {
    return write(str);
}

size_t
Print::print(char c) {
    return write(c);
}

size_t
Print::print(unsigned char b, int base) {
    return print((unsigned long) b, base);
}

size_t
Print::print(int b, int base) {
    return print((long) b, base);
}

size_t
Print::print(unsigned int b, int base) {
    return print((unsigned long) b, base);
}

size_t
Print::print(long n, int base) {
    if (base == 0) {
        return write(n);
    } else if (base == 10) {
        if (n < 0) {
            int t = print('-');
            n = -n;
            return printNumber(n, 10) + t;
        }
        return printNumber(n, 10);
    } else {
        return printNumber(n, base);
    }
}

size_t
Print::print(unsigned long n, int base) {
    if (base == 0)  {
        return write(n);
    } else {
       return printNumber(n, base) ;
    }
}

size_t
Print::print(const Printable& x) {
   return x.printTo(*this) ;
}

size_t
Print::print(double n, int digits) {
    return printFloat(n, digits);
}

size_t Print::println() { return write("\r\n"); }
size_t Print::println(const char c[]){ return print(c) + println(); }
size_t Print::println(char c){ return print(c) + println(); }
size_t Print::println(unsigned char c, int base){ return print(c, base) + println(); }
size_t Print::println(int c, int base){ return print(c, base) + println(); }
size_t Print::println(unsigned int c, int base){ return print(c, base) + println(); }
size_t Print::println(long c, int base){ return print(c, base) + println(); }
size_t Print::println(unsigned long c, int base){ return print(c, base) + println(); }
size_t Print::println(double c, int digits){ return print(c, digits) + println(); }
size_t Print::println(const Printable& x) { return print(x) + println();}