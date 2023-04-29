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
size_t
Print::println(char c) {
    size_t n = print(c);
    n += println();
    return n;
}

size_t
Print::println(const Printable& x) {
    size_t n = print(x);
    n += println();
    return n;
}

size_t
Print::println(const char c[]) {
    size_t n = print(c);
    n += println();
    return n;
}
// since this version of C++ doesn't actually explicitly describe the order of evaluation, we need to break the actions
// up into separate lines. In later versions of C++, the evaluation order was specified so it was safe to do (for example):
// return print(c, digits) + println();
size_t
Print::println(double c, int digits) {
    size_t n = print(c, digits);
    n += println();
    return n;
}

size_t
Print::println(unsigned char c, int base) {
    size_t n = print(c, base);
    n += println();
    return n;
}

size_t
Print::println(unsigned long c, int base) {
    size_t n = print(c, base);
    n += println();
    return n;
}

size_t
Print::println(long c, int base) {
    size_t n = print(c, base);
    n += println();
    return n;
}

size_t
Print::println(unsigned int c, int base) {
    size_t n = print(c, base);
    n += println();
    return n;
}

size_t
Print::println(int c, int base) {
    size_t n = print(c, base);
    n += println();
    return n;
}


size_t
Print::printNumber(unsigned long n, uint8_t base) {
    char buffer[8 * sizeof(long) + 1];
    char* str = &buffer[sizeof(buffer) - 1];
    *str = '\0';

    if (base < 2)  {
        base = 10;
    }
    do {
        char c = n % base;
        n /= base;
        // taken directly from Print.cc
        // blergs *--str
        *--str = c < 10 ? c + '0' : c + 'A' - 10;
    } while (n);
    return write(str);
}

size_t
Print::printFloat(double number,uint8_t digits) {
    size_t n = 0;
    if (isnan(number)) {
        return print("nan");
    }
    if (isinf(number)) {
        return print("inf");
    }
    if (number > 4294967040.0) {
        // according to arduino impl, this is "determined empirically
        return print ("ovf");
    }
    if (number < -4294967040.0) {
        // according to arduino impl, this is "determined empirically
        return print ("ovf");
    }
    // handle the negative sign
    if (number < 0.0) {
        n += print('-');
        number = -number;
    }
    // round correctly so that print(1.999, 2) prints as "2.00"
    double rounding = 0.5;
    for (uint8_t i = 0; i < digits; ++i) {
        rounding /= 10.0;
    }
    number += rounding;


    // extrac the integer part of the number and print it
    unsigned long integerPart = (unsigned long)number;
    double remainder = number - (double)integerPart;
    n += print(integerPart);

    // print the decimal point, but only if there are digits beyond
    if (digits > 0) {
        n += print ('.');
    }

    // extract digits from the remainder one at a time
    while (digits-- > 0) {
        remainder *= 10.0;
        unsigned int toPrint = (unsigned int)(remainder);
        n += print(toPrint);
        remainder -= toPrint;
    }
    return n;
}

size_t
Print::print(const __FlashStringHelper* str) {
    PGM_P p = reinterpret_cast<PGM_P>(str);
    size_t n = 0;
    while (true) {
        unsigned char c = pgm_read_byte(p++);
        if (c == 0) {
            break;
        }
        if (write(c)) {
            ++n;
        } else {
            break;
        }
    }
    return n;
}

size_t
Print::println(const __FlashStringHelper* str) {
    size_t n = print(str);
    n += println();
    return n;
}

