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

#ifndef HITAGIMON_IPADDRESS_H
#define HITAGIMON_IPADDRESS_H
#include <stdint.h>
#include <cortex/ModernCpp.h>
#include "Printable.h"
#include "WString.h"

class IPAddress : public Printable {
public:
    IPAddress();
    IPAddress(uint8_t first, uint8_t second, uint8_t third, uint8_t fourth);
    IPAddress(uint32_t address);
    IPAddress(const uint8_t* address);

    bool fromString(const char* address);
    bool fromString(const String& address) { return fromString(address.c_str()); }

    operator uint32_t() const { return address_.dword; }
    bool operator==(const IPAddress& other) const { return address_.dword == other.address_.dword; }
    bool operator==(const uint8_t* addr) const;

    uint8_t operator[](int index) const { return address_.bytes[index]; }
    uint8_t& operator[](int index) { return address_.bytes[index]; }

    IPAddress& operator=(const uint8_t* address);
    IPAddress& operator=(uint32_t address);

    virtual size_t printTo(Print& p) const override;

    friend class EthernetClass;
    friend class UDP;
    friend class Client;
    friend class Server;
    friend class DhpClass;
    friend class DNSClient;
private:
    uint8_t* rawAddress() noexcept {
        return address_.bytes;
    }
private:
    union {
        uint8_t bytes[4];
        uint32_t dword;
    } address_;
};

const IPAddress INADDR_NONE(0,0,0,0);

#endif //HITAGIMON_IPADDRESS_H
