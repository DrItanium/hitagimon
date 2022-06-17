/*
hitagimon
Copyright (c) 2020-2022, Joshua Scoggins
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
#include "SPI.h"

DMAEngine::DMAEngine(uint32_t offset) : BuiltinIOBaseDevice(offset), numRegisteredRequests_(0) { }
DMAEngine::~DMAEngine() {

}

bool 
DMAEngine::hasPendingRequests() const {
    for (int i = 0; i < 10; ++i) {
        const Request& curr = requests_[i];
        if (curr.isValid() && !curr.isFulfilled()) {
            return true;
        }
    }
    return false;
}
void
DMAEngine::Request::clear() {
    src = 0;
    destination = 0;
    full = 0;
}
void
DMAEngine::clear()
{
    for (int i = 0; i < 10; ++i) {
        requests_[i].clear();
    }
    numRegisteredRequests_ = 0;
}
void
DMAEngine::begin()
{
    static bool initialized = false;
    if (!initialized) {
        initialized = true;
        clear();
    }
}

bool
DMAEngine::registerRequest(Buffer *src, Buffer *dest, uint8_t count, bool incrementSourceAddress, bool incrementDestinationAddress) {
    if (numRegisteredRequests_ < 10) {
        // find the first invalid
        Request& curr = requests_[numRegisteredRequests_];
        curr.count = count;
        curr.src = src;
        curr.destination = dest;
        curr.incrementDestinationAddress = incrementDestinationAddress;
        curr.incrementSourceAddress = incrementSourceAddress;
        ++numRegisteredRequests_;
        return true;
    } else {
        return false;
    }
}

bool
DMAEngine::processRequest() {
    if (!empty()) {
        // find the first valid unfulfilled element

    }
}
