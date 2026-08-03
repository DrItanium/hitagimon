/**
 * @file
 * Construct multifields programatically
 * @copyright
 * Copyright (c) 2015-2022 Parasoft Corporation
 *
 * This software is provided 'as-is', without any express or implied
 * warranty. In no event will the authors be held liable for any damages
 * arising from the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 *    claim that you wrote the original software. If you use this software
 *    in a product, an acknowledgment in the product documentation would be
 *    appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 *    misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 */

#ifndef __LibElectron_MultifieldBuilder_h__
#define __LibElectron_MultifieldBuilder_h__
#include "electron/Environment.h"

namespace Electron 
{
/**
 * Programatic equivalent to create$
 */
class MultifieldBuilder
{
public:
    using RawType = ::MultifieldBuilder;
public:
    MultifieldBuilder(Environment& env, size_t initialCapacity = 10);
    ~MultifieldBuilder();
    void reset() noexcept;
    Multifield* create() noexcept;
    auto length() const noexcept { return _raw->length; }
    void append(Value* value) noexcept;
    void append(UDFValue* value) noexcept;
    void append(int64 value) noexcept;
    void append(double value) noexcept;
    void append(Integer* value) noexcept;
    void append(Float* value) noexcept;
    void append(Lexeme* value) noexcept;
    void append(Instance* value) noexcept;
    void append(Fact* value) noexcept;
    void append(ExternalAddress* value) noexcept;
    void append(const std::string& lexeme, TreatLexemeAsString) noexcept;
    void append(const std::string& lexeme, TreatLexemeAsSymbol) noexcept;
    void append(const std::string& lexeme, TreatLexemeAsInstanceName) noexcept;
    template<typename Tag>
    void append(const std::tuple<std::string, Tag>& tup) noexcept
    {
        append(std::get<0>(tup), Tag{});
    }

private:
    RawType* _raw;
};
} // end namespace Electron

#endif // end __LibElectron_MultifieldBuilder_h__
