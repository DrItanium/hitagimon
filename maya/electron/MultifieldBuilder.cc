/**
 * @file
 * MultifieldBuilder class implementation.
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
#include "electron/MultifieldBuilder.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{
// begin MultifieldBuilder stuff

MultifieldBuilder::MultifieldBuilder(Environment& env, size_t initialCapacity) : _raw(::CreateMultifieldBuilder(env.getRawEnvironment(), initialCapacity)) { }
MultifieldBuilder::~MultifieldBuilder()
{
    ::MBDispose(_raw);
}
void
MultifieldBuilder::reset() noexcept
{
    ::MBReset(_raw);
}

Multifield*
MultifieldBuilder::create() noexcept
{
    return ::MBCreate(_raw);
}
void
MultifieldBuilder::append(Value* value) noexcept
{
    ::MBAppend(_raw, value);
}
void
MultifieldBuilder::append(UDFValue* value) noexcept
{
    ::MBAppendUDFValue(_raw, value);
}
void
MultifieldBuilder::append(int64 value) noexcept
{
    ::MBAppendInteger(_raw, value);
}
void
MultifieldBuilder::append(double value) noexcept
{
    ::MBAppendFloat(_raw, value);
}
void
MultifieldBuilder::append(const std::string& value, TreatLexemeAsString) noexcept
{
    ::MBAppendString(_raw, value.c_str());
}
void
MultifieldBuilder::append(const std::string& value, TreatLexemeAsSymbol) noexcept
{
    ::MBAppendSymbol(_raw, value.c_str());
}
void
MultifieldBuilder::append(const std::string& value, TreatLexemeAsInstanceName) noexcept
{
    ::MBAppendInstanceName(_raw, value.c_str());
}
void
MultifieldBuilder::append(Integer* value) noexcept
{
    ::MBAppendCLIPSInteger(_raw, value);
}
void
MultifieldBuilder::append(Float* value) noexcept
{
    ::MBAppendCLIPSFloat(_raw, value);
}
void
MultifieldBuilder::append(Instance* value) noexcept
{
    ::MBAppendInstance(_raw, value);
}
void
MultifieldBuilder::append(::Fact* value) noexcept
{
    ::MBAppendFact(_raw, value);
}
void
MultifieldBuilder::append(ExternalAddress* value) noexcept
{
    ::MBAppendCLIPSExternalAddress(_raw, value);
}

void
MultifieldBuilder::append(Lexeme* value) noexcept
{
    ::MBAppendCLIPSLexeme(_raw, value);
}

} // end namespace Electron
