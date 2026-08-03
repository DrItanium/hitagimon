/**
 * @file
 * FactModifier class implementation.
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
#include "electron/FactModifier.h"
#include "error/Exception.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{
FactModifier::FactModifier(Environment& env, Fact* target) : _env(env),
    _raw(::CreateFactModifier(env.getRawEnvironment(), target))
{

}

FactModifier::~FactModifier()
{
    ::FMDispose(_raw);
}

void
FactModifier::abort()
{
    ::FMAbort(_raw);
}

Fact*
FactModifier::modify()
{
    return ::FMModify(_raw);
}

FactModifier::ErrorCode
FactModifier::error()
{
    return ::FMError(_env.getRawEnvironment());
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Value* value)
{
    return ::FMPutSlot(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Integer* value)
{
    return ::FMPutSlotCLIPSInteger(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, int64 value)
{
    return ::FMPutSlotInteger(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Float* value)
{
    return ::FMPutSlotCLIPSFloat(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, double value)
{
    return ::FMPutSlotFloat(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Lexeme* value)
{
    return ::FMPutSlotCLIPSLexeme(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsInstanceName)
{
    return ::FMPutSlotInstanceName(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsString)
{
    return ::FMPutSlotString(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsSymbol)
{
    return ::FMPutSlotSymbol(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Fact* value)
{
    return ::FMPutSlotFact(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Instance* value)
{
    return ::FMPutSlotInstance(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, ExternalAddress* value)
{
    return ::FMPutSlotExternalAddress(_raw, slotName.c_str(), value);
}

PutSlotError
FactModifier::putSlot(const std::string& slotName, Multifield* value)
{
    return ::FMPutSlotMultifield(_raw, slotName.c_str(), value);
}

} // end namespace Electron
