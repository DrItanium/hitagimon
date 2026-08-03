/**
 * @file
 * InstanceModifier class implementation.
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
#include "electron/InstanceModifier.h"
#include "error/Exception.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{
InstanceModifier::InstanceModifier(Environment& env, Instance* target) :
    _env(env),
    _raw(::CreateInstanceModifier(env.getRawEnvironment(), target))
{

}

InstanceModifier::~InstanceModifier()
{
    ::IMDispose(_raw);
}
Instance*
InstanceModifier::modify()
{
    return ::IMModify(_raw);
}
void
InstanceModifier::abort()
{
    ::IMAbort(_raw);
}

InstanceModifier::ErrorCode
InstanceModifier::error()
{
    return ::IMError(_env.getRawEnvironment());
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Value* value)
{
    return ::IMPutSlot(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Integer* value)
{
    return ::IMPutSlotCLIPSInteger(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, int64 value)
{
    return ::IMPutSlotInteger(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Float* value)
{
    return ::IMPutSlotCLIPSFloat(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, double value)
{
    return ::IMPutSlotFloat(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Lexeme* value)
{
    return ::IMPutSlotCLIPSLexeme(_raw, slotName.c_str(), value);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsInstanceName)
{
    return ::IMPutSlotInstanceName(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsString)
{
    return ::IMPutSlotString(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, const std::string& value, TreatLexemeAsSymbol)
{
    return ::IMPutSlotSymbol(_raw, slotName.c_str(), value.c_str());
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Fact* f)
{
    return ::IMPutSlotFact(_raw, slotName.c_str(), f);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Instance* i)
{
    return ::IMPutSlotInstance(_raw, slotName.c_str(), i);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, ExternalAddress* i)
{
    return ::IMPutSlotExternalAddress(_raw, slotName.c_str(), i);
}

PutSlotError
InstanceModifier::putSlot(const std::string& slotName, Multifield* i)
{
    return ::IMPutSlotMultifield(_raw, slotName.c_str(), i);
}

} // end namespace Electron
