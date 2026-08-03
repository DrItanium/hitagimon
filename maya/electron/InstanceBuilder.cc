/**
 * @file
 * InstanceBuilder class implementation.
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
#include "electron/InstanceBuilder.h"
#include "error/Exception.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{

PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Value* value)
{
    return ::IBPutSlot(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Integer* value)
{
    return ::IBPutSlotCLIPSInteger(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, int64 value)
{
    return ::IBPutSlotInteger(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Float* value)
{
    return ::IBPutSlotCLIPSFloat(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, double value)
{
    return ::IBPutSlotFloat(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Lexeme* value)
{
    return ::IBPutSlotCLIPSLexeme(_raw, slotName.c_str(), value);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsInstanceName)
{
    return ::IBPutSlotInstanceName(_raw, slotName.c_str(), lexeme.c_str());
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsString)
{
    return ::IBPutSlotString(_raw, slotName.c_str(), lexeme.c_str());
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsSymbol)
{
    return ::IBPutSlotSymbol(_raw, slotName.c_str(), lexeme.c_str());
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Fact* f)
{
    return ::IBPutSlotFact(_raw, slotName.c_str(), f);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Instance* i)
{
    return ::IBPutSlotInstance(_raw, slotName.c_str(), i);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, ExternalAddress* i)
{
    return ::IBPutSlotExternalAddress(_raw, slotName.c_str(), i);
}
PutSlotError 
InstanceBuilder::putSlot(const std::string& slotName, Multifield* i)
{
    return ::IBPutSlotMultifield(_raw, slotName.c_str(), i);
}

InstanceBuilder::InstanceBuilder(Environment& env, const std::string& name) noexcept : _env(env), _raw(::CreateInstanceBuilder(env.getRawEnvironment(), name.c_str())) 
{ 

}

InstanceBuilder::~InstanceBuilder() noexcept
{
    ::IBDispose(_raw);
}

Instance* 
InstanceBuilder::make(const std::string& instanceName) noexcept
{
    return ::IBMake(_raw, instanceName.c_str());
}
Instance* 
InstanceBuilder::make() noexcept
{
    return ::IBMake(_raw, nullptr);
}

InstanceBuilder::ErrorCode
InstanceBuilder::error() noexcept
{
    return ::IBError(_env.getRawEnvironment());
}

InstanceBuilder::ErrorCode 
InstanceBuilder::make(Value& out, const std::string& instanceName) noexcept
{
    out.instanceValue = make(instanceName);
    return error();
}

InstanceBuilder::ErrorCode 
InstanceBuilder::make(Value& out) noexcept
{
    out.instanceValue = make();
    return error();
}

} // end namespace Electron
