/**
 * @file
 * FactBuilder class implementation.
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
#include "electron/FactBuilder.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{

FactBuilder::FactBuilder(Environment& env, const std::string& templateName) : 
    _env(env),
    _raw(::CreateFactBuilder(env.getRawEnvironment(), templateName.c_str()))
{

}

FactBuilder::~FactBuilder() 
{
    ::FBDispose(_raw);
}

Fact* 
FactBuilder::make()
{
    return ::FBAssert(_raw);
}
void 
FactBuilder::abort()
{
    ::FBAbort(_raw);
}
FactBuilder::ErrorCode 
FactBuilder::error() noexcept
{
    return ::FBError(_env.getRawEnvironment());
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Value* value)
{
    return ::FBPutSlot(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Integer* value)
{
    return ::FBPutSlotCLIPSInteger(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, int64 value)
{
    return ::FBPutSlotInteger(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Float* value)
{
    return ::FBPutSlotCLIPSFloat(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, double value)
{
    return ::FBPutSlotFloat(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Lexeme* value)
{
    return ::FBPutSlotCLIPSLexeme(_raw, slotName.c_str(), value);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsInstanceName)
{
    return ::FBPutSlotInstanceName(_raw, slotName.c_str(), lexeme.c_str());
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsString)
{
    return ::FBPutSlotString(_raw, slotName.c_str(), lexeme.c_str());
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsSymbol)
{
    return ::FBPutSlotSymbol(_raw, slotName.c_str(), lexeme.c_str());
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Fact* f)
{
    return ::FBPutSlotFact(_raw, slotName.c_str(), f);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Instance* i)
{
    return ::FBPutSlotInstance(_raw, slotName.c_str(), i);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, ExternalAddress* i)
{
    return ::FBPutSlotCLIPSExternalAddress(_raw, slotName.c_str(), i);
}

PutSlotError 
FactBuilder::putSlot(const std::string& slotName, Multifield* i)
{
    return ::FBPutSlotMultifield(_raw, slotName.c_str(), i);
}

} // end namespace Electron
