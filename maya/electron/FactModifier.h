/**
 * @file
 * Fact modification class, makes it trivial to update deftemplates
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

#ifndef __LibElectron_FactModifier_h__
#define __LibElectron_FactModifier_h__
#include "electron/Environment.h"

namespace Electron
{
/**
 * Programatic equivalent to modify
 */
class FactModifier
{
public:
    using RawType = ::FactModifier;
    using ErrorCode = ::FactModifierError;
public:
    FactModifier(Environment& env, Fact* target);
    ~FactModifier();
    Fact* modify();
    template<typename T>
    ErrorCode modify(T* out)
    {
        out->factValue = modify();
        return error();
    }
    void abort();
    ErrorCode error();
    PutSlotError putSlot(const std::string& slotName, Value* value);
    PutSlotError putSlot(const std::string& slotName, Integer* value);
    PutSlotError putSlot(const std::string& slotName, int64 value);
    PutSlotError putSlot(const std::string& slotName, Float* value);
    PutSlotError putSlot(const std::string& slotName, double value);
    PutSlotError putSlot(const std::string& slotName, Lexeme* value);
    PutSlotError putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsString);
    PutSlotError putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsSymbol);
    PutSlotError putSlot(const std::string& slotName, const std::string& lexeme, TreatLexemeAsInstanceName);
    template<typename Tag>
    PutSlotError putSlot(const std::string& slotName, const std::tuple<std::string, Tag>& tup)
    {
        return putSlot(slotName, std::get<0>(tup), Tag{});
    }
    PutSlotError putSlot(const std::string& slotName, Fact* f);
    PutSlotError putSlot(const std::string& slotName, Instance* i);
    PutSlotError putSlot(const std::string& slotName, ExternalAddress* i);
    PutSlotError putSlot(const std::string& slotName, Multifield* i);
private:
    Environment& _env;
    RawType* _raw;
};

} // end namespace Electron
#endif // end __LibElectron_FactModifier_h__
