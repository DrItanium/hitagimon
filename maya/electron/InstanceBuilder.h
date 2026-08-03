/**
 * @file
 * C++ wrapper class to make building instances easy
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

#ifndef __LibElectron_InstanceBuilder_h__
#define __LibElectron_InstanceBuilder_h__
#include "electron/Environment.h"
namespace Electron 
{ 
/**
 * Programatic equivalent to make-instance
 */
class InstanceBuilder
{
public:
    using RawType = ::InstanceBuilder;
    using ErrorCode = ::InstanceBuilderError;
public:
    InstanceBuilder(Environment& env, const std::string& className) noexcept;
    ~InstanceBuilder() noexcept;
    Instance* make(const std::string& instanceName) noexcept;
    Instance* make() noexcept;
    ErrorCode make(Value& out, const std::string& instanceName) noexcept;
    ErrorCode make(Value& out) noexcept;
    void abort();
    ErrorCode error() noexcept;
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
    /**
     * Convert a native C++ into its corresponding electron type and put it into
     * target named slot.
     * @param slotName the slot to install the constructed type into
     * @param value A pointer to the given thing to put
     * @return error code if something went wrong
     * @tparam T the native type to try and encode
     */
    template<typename T>
    PutSlotError putSlot(const std::string& slotName, T* value)
    {
        if (!value) {
            throw Neutron::Exception(__HERE__, "nullptr provided!");
        }
        return putSlot(slotName, _env.createExternalAddress(value));
    }
    template<typename T>
    PutSlotError putSlot(const std::string& slotName, std::shared_ptr<T>& ptr)
    {
        if (!ptr) {
            throw Neutron::Exception(__HERE__, "empty shared ptr provided!");
        }
        return putSlot(slotName, ptr.get());
    }
    template<typename T>
    PutSlotError putSlot(const std::string& slotName, std::unique_ptr<T>& ptr)
    {
        if (!ptr) {
            throw Neutron::Exception(__HERE__, "empty unique ptr provided!");
        }
        return putSlot(slotName, ptr.get());
    }

    inline PutSlotError putSlot(const std::string& slotName, uint32 value) { return putSlot(slotName, static_cast<int64>(value)); }
private:
    Environment& _env;
    RawType* _raw;

};
} // end namespace Electron
#endif // end __LibElectron_InstanceBuilder_h__
