/**
 * @file
 * C++ wrapper class for interacting with a deftemplate object
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

#ifndef LibElectron_DeftemplateInspector_h__
#define LibElectron_DeftemplateInspector_h__
#include "electron/Environment.h"

namespace Electron
{

/**
 * Use this class to query information about a given deftemplate not a specific instance
 */
class DeftemplateInspector
{
public:
    using RawType = Deftemplate;
    using RawDefaultType = ::DefaultType;
public:
    DeftemplateInspector(Environment& env, RawType* type) : _env(env), _raw(type) { }
    ~DeftemplateInspector() = default;
    bool isWatched() const noexcept;
    void watch(bool enable = true) noexcept;
    // attributes
    std::string getModule() const noexcept;
    std::string getName() const noexcept;
    std::string getPrettyPrintForm() const noexcept;
    std::list<std::string> getSlotNames() const noexcept;
    // deletion
    bool isDeletable() const noexcept;
    /// @todo provide support for undeftemplate if it makes sense
    // slot attributes
    /// @todo implement support for querying slot attributes
    // slot predicates
    bool slotExists(const std::string& slotName) const noexcept;
    bool slotIsMulti(const std::string& slotName) const noexcept;
    bool slotIsSingle(const std::string& slotName) const noexcept;
    RawDefaultType slotDefaultType(const std::string& slotName) const noexcept;
    bool slotHasNoDefault(const std::string& slotName) const noexcept      { return slotDefaultType(slotName) == RawDefaultType::NO_DEFAULT; }
    bool slotHasStaticDefault(const std::string& slotName) const noexcept  { return slotDefaultType(slotName) == RawDefaultType::STATIC_DEFAULT; }
    bool slotHasDynamicDefault(const std::string& slotName) const noexcept { return slotDefaultType(slotName) == RawDefaultType::DYNAMIC_DEFAULT; }
    /// @todo make it possible to iterate through the set of facts that are of this deftemplate type

private:
    Environment& _env;
    RawType* _raw;
};

} // end namespace Electron

#endif // end LibElectron_DeftemplateInspector_h__
