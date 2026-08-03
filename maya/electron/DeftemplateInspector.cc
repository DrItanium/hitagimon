/**
 * @file
 * DeftemplateInspector class implementation.
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
#include "electron/DeftemplateInspector.h"

namespace Electron
{
bool
DeftemplateInspector::isWatched() const noexcept
{
    return ::DeftemplateGetWatch(_raw);
}

void
DeftemplateInspector::watch(bool enable) noexcept
{
    ::DeftemplateSetWatch(_raw, enable);
}

std::string
DeftemplateInspector::getModule() const noexcept
{
    return std::string(::DeftemplateModule(_raw));
}

std::string
DeftemplateInspector::getName() const noexcept
{
    return std::string(::DeftemplateName(_raw));
}

std::string
DeftemplateInspector::getPrettyPrintForm() const noexcept
{
    return std::string(::DeftemplatePPForm(_raw));
}

std::list<std::string>
DeftemplateInspector::getSlotNames() const noexcept
{
    std::list<std::string> names;

    Value value;
    ::DeftemplateSlotNames(_raw, &value);
    auto mf = value.multifieldValue;
    for (auto i = 0; i < mf->length; ++i) {
        names.emplace_back(mf->contents[i].lexemeValue->contents);
    }
    return names;
}

bool 
DeftemplateInspector::slotExists(const std::string& slotName) const noexcept
{
    return ::DeftemplateSlotExistP(_raw, slotName.c_str());
}

bool 
DeftemplateInspector::slotIsMulti(const std::string& slotName) const noexcept
{
    return ::DeftemplateSlotMultiP(_raw, slotName.c_str());
}

bool 
DeftemplateInspector::slotIsSingle(const std::string& slotName) const noexcept
{
    return ::DeftemplateSlotSingleP(_raw, slotName.c_str());
}

DeftemplateInspector::RawDefaultType 
DeftemplateInspector::slotDefaultType(const std::string& slotName) const noexcept
{
    return ::DeftemplateSlotDefaultP(_raw, slotName.c_str());
}

bool
DeftemplateInspector::isDeletable() const noexcept {
    return ::DeftemplateIsDeletable(_raw);
}


} // end namespace Electron
