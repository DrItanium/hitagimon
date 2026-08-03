/**
 * @file
 * C++ wrapper around the defrule struct 
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

#ifndef LibElectron_DefruleInspector_h__
#define LibElectron_DefruleInspector_h__
#include "electron/Environment.h"

namespace Electron
{
class DefruleInspector
{
public:
    using RawType = Defrule;
    using RawVerbosity = ::Verbosity;
    using MatchResult = std::tuple<int64, int64, int64>;
public:
    DefruleInspector(Environment& env, RawType* raw) : _env(env), _raw(raw) { }
    ~DefruleInspector() = default;
    bool isDeletable() const noexcept { return ::DefruleIsDeletable(_raw); }
    bool undef() noexcept             { return ::Undefrule(_raw, _env.getRawEnvironment()); }
    // watch related
    bool activationsWatched() const noexcept           { return ::DefruleGetWatchActivations(_raw); }
    bool firingsWatched() const noexcept               { return ::DefruleGetWatchFirings(_raw); }
    void watchActivations(bool enable = true) noexcept { ::DefruleSetWatchActivations(_raw, enable); }
    void watchFirings(bool enable = true) noexcept     { ::DefruleSetWatchFirings(_raw, enable); }
    // breakpoint related
    bool hasBreakpoint() const noexcept { return ::DefruleHasBreakpoint(_raw); }
    bool removeBreakpoint() noexcept { return ::RemoveBreak(_raw); }
    void enableBreakpoint() noexcept { return ::SetBreak(_raw); }
    // match related
    MatchResult matches(RawVerbosity verbosity = RawVerbosity::TERSE) noexcept;
    /// @todo add match related routine, the result is complicated
    // refresh
    void refresh() noexcept { ::Refresh(_raw); }



private:
    Environment& _env;
    RawType* _raw;


};

} // end namespace Electron

#endif // end LibElectron_DefruleInspector_h__
