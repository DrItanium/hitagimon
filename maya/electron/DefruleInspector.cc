/**
 * @file
 * DefruleInspector class implementation.
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

#include "electron/DefruleInspector.h"

namespace Electron
{
DefruleInspector::MatchResult
DefruleInspector::matches(DefruleInspector::RawVerbosity v) noexcept
{
    Value out;
    ::Matches(_raw, v, &out);
    /// @todo implement size checks
    int64 patternMatches = out.multifieldValue->contents[0].integerValue->contents;
    int64 partialMatches = out.multifieldValue->contents[1].integerValue->contents;
    int64 activations    = out.multifieldValue->contents[2].integerValue->contents; // parasoft-suppress FORMAT-07 "extra spaces for alignment"
    return std::make_tuple(patternMatches, partialMatches, activations);
}
} // end namespace Electron
