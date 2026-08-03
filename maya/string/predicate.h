/**
 * @file
 * Interface for boost string predicate functions
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
 */

#ifndef string_predicate_h__ // {
#define string_predicate_h__

#include <string>
#include <boost/algorithm/string/predicate.hpp>
#include <boost/algorithm/string/classification.hpp>

namespace Neutron {

using boost::algorithm::is_any_of;

inline bool hasPrefix(const std::string& str, const std::string& prefix)
{
    return boost::algorithm::starts_with(str, prefix);
}

inline bool hasSuffix(const std::string& str, const std::string& suffix)
{
    return boost::algorithm::ends_with(str, suffix);
}

} // namespace Neutron

#endif // } __string_predicate_h__

// end of file

