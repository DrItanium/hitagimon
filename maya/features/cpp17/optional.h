/**
 * @file
 * Provide support for std::optional regardless of standard version
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
#ifndef Neutron_Features_Cpp17_Optional_h__
#define Neutron_Features_Cpp17_Optional_h__
#if __cplusplus >= 201703L
#include <optional>
#else 
#include <experimental/optional>
#define __cpp14_optional
#endif 

namespace Neutron {
#if defined(__cpp14_optional) 
    template<typename T>
    using optional = std::experimental::optional<T>;
    using std::experimental::nullopt;
    #undef __cpp14_optional
#else
    template<typename T>
    using optional = std::optional<T>;
    using std::nullopt;
#endif 
} // end namespace Neutron
#endif // end Neutron_Features_Cpp17_Optional_h__
