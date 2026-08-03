/**
 * @file
 * More type trait analysis functionality
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
#ifndef Neutron_Features_Extended_Type_Traits_h__
#define Neutron_Features_Extended_Type_Traits_h__
#include <type_traits>
#include <memory>
#include "features/observer_ptr.h"

namespace Neutron {
    /**
     * Determine at compile time if we are looking at one of the wrapped pointer
     * classes like shared_ptr, unique_ptr, non_owning_ptr, etc
     * @tparam T The type to inspect
     */
    template<typename T>
    struct is_wrapped_ptr final : std::false_type {
            is_wrapped_ptr() = delete;
            ~is_wrapped_ptr() = delete;
            is_wrapped_ptr(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr& operator=(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr(const is_wrapped_ptr&) = delete;
            is_wrapped_ptr& operator=(const is_wrapped_ptr&) = delete;
    };
    template<typename T>
    struct is_wrapped_ptr<std::shared_ptr<T>> final : std::true_type {
            is_wrapped_ptr() = delete;
            ~is_wrapped_ptr() = delete;
            is_wrapped_ptr(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr& operator=(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr(const is_wrapped_ptr&) = delete;
            is_wrapped_ptr& operator=(const is_wrapped_ptr&) = delete;
    };
    template<typename T>
    struct is_wrapped_ptr<std::unique_ptr<T>> final : std::true_type {
            is_wrapped_ptr() = delete;
            ~is_wrapped_ptr() = delete;
            is_wrapped_ptr(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr& operator=(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr(const is_wrapped_ptr&) = delete;
            is_wrapped_ptr& operator=(const is_wrapped_ptr&) = delete;
    };
    template<typename T>
    struct is_wrapped_ptr<Neutron::non_owning_ptr<T>> final : std::true_type {
            is_wrapped_ptr() = delete;
            ~is_wrapped_ptr() = delete;
            is_wrapped_ptr(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr& operator=(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr(const is_wrapped_ptr&) = delete;
            is_wrapped_ptr& operator=(const is_wrapped_ptr&) = delete;
    };
    template<typename T>
    struct is_wrapped_ptr<std::weak_ptr<T>> final : std::true_type {
            is_wrapped_ptr() = delete;
            ~is_wrapped_ptr() = delete;
            is_wrapped_ptr(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr& operator=(is_wrapped_ptr&&) = delete;
            is_wrapped_ptr(const is_wrapped_ptr&) = delete;
            is_wrapped_ptr& operator=(const is_wrapped_ptr&) = delete;
    };
    template<typename T>
    inline constexpr bool is_wrapped_ptr_v = is_wrapped_ptr<T>::value;

} // end namespace Neutron

#endif // end Neutron_Features_Extended_Type_Traits_h__
