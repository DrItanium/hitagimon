/**
 * @file
 * Expose std::experimental::observer_ptr
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
#ifndef Neutron_Features_Observer_Ptr_h__
#define Neutron_Features_Observer_Ptr_h__
#include <experimental/memory>

namespace Neutron {
    /**
     * Non-owning smart pointer
     * @tparam T the type of the pointer to own
     * @see http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2013/n3514.pdf
     */
    template<typename T>
    using observer_ptr = std::experimental::observer_ptr<T>;
#define X(name) \
    template<typename T> \
    using name ## _ptr = observer_ptr<T>
    X(aloof);
    X(blameless);
    X(disinterested);
    X(estranged);
    X(faultless);
    X(oblivious);
    X(non_owning);
    X(unowned);
    X(witless);
#undef X

} // end namespace Neutron
#endif // end Neutron_Features_Dependent_False_h__
