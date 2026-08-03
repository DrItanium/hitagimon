/**
 * @file
 * Interface details for Exception
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

#ifndef __Exception_h__ // {
#define __Exception_h__

#include <string>
#include <exception>
#include <typeinfo>
 #include <sstream>

#include "debug/here.h"

namespace Neutron {

/**
 * A base exception using the "here" mechanism.  This way we can capture
 * The file, line and function where the exception was thrown.
 * @ingroup errors
 */
class Exception : public std::exception {
public:
    /**
     * @param __HERE_DECL__ standard "here" declarations see here.h
     */
    template <typename ...Args>
    explicit Exception(__HERE_DECL__, Args &&...args) noexcept :
        __HERE_INIT__
    {
        std::ostringstream os;
        print(os, args...);
        _message = os.str();
    }

    virtual ~Exception() noexcept;

    /**
     * @return a std::string describing the exception.
     */
    std::string message() const        { return _message; }

    /**
     * Some compatibility for std::exception.
     * @note Users should not override this method. It's virtual because
     * it's virtual in std::exception.
     */
    virtual const char* what() const noexcept final;


    /**
     * Exception objects, (and their derivatives), should not be assigned.
     */
    Exception& operator=(const Exception&) = delete;
protected:
    __HERE_MEMBER_DECL__;
private:
    std::string _message;
};

} // namespace Neutron

#endif // __Exception_h__ }

// end of file
