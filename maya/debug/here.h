/**
 * @file
 * Common macros for debugging purposes.
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

#ifndef __here_h__ // {
#define __here_h__
#include <ostream>

/**
 * Convenience macro to automatically pass in file, line and function
 * to various functions
 */
#define __HERE__ __FILE__,  __LINE__, __FUNCTION__

/**
 * Convenience macro for declaring "here" formal parameters in a function.
 */
#define __HERE_DECL__ const char* file, unsigned long line, const char* func

/**
 * Convenience macro for passing "here" arguments from one function to
 * another. 
 */
#define __HERE_ARGS__ file, line, func

/**
 * Convenience macro for declaring "here" arguments as member variables.
 */
#define __HERE_MEMBER_DECL__ const char* _file; unsigned long _line; const char* _func;

/**
 * Convenience macro of passing member variable "here" args.
 */
#define __HERE_MEMBER_ARGS__ _file, _line, _func

/**
 * Convenience macro for initializing "here" member vars in ctor-init.
 */
#define __HERE_INIT__ _file(file), _line(line), _func(func)

namespace Neutron {

/**
 * Print things on the given ostream.
 * This function allows us to print things as a functor which type-safety.
 * @tparam Args The type of the rest of the things to print.
 * This function will do template "recursion" to output each arg one at
 * a time.
 * @param os The std::ostream to write to.
 * @param args the arguments, (if any), to write out.
 */
template <typename ...Args>
void print(std::ostream &os, Args &&...args)
{
    (os << ... << args) << std::endl;
}

/**
 * @example
 * @code
 *    ErrorStatus es;
 *    DateTime    dt;
 *    print(std::cerr, dt, ": cannot open ", filename, " error: ", es);
 * @endcode
 */

 }

#endif // __here_h__ }

// end of file
