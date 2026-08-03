/**
 * @file
 * Extensions to electron which provide clips parsing functionality
 * @copyright
 * Copyright (c) 2023 Joshua Scoggins
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

#ifndef __LibElectron_ParsingExtensions_h__
#define __LibElectron_ParsingExtensions_h__
#include "electron/Environment.h"
namespace Electron
{
/**
* @brief Provides functions for parsing clips code from within clips
* @param env The environment to install the functions into
*/
void InitializeParsingExtensions(RawEnvironment* env);
} // end namespace Electron


#endif // end __LibElectron_ParsingExtensions_h__
