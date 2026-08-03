/**
 * @file
 * Compatibility header to temporarily disable LHS and RHS from clips
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

/**
 * boost uses LHS and RHS in their parsing libraries, CLIPS also defines it as macros. 
 * So we have to disable the macro defines.
 */
#ifdef LHS
#define PREVIOUS_LHS LHS
#undef LHS
#endif // end LHS

#ifdef RHS
#define PREVIOUS_RHS RHS
#undef RHS
#endif // end RHS
