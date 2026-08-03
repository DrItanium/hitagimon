/**
 * @file
 * Extended functionality around multifields
 * @copyright
 * Copyright (c) 2013-2025 Joshua Scoggins
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

#include "electron/Environment.h"
#include "electron/MultifieldExtensions.h"
extern "C" {
#include "clips/clips.h"
}
namespace Electron
{

static void multifieldEmpty(UDF_ARGS__);

void
InitializeMultifieldExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("empty$", "b", 1, 1, "m;m", multifieldEmpty, "multifieldEmpty");
}

void
multifieldEmpty(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (!theEnv.firstArgument(context, ArgumentBits::Multifield, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        out->lexemeValue = theEnv.createBool(arg0.multifieldValue->length == 0);
    }
}
} // end namespace Electron
