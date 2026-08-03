/**
 * @file
 * Implementation of the extensions to electron which provide extra string related abilities
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

#include "electron/Environment.h"
#include "electron/StringExtensions.h"
#include "electron/MultifieldBuilder.h"

#include "fs/path.h"
#include "string/replace.h"
#include "string/predicate.h"
#include "regex/regex.h"
#include <string>

extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
static void hasPrefix(UDF_ARGS__);
static void hasSuffix(UDF_ARGS__);
static void splitOnce(UDF_ARGS__);
static void replaceAll(UDF_ARGS__);
static void replaceRegexAll(UDF_ARGS__);

void
InitializeStringExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("has-prefix", "b", 2, 2, "sy;sy;sy", hasPrefix, "hasPrefix");
    theEnv.addFunction("has-suffix", "b", 2, 2, "sy;sy;sy", hasSuffix, "hasSuffix");
    theEnv.addFunction("split-once", "mb", 2, 2, "sy;sy;sy", splitOnce, "splitOnce");
    theEnv.addFunction("replace-all", "sy", 3, 3, "sy;sy;sy;sy", replaceAll, "replaceAll");
    theEnv.addFunction("replace-regex-all", "sy", 3, 3, "sy;sy;sy;sy", replaceRegexAll, "replaceRegexAll");
}

void
splitOnce(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }

    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string split(arg1.lexemeValue->contents);
    MultifieldBuilder mfb(theEnv);
    if (auto result = str.find(split); result == std::string::npos) {
        mfb.append(theEnv.createString(str));
    } else {
        mfb.append(str.substr(0, result), TreatLexemeAsString {});
        mfb.append(str.substr(result + split.length()), TreatLexemeAsString{});
    }
    out->multifieldValue = mfb.create();
}

void
hasPrefix(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string prefix(arg1.lexemeValue->contents);
    out->lexemeValue = theEnv.createBool(Neutron::hasPrefix(str, prefix));
}

void
hasSuffix(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string prefix(arg1.lexemeValue->contents);
    out->lexemeValue = theEnv.createBool(Neutron::hasSuffix(str, prefix));
}

void
replaceAll(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    UDFValue arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string find(arg1.lexemeValue->contents);
    std::string replacement(arg2.lexemeValue->contents);
    auto result = Neutron::replace_all_copy(str, find, replacement);
    out->lexemeValue = theEnv.createString(result);
}

void
replaceRegexAll(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    UDFValue arg1;
    UDFValue arg2;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg1)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &arg2)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    }
    std::string str(arg0.lexemeValue->contents);
    std::string find(arg1.lexemeValue->contents);
    std::string replacement(arg2.lexemeValue->contents);
    auto result = Neutron::replace_all_regex_copy(str, Neutron::regex(find), replacement);
    out->lexemeValue = theEnv.createString(result);
}

} // end namespace Electron
