/**
 * @file
 * A C++ singleton which provides clips with access to an include path like in C.
 * This is meant to make the concept of an include easier.
 * @copyright
 * Copyright (c) 2015-2025 Parasoft Corporation
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
#include "electron/MultifieldBuilder.h"
#include "error/Exception.h"
#include <sstream>
extern "C" {
#include "clips/clips.h"
#include "clips/prcdrpsr.h"
#include "clips/pprint.h"
}

namespace Electron
{

void getIncludeList(UDF_ARGS__) noexcept;
void addToIncludeListBack(UDF_ARGS__) noexcept;
void addToIncludeListFront(UDF_ARGS__) noexcept;
bool parseIncludeStatement(RawEnvironment* env, const char* readSource);
void clearIncludeCache(RawEnvironment* env, void* context) noexcept;
bool parseUseModuleDeclStatement(RawEnvironment* env, const char* readSource);
bool parseUseModuleTypeStatement(RawEnvironment* env, const char* readSource);
bool parseUseModuleLogicStatement(RawEnvironment* env, const char* readSource);

void
Environment::installIncludePathFunctions()
{
    // not sure where to store this yet, no need for pretty print it looks like
    _importConstruct = ::AddConstruct(_env, 
            "include", 
            "includes", 
            parseIncludeStatement,
            nullptr, nullptr, nullptr, nullptr,
            nullptr, nullptr, nullptr, nullptr, nullptr);
    addFunctionNoArgs("get-include-path", "b", getIncludeList, "getIncludeList");
    addFunction("add-to-include-path-back", "b", 1, 1, "sy;sy", addToIncludeListBack, "addToIncludeListBack");
    addFunction("add-to-include-path-front", "b", 1, 1, "sy;sy", addToIncludeListFront, "addToIncludeListFront");
    addClearFunction("clear_include_cache", clearIncludeCache, 0, nullptr);
}

void
clearIncludeCache(RawEnvironment* env, void*) noexcept {
    auto& theEnv = Environment::fromRaw(env);
    theEnv.clearIncludedFileSet();
}
void
addToIncludeListBack(UDF_ARGS__) noexcept {
    auto &theEnv = Environment::fromRaw(env);
    UDFValue arg;
    if (!theEnv.firstArgument(context, LEXEME_BITS, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        std::string str(arg.lexemeValue->contents);
        theEnv.addToIncludePathBack(str);
        out->lexemeValue = theEnv.trueSymbol();
    }
}


void
addToIncludeListFront(UDF_ARGS__) noexcept {
    auto &theEnv = Environment::fromRaw(env);
    UDFValue arg;
    if (!theEnv.firstArgument(context, LEXEME_BITS, &arg)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        std::string str(arg.lexemeValue->contents);
        theEnv.addToIncludePathFront(str);
        out->lexemeValue = theEnv.trueSymbol();
    }
}

void
getIncludeList(UDF_ARGS__) noexcept 
{
    auto &theEnv = Environment::fromRaw(env);
    auto container = theEnv.getIncludePathList();
    MultifieldBuilder mb(theEnv);
    for (const auto& line : container) {
        mb.append(line.string(), TreatLexemeAsString { });
    }

    out->multifieldValue = mb.create();
}
bool
onParseSuccess(Environment& theEnv, const char*readSource, const std::string& func) {
    Token tmp;
    theEnv.getToken(readSource, tmp);
    auto result = tmp.tknType != TokenType::RIGHT_PARENTHESIS_TOKEN;
    if (tmp.tknType != TokenType::RIGHT_PARENTHESIS_TOKEN) {
        theEnv.syntaxErrorMessage(func);
    }
    return result;
}

bool
parseIncludeStatement(RawEnvironment* env, const char* readSource)
{
    ::GCBlock frame;
    GCBlockStart(env, &frame);
    auto &theEnv = Environment::fromRaw(env);
    bool outcome = true;
    Token inputToken;
    theEnv.getToken(readSource, inputToken);
    if (inputToken.tknType == TokenType::SYMBOL_TOKEN || inputToken.tknType == TokenType::STRING_TOKEN) {
        Neutron::Path basePath(inputToken.lexemeValue->contents);
        Neutron::Path targetPrefix;
        bool foundPrefix = false;
        for (const auto& prefix : theEnv.getIncludePathList()) {
            auto newPath = prefix / basePath;
            if (Neutron::exists(newPath)) {
                foundPrefix = true;
                targetPrefix = newPath;
                break;
            } 
        }
        if (foundPrefix) {
            if (!theEnv.fileAlreadyIncluded(targetPrefix)) {
                // preserve the old fast ptr result since CLIPS tramples it inside
                // Load. Our include command supports embedding includes as constructs
                auto oldParsingConstruct = theEnv.constructData().ParsingConstruct;
                auto oldParsedBindNames = ::GetParsedBindNames(env);
                auto oldReturnContext = theEnv.expressionData().ReturnContext;
                auto oldBreakContext = theEnv.expressionData().BreakContext;
                auto oldFastPtr = ::GetFastLoad(env);
                auto lip = ::GetLoadInProgress(env);
                auto oldWarningFileName = ::CopyString(env, ::GetWarningFileName(env));
                auto oldErrorFileName = ::CopyString(env, ::GetErrorFileName(env));
                auto oldHaltExecution = ::GetHaltExecution(env);
                ::ClearParsedBindNames(env);
                auto result = ::Load(env, targetPrefix.string().c_str());
                theEnv.setHaltExecution(oldHaltExecution);
                ::SetFastLoad(env, oldFastPtr);
                ::SetLoadInProgress(env, lip);
                ::SetWarningFileName(env, oldWarningFileName);
                ::SetErrorFileName(env, oldErrorFileName);
                ::SetParsedBindNames(env, oldParsedBindNames);
                ::DeleteString(env, oldWarningFileName);
                ::DeleteString(env, oldErrorFileName);
                theEnv.constructData().ParsingConstruct = oldParsingConstruct;
                theEnv.expressionData().ReturnContext = oldReturnContext;
                theEnv.expressionData().BreakContext = oldBreakContext;
                switch (result) {
                    case ::LoadError::LE_PARSING_ERROR:
                        theEnv.syntaxErrorMessage("include");
                        break;
                    case ::LoadError::LE_OPEN_FILE_ERROR:
                        theEnv.openErrorMessage("include", basePath.string());
                        break;
                    case ::LoadError::LE_NO_ERROR:
                        theEnv.addPathToIncludedFileSet(targetPrefix);
                        outcome = onParseSuccess(theEnv, readSource, "include");
                        break;
                    default:
                        outcome = false;
                        break;
                }
            } else {
                // make sure that we still continue execution even though we
                // already saw this file
                outcome = onParseSuccess(theEnv, readSource, "include");
            }
        }  else {
            theEnv.openErrorMessage("include", basePath.string());
        }
    } else {
        theEnv.syntaxErrorMessage("include");
    }
    ::GCBlockEnd(env, &frame);
    ::CallPeriodicTasks(env);
    return outcome;
}

void
Environment::addToIncludePathBack(const Neutron::Path& path)
{
    _path.push_back(path);
}

void
Environment::addToIncludePathFront(const Neutron::Path& path)
{
    _path.push_front(path);
}

void
Environment::addPathToIncludedFileSet(const Neutron::Path& p) {
    _includedFiles.emplace(p);
}
void
Environment::clearIncludedFileSet() noexcept {
    _includedFiles.clear();
}
bool
Environment::fileAlreadyIncluded(const Neutron::Path& p) {
    return _includedFiles.count(p);
}



} // end namespace Electron
