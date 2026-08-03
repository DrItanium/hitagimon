/**
 * @file
 * Implementation of the extensions to electron which provide CLIPS parsing * capabilities 
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

#include "electron/ParsingExtensions.h"
#include "electron/Environment.h"
#include "electron/MultifieldBuilder.h"
#include "fs/path.h"
#include <string>
extern "C" {
#include "clips/clips.h"
}


namespace Electron
{

static void nextTokenFunction(UDF_ARGS__);

void
InitializeParsingExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    // this function is useful if you want to parse CLIPS syntax as is, if you want to use clips for general parsing
    // then you will need to use get-char and pray
    theEnv.addFunction("next-token", "synldfie", 1, 1, "y", nextTokenFunction, "nextTokenFunction");
}

void
nextTokenFunction(UDF_ARGS__) {
    auto& theEnv = Environment::fromRaw(env);
    auto logicalName = ::GetLogicalName(context, STDIN);
    MultifieldBuilder bldr(theEnv, 2);
    auto readErrorMultifield = [&bldr]() {
        bldr.append("ERROR", TreatLexemeAsSymbol{});
        bldr.append("*** READ ERROR ***", TreatLexemeAsString{});
    };
    auto generateReadError = [&theEnv, readErrorMultifield](){
        theEnv.setHaltExecution(true);
        theEnv.setEvaluationError(true);
        readErrorMultifield();
    };
    if (!logicalName) {
        theEnv.illegalLogicalNameMessage("next-token");
        generateReadError();
    } else if (!theEnv.queryRouters(logicalName)) {
        theEnv.unrecognizedRouterMessage(logicalName);
        generateReadError();
    } else {
        auto theToken = theEnv.getToken(logicalName);
        auto &rdat = theEnv.routerData();
        rdat.AwaitingInput = false;
        rdat.CommandBufferInputCount = 0;
        switch (theToken.tknType) {
            case TokenType::SYMBOL_TOKEN:
                bldr.append("SYMBOL", TreatLexemeAsSymbol{});
                bldr.append(theToken.lexemeValue);
                break;
            case TokenType::STRING_TOKEN:
                bldr.append("STRING", TreatLexemeAsSymbol{});
                bldr.append(theToken.lexemeValue);
                break;
            case TokenType::INSTANCE_NAME_TOKEN:
                bldr.append("INSTANCE_NAME", TreatLexemeAsSymbol{});
                bldr.append(theToken.lexemeValue);
                break;
            case TokenType::FLOAT_TOKEN:
                bldr.append("FLOAT", TreatLexemeAsSymbol{});
                bldr.append(theToken.floatValue);
                break;
            case TokenType::INTEGER_TOKEN:
                bldr.append("INTEGER", TreatLexemeAsSymbol{});
                bldr.append(theToken.integerValue);
                break;
            case TokenType::LEFT_PARENTHESIS_TOKEN:
                bldr.append("LEFT_PARENTHESIS", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::RIGHT_PARENTHESIS_TOKEN:
                bldr.append("RIGHT_PARENTHESIS", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::SF_VARIABLE_TOKEN:
                bldr.append("SF_VARIABLE", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::MF_VARIABLE_TOKEN:
                bldr.append("MF_VARIABLE", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::GBL_VARIABLE_TOKEN:
                bldr.append("GBL_VARIABLE", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::SF_WILDCARD_TOKEN:
                bldr.append("SF_WILDCARD", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::MF_WILDCARD_TOKEN:
                bldr.append("MF_WILDCARD", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::MF_GBL_VARIABLE_TOKEN:
                bldr.append("MF_GBL_WILDCARD", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::NOT_CONSTRAINT_TOKEN:
                bldr.append("NOT_CONSTRAINT", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::AND_CONSTRAINT_TOKEN:
                bldr.append("AND_CONSTRAINT", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::OR_CONSTRAINT_TOKEN:
                bldr.append("OR_CONSTRAINT", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::STOP_TOKEN:
                bldr.append("STOP", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsSymbol{});
                break;
            case TokenType::UNKNOWN_VALUE_TOKEN:
                bldr.append("UNKNOWN_VALUE", TreatLexemeAsSymbol{});
                bldr.append(theToken.printForm, TreatLexemeAsString{});
                break;
            default:
                readErrorMultifield();
                break;
        }
    }
    out->multifieldValue = bldr.create();
}

} // end namespace Electron
