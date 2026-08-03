/**
 * @file
 * Aliases around clips types
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

#ifndef __LibElectron_Aliases_h__
#define __LibElectron_Aliases_h__

extern "C" {
    #include "clips/clips.h"
    #include "clips/miscfun.h"
}


namespace Electron
{
using Defrule = ::Defrule;
using Defclass = ::Defclass;
using Deftemplate = ::Deftemplate;
using PutSlotError = ::PutSlotError;
using GetSlotError = ::GetSlotError;
using Fact = ::Fact;
using Expression = ::Expression;
using ExternalAddress = ::CLIPSExternalAddress;
using Multifield = ::Multifield;
using RawEnvironment = ::Environment;
using Lexeme = ::CLIPSLexeme;
using Integer = ::CLIPSInteger;
using Float = ::CLIPSFloat;
using UDFContext = ::UDFContext;
using Value = ::CLIPSValue;
using Instance = ::Instance;
using ExternalAddressRegistration = ::externalAddressType;
using UDFValue = ::UDFValue;
using Token = ::token;
using TokenType = decltype(Token::tknType);
using Defglobal = ::Defglobal;
using RouterDataModule = ::routerData;
using ExpressionDataModule = ::expressionData;
using ConstructDataModule = ::constructData;
using RawActivation = ::Activation;
using SalienceGroup = ::salienceGroup;
using AgendaDataModule = ::agendaData;
using PartialMatch = ::PartialMatch;
using GenericMatch = ::genericMatch;
using AlphaMatch = ::alphaMatch;
using PatternMatch = ::patternMatch;
} // end namespace Electron

#define UDF_ARGS__ Electron::RawEnvironment* env, Electron::UDFContext* context, Electron::UDFValue* out

#endif // end __LibElectron_Aliases_h__
