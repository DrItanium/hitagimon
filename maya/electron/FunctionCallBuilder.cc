/**
 * @file
 * FunctionCallBuilder class implementation.
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
#include "error/Exception.h"
extern "C" {
#include "clips/clips.h"
}

namespace Electron
{
// Begin FunctionBuilder stuff
FunctionBuilder::FunctionBuilder(Environment& e, size_t capacity) : _env(e), _bldr(::CreateFunctionCallBuilder(e.getRawEnvironment(), capacity)) { }

FunctionBuilder::~FunctionBuilder()
{
    ::FCBDispose(_bldr);
}

void
FunctionBuilder::add(bool value)
{
    add(_env.createBool(value));
}


void
FunctionBuilder::call(const std::string& name, Value* ret)
{
    switch (::FCBCall(_bldr, name.c_str(), ret)) {
        case ::FCBE_NULL_POINTER_ERROR:
            throw Neutron::Exception(__HERE__, "Function builder or function name is null!");
        case ::FCBE_FUNCTION_NOT_FOUND_ERROR:
            throw Neutron::Exception(__HERE__, "Function ", name, " was not found!");
        case ::FCBE_INVALID_FUNCTION_ERROR:
            throw Neutron::Exception(__HERE__, "Function ", name, " has a specialized parser and cannot be invoked in this manner");
        case ::FCBE_ARGUMENT_COUNT_ERROR:
            throw Neutron::Exception(__HERE__, "Incorrect number of function arguments passed to ", name);
        case ::FCBE_ARGUMENT_TYPE_ERROR:
            throw Neutron::Exception(__HERE__, "Found an argument with an incorrect type passed to ", name);
        case ::FCBE_PROCESSING_ERROR:
            throw Neutron::Exception(__HERE__, "Error during evaluation of function ", name);
        case FCBE_NO_ERROR:
        default:
            break;
    }
}

void
FunctionBuilder::add(std::function<void(FunctionBuilder*)> fn)
{
    fn(this);
}

void
FunctionBuilder::reset()
{
    ::FCBReset(_bldr);
}

void
FunctionBuilder::add(Value* value)
{
    ::FCBAppend(_bldr, value);
}
void
FunctionBuilder::add(UDFValue* value)
{
    ::FCBAppendUDFValue(_bldr, value);
}
void
FunctionBuilder::add(const char* str)
{
    ::FCBAppendString(_bldr, str);
}
void
FunctionBuilder::add(int64 value)
{
    ::FCBAppendInteger(_bldr, value);
}
void
FunctionBuilder::add(uint64 value)
{
    add(static_cast<int64>(value));
}
void
FunctionBuilder::add(int32 value)
{
    add(static_cast<int64>(value));
}
void
FunctionBuilder::add(uint32 value)
{
    add(static_cast<int64>(value));
}
void
FunctionBuilder::add(double value)
{
    ::FCBAppendFloat(_bldr, value);
}
void
FunctionBuilder::add(float value)
{
    add(static_cast<double>(value));
}

void
FunctionBuilder::add(Lexeme* value)
{
    ::FCBAppendCLIPSLexeme(_bldr, value);
}
void
FunctionBuilder::add(Integer* value)
{
    ::FCBAppendCLIPSInteger(_bldr, value);
}
void
FunctionBuilder::add(::Fact* value)
{
    ::FCBAppendFact(_bldr, value);
}
void
FunctionBuilder::add(Multifield* value)
{
    ::FCBAppendMultifield(_bldr, value);
}
void
FunctionBuilder::add(ExternalAddress* value)
{
    ::FCBAppendCLIPSExternalAddress(_bldr, value);
}
void
FunctionBuilder::add(Instance* value)
{
    ::FCBAppendInstance(_bldr, value);
}
void
FunctionBuilder::add(const char* str, TreatLexemeAsSymbol)
{
    ::FCBAppendSymbol(_bldr, str);
}
void
FunctionBuilder::add(const std::string& str, TreatLexemeAsSymbol)
{
    add(str.c_str(), TreatLexemeAsSymbol{});
}
void
FunctionBuilder::add(const Neutron::Path& path, TreatLexemeAsSymbol)
{
    add(path.string(), TreatLexemeAsSymbol{});
}

void
FunctionBuilder::add(const char* str, TreatLexemeAsInstanceName)
{
    ::FCBAppendInstanceName(_bldr, str);
}
void
FunctionBuilder::add(const std::string& str, TreatLexemeAsInstanceName)
{
    add(str.c_str(), TreatLexemeAsInstanceName{});
}
void
FunctionBuilder::add(const Neutron::Path& path, TreatLexemeAsInstanceName)
{
    add(path.string(), TreatLexemeAsInstanceName{});
}

void
FunctionBuilder::add(const char* str, TreatLexemeAsString)
{
    ::FCBAppendString(_bldr, str);
}
void
FunctionBuilder::add(const std::string& str, TreatLexemeAsString)
{
    add(str.c_str(), TreatLexemeAsString{});
}
void
FunctionBuilder::add(const Neutron::Path& path, TreatLexemeAsString)
{
    add(path.string(), TreatLexemeAsString{});
}
} // end namespace Electron
