/**
 * @file
 * Environment class implementations.
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
#include "electron/StringExtensions.h"
#include "electron/FileSystemExtensions.h"
#include "electron/ParsingExtensions.h"
#include "electron/MultifieldExtensions.h"

extern "C" {
#include "clips/clips.h"
#include "clips/factfile.h"
}

namespace Electron
{



Environment::Environment() : _env(::CreateEnvironment())
{
    if (!_env) {
        throw AllocationError(__HERE__, "Could not create a CLIPS environment");
    }
    registerEnv();

    installIncludePathFunctions();
    InitializeStringExtensions(_env);
    InitializeFilesystemExtensions(_env);
    InitializeParsingExtensions(_env);
    InitializeMultifieldExtensions(_env);
}

Environment::~Environment()
{
    this->destroy();
}

void
Environment::applyToFunction(ExpansionFunction fn)
{
    fn(_env);
}

void
Environment::reset() noexcept
{
    ::Reset(_env);
}

int64
Environment::run(int64 count) noexcept
{
    return ::Run(_env, count);
}


void
Environment::loadFile(const Neutron::Path& path)
{
    switch (::Load(_env, path.string().c_str())) {
        case ::LoadError::LE_OPEN_FILE_ERROR:
            throw Neutron::Exception(__HERE__, "Could not open: ", path.string().c_str());
        case ::LoadError::LE_PARSING_ERROR:
            throw Neutron::Exception(__HERE__, "Error during parsing of file ", path.string().c_str());
        case ::LoadError::LE_NO_ERROR:
        default:
            break;
    }
}

void
Environment::watch(const std::string& value)
{
    if (!::WatchString(_env, value.c_str())) {
        throw Neutron::Exception(__HERE__, "Attempting to watch '", value, "' was not successful!", value.c_str());
    }
}

void
Environment::unwatch(const std::string& value)
{
    if (!::UnwatchString(_env, value.c_str())) {
        throw Neutron::Exception(__HERE__, "Attempting to unwatch '", value, "' was not successful!", value.c_str());
    }
}

void
Environment::extractValue(Value* dobj, EnvironmentFunction<void, Value*> fn)
{
    fn(*this, dobj);
}



void
Environment::call(const std::string& function)
{
    Value dontCare;
    call(function, &dontCare);
}

void
Environment::call(const std::string& function, Value* ret)
{
    FunctionBuilder fb(*this);
    fb.call(function, ret);
}



bool
Environment::addRouter(const char* name, int priority, RawIORouterQueryFunction onQuery, RawIORouterWriteFunction onWrite, RawIORouterExitFunction onExit, RawIORouterReadFunction onRead, RawIORouterUnreadFunction onUnread, void* context) noexcept
{
    return ::AddRouter(_env, name, priority, onQuery, onWrite, onRead, onUnread, onExit, context);
}




bool
Environment::activateRouter(const std::string& name) noexcept
{
    return ::ActivateRouter(_env, name.c_str());
}

bool
Environment::deactivateRouter(const std::string& name) noexcept
{
    return ::DeactivateRouter(_env, name.c_str());
}

// End Environment stuff

// Begin RouterDeactivator stuff
Environment::RouterDeactivator::RouterDeactivator(Environment& parent, const std::string& name) : _parent(parent), _name(name)
{
    _parent.deactivateRouter(_name);
}

Environment::RouterDeactivator::~RouterDeactivator()
{
    _parent.activateRouter(_name);
}
// End RouterDeactivator stuff




EnvironmentFunction<void, Value*>
populateStreamWithStrings(std::ostream& container)
{
    return [&container](Environment& env, Value* obj) {
        if (!isMultifield(obj)) {
            throw Neutron::Exception(__HERE__, "provided value is not a multifield");
        }
        for (size_t ind = 0; ind < obj->multifieldValue->length; ++ind) {
            auto& curr = obj->multifieldValue->contents[ind];
            if (!usesLexemeField(curr)) {
                throw Neutron::Exception(__HERE__, "Multifield has an entry which does not use the lexeme field");
            }
            std::string tmp(curr.lexemeValue->contents);
            container << tmp << '\n';
        }
    };
}


void
printExternalAddress(RawEnvironment* env, const std::string& logicalName, void* theValue, const std::string& func) noexcept
{
    /// @todo fix this function to cut down on the number of unnecessary string build actions
    std::stringstream ss;
    ExternalAddress* ea = (ExternalAddress*)theValue;
    auto ptr = ea->contents;
    ss << "<Pointer-" << func << "-" << std::hex << ((ptr) ? ptr : theValue) << ">";
    auto str = ss.str();
    ::WriteString(env, logicalName.c_str(), str.c_str());
}



template<>
std::list<std::string>
extractData<std::list<std::string>>(Environment& env, Value* obj)
{
    return populateStringContainer<std::list<std::string>>(env, obj);
}

template<>
std::vector<Neutron::Path>
extractData<std::vector<Neutron::Path>>(Environment& env, Value* dobj)
{
    return populateStringContainer<std::vector<Neutron::Path>>(env, dobj);
}


template<>
std::list<Neutron::Path>
extractData<std::list<Neutron::Path>>(Environment& env, Value* dobj)
{
    return populateStringContainer<std::list<Neutron::Path>>(env, dobj);
}

template<>
std::vector<std::string>
extractData<std::vector<std::string>>(Environment& env, Value* dobj)
{
    return populateStringContainer<std::vector<std::string>>(env, dobj);
}



template<>
std::string
extractData<std::string>(Environment& env, Value* dobj)
{
    return {dobj->lexemeValue->contents};
}

template<>
Neutron::Path
extractData(Environment& env, Value* dobj)
{
    return {dobj->lexemeValue->contents};
}

template<>
bool
Environment::extractValue<bool>(Value* data)
{
    return (data->header->type == SYMBOL_TYPE) && (data->lexemeValue != falseSymbol());
}

void
Environment::compileToBinaryFile(const Neutron::Path &loc) noexcept
{
    ::SetDynamicConstraintChecking(_env, true);
    ::Bsave(_env, loc.string().c_str());
    ::SetDynamicConstraintChecking(_env, false);
}
void
Environment::loadBinaryImage(const Neutron::Path& location) noexcept
{
    ::Bload(_env, location.string().c_str());
}

Environment::Registration *Environment::_envTracker = nullptr;

Environment&
Environment::Registration::fromEnv(RawEnvironment* rawEnv)
{
    if (auto result = _map.find(rawEnv); result == _map.end()) {
        throw Neutron::Exception(__HERE__, "Given environment with address %x is not registered!", rawEnv);
    } else {
        return *result->second;
    }
}

void
Environment::Registration::add(Environment& env)
{
    // if we get a double register then oh nelly we've got a real problem!
    if (auto raw = env.getRawEnvironment(); _map.count(raw) > 0) {
        throw Neutron::Exception(__HERE__, "Given environment with address %x is already registered!", &env);
    } else {
        _map.emplace(raw, &env);
    }
}

void
Environment::Registration::remove(Environment& env)
{
    if (auto result = _map.find(env.getRawEnvironment()); result == _map.end()) {
        throw Neutron::Exception(__HERE__, "Have an environment with address %x that has not been registered with the environment!", &env);
    } else {
        _map.erase(result);
    }
}

bool
Environment::addFunction(const std::string& name, const std::string& returnTypes, uint16 minArgs, uint16 maxArgs, const std::string& argTypes, ::UserDefinedFunction cfp, const std::string& cName, void* context)
{
    // do not read the contents of argTypes.c_str() if min and max args are the same and minArgs == 0
    // It could be nullptr, so don't cause a crash at runtime
    auto noArguments = (minArgs == maxArgs) && (minArgs == 0);
    return ::AddUDF(_env, name.c_str(), returnTypes.c_str(), minArgs, maxArgs, noArguments ? nullptr : argTypes.c_str(), cfp, cName.c_str(), context);
}

bool
Environment::addFunctionNoArgs(const std::string& name, const std::string& returnTypes, ::UserDefinedFunction cfp, const std::string& cName, void* context)
{
    return addFunction(name, returnTypes, 0, 0, "", cfp, cName, context);
}

UnmakeInstanceError
Environment::unmakeInstance(Instance* i) noexcept
{
    return ::UnmakeInstance(i);
}

void
Environment::getToken(const char* name, Token* tok)
{
    ::GetToken(_env, name, tok);
}

void
Environment::getToken(const char* name, Token& tok)
{
    getToken(name, &tok);
}

void
Environment::syntaxErrorMessage(const std::string& name)
{
    ::SyntaxErrorMessage(_env, name.c_str());
}

void
Environment::openErrorMessage(const std::string& name, const std::string& message)
{
    ::OpenErrorMessage(_env, name.c_str(), message.c_str());
}

void
Environment::cantFindItemErrorMessage(const std::string& name, const std::string& message, bool useQuotes) {
    ::CantFindItemErrorMessage(_env, name.c_str(), message.c_str(), useQuotes);
}

Instance*
Environment::instanceNameToAddress(Lexeme* lexeme) noexcept
{
    return ::FindInstanceBySymbol(_env, lexeme);
}
void
Environment::write(const char* logicalName, const char* str) noexcept
{
    ::WriteString(_env, logicalName, str);
}
void
Environment::write(const char* logicalName, const std::string& str) noexcept
{
    write(logicalName, str.c_str());
}

bool
respondsToLogicalName(Environment& env, const char* logicalName, const std::string& expected) noexcept
{
    std::string tmp(logicalName);
    return tmp == expected;
}


bool
Environment::addRouter(const std::string& name,
        int priority,
        RawIORouterQueryFunction onQuery,
        RawIORouterWriteFunction onWrite,
        RawIORouterExitFunction onExit,
        RawIORouterReadFunction onRead,
        RawIORouterUnreadFunction onUnread,
        void* context) noexcept
{
    return addRouter(name.c_str(), priority, onQuery, onWrite, onExit, onRead, onUnread, context);
}
auto
Environment::readRouter(const char* logicalName) noexcept
{
    return ::ReadRouter(_env, logicalName);
}
auto
Environment::unreadRouter(const char* logicalName, int ch) noexcept
{
    return ::UnreadRouter(_env, logicalName, ch);
}
auto
Environment::readRouter(const std::string& logicalName) noexcept
{
    return readRouter(logicalName.c_str());
}

auto
Environment::unreadRouter(const std::string& logicalName, int ch) noexcept
{
    return unreadRouter(logicalName.c_str(), ch);
}
bool
Environment::hasNextArgument(UDFContext* context) noexcept
{
    return UDFHasNextArgument(context);
}
void
Environment::setErrorValue(::TypeHeader* theValue) noexcept
{
    ::SetErrorValue(_env, theValue);
}
uint32
Environment::getArgumentCount(UDFContext* context) noexcept
{
    return ::UDFArgumentCount(context);
}
bool
Environment::addClearFunction(const char* name, RawVoidCallFunction f, int priority, void* context) noexcept
{
    return ::AddClearFunction(_env, name, f, priority, context);
}
bool
Environment::addClearFunction(const std::string& name, RawVoidCallFunction f, int priority, void* context) noexcept
{
    return addClearFunction(name.c_str(), f, priority, context);
}
bool
Environment::removeClearFunction(const char* name) noexcept
{
    return ::RemoveClearFunction(_env, name);
}

bool
Environment::removeClearFunction(const std::string& name) noexcept
{
    return removeClearFunction(name.c_str());
}

Fact*
Environment::assertString(const std::string& value) noexcept
{
    return ::AssertString(_env, value.c_str());
}

Fact*
Environment::assertStringOrThrow(const std::string& value)
{
    if (auto f = assertString(value); f) {
        return f;
    } else {
        throw Neutron::Exception(__HERE__, "Unable to assert fact \"", value, "\"");
    }
}

template<>
std::set<std::string>
extractData<std::set<std::string>>(Environment& env, Value* obj)
{
    return populateStringContainer<std::set<std::string>>(env, obj);
}

template<>
std::set<Neutron::Path>
extractData<std::set<Neutron::Path>>(Environment& env, Value* dobj)
{
    return populateStringContainer<std::set<Neutron::Path>>(env, dobj);
}

Deftemplate*
Environment::findDeftemplate(const std::string& name) const noexcept
{
    return ::FindDeftemplate(_env, name.c_str());
}

Defrule*
Environment::findDefrule(const std::string& name) const noexcept
{
    return ::FindDefrule(_env, name.c_str());
}

void
Environment::deregisterEnv()
{
    createRegistration();
    _envTracker->remove(*this);
    // execute deregistration action
    for (auto& deregisterAType: _externalAddressDeregistrationList) {
        deregisterAType();
    }
}
void
Environment::destroy()
{
    // if _env is already nullptr then don't worry and just return
    if (_env) {
        deregisterEnv();
        if (!::DestroyEnvironment(_env)) {
            // BREAK THE WORLD!
            throw DeallocationError(__HERE__, "Could not destroy the given CLIPS environment!"); // parasoft-suppress EXCEPT-03-1 BD-PB-EXCEPT "This is a terminate execution condition because something REALLY bad happened!"
        }
        _env = nullptr;
    }
}

void
Environment::build(const std::string& construct)
{
    switch (::Build(_env, construct.c_str())) {
        case BuildError::BE_COULD_NOT_BUILD_ERROR:
            throw Neutron::Exception(__HERE__, "Could not build the given construct either due to a join operation in progress or a string source could not be opened!");
        case BuildError::BE_CONSTRUCT_NOT_FOUND_ERROR:
            throw Neutron::Exception(__HERE__, "Given construct not found!");
        case BuildError::BE_PARSING_ERROR:
            throw Neutron::Exception(__HERE__, "Error during parsing of construct");
        case BuildError::BE_NO_ERROR:
        default:
            return;
    }
}

bool
Environment::batchFile(const Neutron::Path& path, bool silent)
{
    if (silent)  {
        return ::BatchStar(_env, path.string().c_str());
    } else {
        return ::Batch(_env, path.string().c_str());
    }
}

} // end namespace Electron


