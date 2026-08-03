/**
 * @file
 * IORouter wrapper implementation
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
#include "electron/IORouter.h"
#include "error/Exception.h"
extern "C" {
#include "clips/clips.h"
#include "clips/factfile.h"
}

namespace Electron
{

bool
genericIORouter_QueryFunction(RawEnvironment* env, const char* logicalName, void* context)
{
    auto& theEnv = Environment::fromRaw(env);
    return ((IORouter*)context)->respondsTo(theEnv, logicalName);
}

void
genericIORouter_Write(RawEnvironment* env, const char* logicalName, const char* str, void* context)
{
    auto& theEnv = Environment::fromRaw(env);
    ((IORouter*)context)->write(theEnv, logicalName, str);
}
void
genericIORouter_Exit(RawEnvironment* env, int code, void* context)
{
    auto& theEnv = Environment::fromRaw(env);
    ((IORouter*)context)->exit(theEnv, code);
}

int
genericIORouter_Read(RawEnvironment* env, const char* logicalName, void* context)
{
    auto& theEnv = Environment::fromRaw(env);
    return ((IORouter*)context)->read(theEnv, logicalName);
}

int
genericIORouter_Unread(RawEnvironment* env, const char* logicalName, int ch, void* context)
{
    auto& theEnv = Environment::fromRaw(env);
    return ((IORouter*)context)->unread(theEnv, logicalName, ch);
}

bool
Environment::addRouter(IORouter* router) noexcept
{
    return addRouter(router->getName(),
            router->getPriority(),
            router->hasQuery() ? genericIORouter_QueryFunction : nullptr,
            router->hasWrite() ? genericIORouter_Write : nullptr,
            router->hasExit() ? genericIORouter_Exit : nullptr,
            router->hasRead() ? genericIORouter_Read : nullptr,
            router->hasUnread() ? genericIORouter_Unread : nullptr,
            router);
}

std::unique_ptr<IORouter>
Environment::addRouter(const char* name,
        int priority,
        RouterQueryFunction onQuery,
        RouterWriteFunction onWrite,
        RouterExitFunction onExit,
        RouterReadFunction onRead,
        RouterUnreadFunction onUnread) noexcept
{
    if (IORouter* lambda = new IORouter(name, priority, onQuery, onWrite, onRead, onUnread, onExit); !addRouter(lambda)) {
        delete lambda;
        return std::unique_ptr<IORouter>();
    } else {
        return std::unique_ptr<IORouter>(lambda);
    }
}

// Begin IORouter stuff
IORouter::IORouter(const std::string& name,
        int priority,
        QueryFunction onQuery,
        WriteFunction onWrite,
        ReadFunction onRead,
        UnreadFunction onUnread,
        ExitFunction onExit) noexcept :
    _name(name),
    _priority(priority),
    _onQuery(onQuery),
    _onWrite(onWrite),
    _onRead(onRead),
    _onUnread(onUnread),
    _onExit(onExit)
{
}


bool
IORouter::respondsTo(Environment& env, const char* logicalName) noexcept
{
    return _onQuery != nullptr && _onQuery(env, logicalName);
}

void
IORouter::write(Environment& env, const char* logicalName, const char* str) noexcept
{
    if (_onWrite) {
        _onWrite(env, logicalName, str);
    }
}

void
IORouter::exit(Environment& env, int code) noexcept
{
    if (_onExit) {
        _onExit(env, code);
    }
}

int
IORouter::read(Environment& env, const char* logicalName) noexcept
{
    if (_onRead) {
        return _onRead(env, logicalName);
    } else {
        return 0;
    }
}

int
IORouter::unread(Environment& env, const char* logicalName, int ch) noexcept
{
    if (_onUnread) {
        return _onUnread(env, logicalName, ch);
    } else {
        return 0;
    }
}

// End IORouter stuff

RouterQueryFunction
makeIORouterQueryFunction(const std::string& expected, IORouterEnhancedQueryFunction fn) noexcept
{
    return [fn, expected](Environment& env, const char* value) noexcept
    {
        return fn(env, value, expected);
    };
}

}  // end namespace Electron
