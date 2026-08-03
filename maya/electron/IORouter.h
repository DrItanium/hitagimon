/**
 * @file * C++ wrapper functions for building electron environments
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

#ifndef __LibElectron_IORouter_h__
#define __LibElectron_IORouter_h__

#include "electron/Environment.h"

namespace Electron
{


bool respondsToLogicalName(Environment& env, const char* logicalName, const std::string& expected) noexcept;
using IORouterEnhancedQueryFunction = EnvironmentFunction<bool, const char*, const std::string&>;
/**
 * Perform function currying to compare the expected string against provided
 * inputs.
 * @param fn the function to perform currying against
 * @param expected the value to bind to this new function
 * @return a curried version of this comparison function
 */
RouterQueryFunction makeIORouterQueryFunction(const std::string& expected, IORouterEnhancedQueryFunction fn = respondsToLogicalName) noexcept;

/**
 * Wrapper around the concept of an IO router
 */
class IORouter
{
public:
    using QueryFunction = RouterQueryFunction;
    using ReadFunction = RouterReadFunction;
    using WriteFunction = RouterWriteFunction;
    using ExitFunction = RouterExitFunction;
    using UnreadFunction = RouterUnreadFunction;
public:
    /**
     * Construct a fully specified IORouter
     * @param name the logical name of the io router
     * @param priority the type of io router and how it should be handled according to the advanced programming guide.
     * @param onQuery the function that should be invoked when the router is queried
     * @param onPrint the function that actually performs the print operation itself
     * @param onGetc the function that retrieves a character when read operations are called
     * @param onUngetc The function that puts a character back into the input stream
     * @param onExit the function that performs an action when the router is shutdown
     */
    IORouter(const std::string& name, int priority, QueryFunction onQuery = nullptr, WriteFunction onWrite = nullptr, ReadFunction onRead = nullptr, UnreadFunction onUnread = nullptr, ExitFunction onExit = nullptr) noexcept;
    virtual ~IORouter() = default;
    bool respondsTo(Environment& env, const char* logicalName) noexcept;
    void write(Environment& env, const char* logicalName, const char* str) noexcept;
    void exit(Environment& env, int code) noexcept;
    int read(Environment& env, const char* logicalName) noexcept;
    int unread(Environment& env, const char* logicalName, int ch) noexcept;
    const char* getName() const noexcept                    { return _name.c_str(); }
    std::string getNameString() const noexcept              { return _name; }
    int getPriority() const noexcept                        { return _priority; }
    void setQueryFunction(QueryFunction fn) noexcept        { _onQuery = fn; }
    void setReadFunction(ReadFunction fn) noexcept          { _onRead = fn; }
    void setWriteFunction(WriteFunction fn) noexcept        { _onWrite = fn; }
    void setExitFunction(ExitFunction fn) noexcept          { _onExit = fn; }
    void setUnreadFunction(UnreadFunction fn) noexcept      { _onUnread = fn; }
    bool hasQuery() const noexcept                          { return _onQuery != nullptr; }
    bool hasWrite() const noexcept                          { return _onWrite != nullptr; }
    bool hasRead() const noexcept                           { return _onRead != nullptr;  }
    bool hasUnread() const noexcept                         { return _onUnread != nullptr; }
    bool hasExit() const noexcept                           { return _onExit != nullptr; }
private:
    std::string _name;
    int _priority;
    QueryFunction _onQuery;
    WriteFunction _onWrite;
    ReadFunction _onRead;
    UnreadFunction _onUnread;
    ExitFunction _onExit;
};


} // end namespace Electron

#endif  // end __LibElectron_IORouter_h__
