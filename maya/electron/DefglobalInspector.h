/**
 * @file
 * C++ wrapper for interacting with defglobals
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

#ifndef LibElectron_DefglobalInspector_h__
#define LibElectron_DefglobalInspector_h__
#include "electron/Environment.h"
namespace Electron
{
class DefglobalInspector
{
public:
    using RawType = Defglobal;
public:
    DefglobalInspector(Environment& env, RawType* defglobal) : _env(env), _raw(defglobal) { }
    ~DefglobalInspector() = default;
    std::string getModule() const          { return std::string(::DefglobalModule(_raw)); }
    std::string getName() const            { return std::string(::DefglobalName(_raw)); }
    std::string getPrettyPrintForm() const { return std::string(::DefglobalPPForm(_raw)); }
    Value getValue() const noexcept {
        Value v;
        ::DefglobalGetValue(_raw, &v);
        return v;
    }

    void setValue(Value* value) noexcept { ::DefglobalSetValue(_raw, value); }
    void setValue(int64 value) noexcept  { ::DefglobalSetInteger(_raw, value); }
    void setValue(double value) noexcept { ::DefglobalSetFloat(_raw, value); }
    // lexeme related
    void setValueSymbol(const std::string& value) noexcept       { ::DefglobalSetSymbol(_raw, value.c_str()); }
    void setValueString(const std::string& value) noexcept       { ::DefglobalSetString(_raw, value.c_str()); }
    void setValueInstanceName(const std::string& value) noexcept { ::DefglobalSetInstanceName(_raw, value.c_str()); }
    // CLIPS structures
    void setValue(Integer* value) noexcept         { ::DefglobalSetCLIPSInteger(_raw, value); }
    void setValue(Float* value) noexcept           { ::DefglobalSetCLIPSFloat(_raw, value); }
    void setValue(Lexeme* value) noexcept          { ::DefglobalSetCLIPSLexeme(_raw, value); }
    void setValue(Fact* value) noexcept            { ::DefglobalSetFact(_raw, value); }
    void setValue(Instance* value) noexcept        { ::DefglobalSetInstance(_raw, value); }
    void setValue(Multifield* value) noexcept      { ::DefglobalSetMultifield(_raw, value); }
    void setValue(ExternalAddress* value) noexcept { ::DefglobalSetCLIPSExternalAddress(_raw, value); }
    // deletion related stuff
    bool isDeletable() const noexcept       { return ::DefglobalIsDeletable(_raw); }
    bool undef() noexcept                   { return ::Undefglobal(_raw, _env.getRawEnvironment()); }
    bool isWatched() const noexcept         { return ::DefglobalGetWatch(_raw); }
    void watch(bool enable = true) noexcept { ::DefglobalSetWatch(_raw, enable); }
private:
    Environment& _env;
    RawType* _raw;

};
} // end namespace Electron
#endif // end __LibElectron_DefglobalInspector_h__
