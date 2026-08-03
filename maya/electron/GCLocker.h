/**
 * @file
 * Wrapper around CLIPS gc locking functions
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

#ifndef __LibElectron_GCLocker_h__
#define __LibElectron_GCLocker_h__

#include "electron/Environment.h"

namespace Electron 
{
/**
 * Marks the given primitive CLIPS value to be retained for the lifetime of this object.
 * Each primitive to be retained must have a separate instance.
 *
 * This object cannot be allocated in the heap!
 */
class GCLocker final {
private:
    enum class Kind
    {
        Header,
        Value,
        UDFValue,
        Fact,
        Instance,
        Multifield,
        Lexeme,
        Float,
        Integer,
    };
    explicit GCLocker(Environment& env, void* value, Kind kind) : _env(env), _generic(value), _kind(kind)
    {
        auto raw = _env.getRawEnvironment();
        switch(kind) {
            case Kind::Header:
                ::Retain(raw, _header);
                break;
            case Kind::Value:
                ::RetainCV(raw, _value);
                break;
            case Kind::UDFValue:
                ::RetainUDFV(raw, _udfValue);
                break;
            case Kind::Fact:
                ::RetainFact(_fact);
                break;
            case Kind::Float:
                ::RetainFloat(raw, _float);
                break;
            case Kind::Integer:
                ::RetainInteger(raw, _integer);
                break;
            case Kind::Lexeme:
                ::RetainLexeme(raw, _lexeme);
                break;
            case Kind::Instance:
                ::RetainInstance(_instance);
                break;
            case Kind::Multifield:
                ::RetainMultifield(raw, _multifield);
                break;
            default:
                throw Neutron::Exception(__HERE__, "Illegal kind to lock!");
        }
    }
public:
    /**
     * Constructs a new GCLocker object which will immediately increment the gc lock count
     * @param env The environment to disable the GC on for the lifetime of this object
     */
    GCLocker(Environment& env, ::TypeHeader *value) noexcept : GCLocker(env, value, Kind::Header) { }
    GCLocker(Environment& env, Value *value) noexcept : GCLocker(env, value, Kind::Value) { }
    GCLocker(Environment& env, UDFValue *value) noexcept : GCLocker(env, value, Kind::UDFValue) { }
    GCLocker(Environment& env, Fact* value) noexcept : GCLocker(env, value, Kind::Fact) { }
    GCLocker(Environment& env, Instance* value) noexcept : GCLocker(env, value, Kind::Instance) { }
    GCLocker(Environment& env, Multifield* value) noexcept : GCLocker(env, value, Kind::Multifield) { }
    GCLocker(Environment& env, Lexeme* value) noexcept : GCLocker(env, value, Kind::Lexeme) { }
    GCLocker(Environment& env, Float* value) noexcept : GCLocker(env, value, Kind::Float) { }
    GCLocker(Environment& env, Integer* value) noexcept : GCLocker(env, value, Kind::Integer) { }
    /**
     * Decrements the gc lock count of the target environment
     */
    ~GCLocker() noexcept {
        auto raw = _env.getRawEnvironment();
        switch(_kind) {
            case Kind::Header:
                ::Release(raw, _header);
                break;
            case Kind::Value:
                ::ReleaseCV(raw, _value);
                break;
            case Kind::UDFValue:
                ::ReleaseUDFV(raw, _udfValue);
                break;
            case Kind::Fact:
                ::ReleaseFact(_fact);
                break;
            case Kind::Float:
                ::ReleaseFloat(raw, _float);
                break;
            case Kind::Integer:
                ::ReleaseInteger(raw, _integer);
                break;
            case Kind::Lexeme:
                ::ReleaseLexeme(raw, _lexeme);
                break;
            case Kind::Instance:
                ::ReleaseInstance(_instance);
                break;
            case Kind::Multifield:
                ::ReleaseMultifield(raw, _multifield);
                break;
            default:
                throw Neutron::Exception(__HERE__, "Illegal kind to unlock!");
        }
    }
    GCLocker(const GCLocker&) = delete;
    GCLocker(GCLocker&&) = delete;
    GCLocker& operator=(const GCLocker&) = delete;
    GCLocker& operator=(GCLocker&&) = delete;
    void* operator new(size_t) = delete;
    void* operator new[](size_t) = delete;
    void operator delete(void*) = delete;
    void operator delete[](void*) = delete;
private:
    Environment& _env;
    union {
        void*         _generic;
        ::TypeHeader* _header;
        Value*        _value;
        UDFValue*     _udfValue;
        Fact*         _fact;
        Instance*     _instance;
        Multifield*   _multifield;
        Lexeme*       _lexeme;
        Float*        _float;
        Integer*      _integer;
    };
    Kind _kind;
};

}  // end namespace Electron

#endif // end __LibElectron_GCLocker_h__
