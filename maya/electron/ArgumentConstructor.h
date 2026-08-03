/**
 * @file
 * Make constructing argument lists simpler
 * @copyright
 * maya
 * Copyright (c) 2012-2022, Joshua Scoggins
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 */

#ifndef MAYA_ARGUMENTCONSTRUCTOR_H
#define MAYA_ARGUMENTCONSTRUCTOR_H
#include <functional>
#include <string>
#include <set>
#include <list>
#include <type_traits>
#include <sstream>
#include <tuple>
#include "electron/Aliases.h"
namespace Electron
{
    /**
     * @brief Mapping of CLIPS argument characters to an enum
     */
    enum class ArgumentTypes : char {
        Error = '\0',
        Any = '*',
        Integer = 'l',
        Float = 'd',
        String = 's',
        Symbol = 'y',
        InstanceName = 'n',
        Multifield = 'm',
        FactAddress = 'f',
        InstanceAddress = 'i',
        ExternalAddress = 'e',
        Boolean = 'b',
        Void = 'v',
    };
    /**
     * @brief Defines the types acceptible by a single argument in a CLIPS output or input argument
     */
    class SingleArgument final {
    public:
        template<typename ... T>
        static SingleArgument make(T&&... types) noexcept {
            SingleArgument arg;
            arg.addMany(types...) ;
            return arg;
        }
    public:
        SingleArgument() = default;
        template<typename ... T>
        explicit SingleArgument(T&&... args) noexcept {
            addMany(args...);
        }
        void add(ArgumentTypes type) noexcept {
            args_.emplace(type);
        }
        template<typename ... T>
        void addMany(T&&... types) noexcept {
            (add(types), ...);
        }
        [[nodiscard]] std::string str() const noexcept {
            std::ostringstream stream;
            for (const auto& a : args_) {
                stream << static_cast<std::underlying_type_t<ArgumentTypes>>(a);
            }
            // make sure that we don't have issues with temporaries
            auto newStr = stream.str();
            return newStr;
        }
        [[nodiscard]] bool empty() const noexcept {
            return args_.empty();
        }
        [[nodiscard]] auto size() const noexcept { return args_.size(); }
    private:
        std::set<ArgumentTypes> args_;
    };

    /**
     * @brief manages multiple single arguments at a time
     */
    class MultiArgument final {
    public:
        template<typename ... T>
        static MultiArgument make(T&&... args) noexcept {
            return MultiArgument {args...};
        }
    public:
        MultiArgument() = default;
        template<typename ... T>
        explicit MultiArgument(T&&... args) noexcept {
            addMany(args...);
        }

        void add(const SingleArgument& argument) noexcept {
            args_.emplace_back(argument);
        }
        template<typename ... T>
        void addMany(T&&... args) noexcept {
            (add(args), ...);
        }
        [[nodiscard]] bool empty() const noexcept {
            return args_.empty();
        }
        [[nodiscard]] std::string str() const noexcept {
            if (args_.empty()) {
                return "";
            }
            std::ostringstream stream;
            // a simple flag to make sure that we don't add ; to the front of the first element
            bool start = true;
            for (const auto& a : args_) {
                if (auto theStr = a.str(); start)  {
                    start = false;
                    stream << theStr;
                } else {
                    stream << ';' << theStr;
                }
            }
            auto outcome = stream.str();
            return outcome;
        }
    private:
        std::list<SingleArgument> args_;
    };
    /**
     * @brief Construct a type definition for a function return type
     * @tparam T The list of types (generally the ArgumentTypes enum) that will make up this function (duplicates accepted)
     * @param args The ArgumentTypes that will be accepted
     * @return the string representation of the valid types that can be returned from a given function
     */
    template<typename ... T>
    std::string makeReturnType(T&&... args) noexcept {
        if (sizeof...(T) == 0) {
            return "";
        } else {
            return SingleArgument::make(args...).str();
        }
    }
    /**
     * @brief Provide a list of single argument objects to make a multiargument string. Keep in mind, that this does nothing to help with the implicit default
     * @tparam T The list of types that the multi argument will hold (generally SingleArgument objects)
     * @param args The list of single argument objects to make the argument string out of
     * @return The final string representation of all these arguments or empty string if no arguments provided
     */
    template<typename ... T>
    std::string makeArgumentList(T&&... args) noexcept {
        if (sizeof...(T) == 0) {
            return "";
        } else {
            return MultiArgument::make(args...).str();
        }
    }
    /**
     * @brief Add a boolean return type (meant to return false) to the list of available returns to make it optional.
     * CLIPS returns false when something fails and the result isn't supposed to be a boolean.
     * @tparam T The list of types that are supposed to be returned by the function
     * @param args The types that are supposed to be returned by the function on success
     * @return A return type string with boolean added into the mix
     */
    template<typename ... T>
    std::string optionalReturnType(T&&... args) noexcept {
        return makeReturnType(ArgumentTypes::Boolean, args...);
    }
    static inline const SingleArgument returnsNothing {ArgumentTypes::Void};
} // end namespace Electron

#endif //MAYA_ARGUMENTCONSTRUCTOR_H
