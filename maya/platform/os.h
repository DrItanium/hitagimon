/**
 * @file
 * Platform autodetect features
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

#ifndef MAYA_OS_H
#define MAYA_OS_H
#ifdef __linux__
#define LINUX 1
#endif
#ifdef __APPLE__
#define DARWIN 1
#endif
#if defined(_WIN32) || defined(_WIN64)
#define WIN_MVC 1
#endif
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFlyBSD__)
#define UNIX_V 1
#endif
#if defined(__MINGW32__)
#define WIN_GCC 1
#endif
#if defined(ARDUINO)
#define EMBEDDED 1
#endif
#ifdef __cplusplus
namespace Neutron {
    constexpr bool onLinux() noexcept {
#ifdef __linux__
        return true;
#else
        return false;
#endif
    }
    constexpr bool onWindows() noexcept {
#if defined(_WIN32) || defined(_WIN64) || defined(__MINGW32__)
        return true;
#else
        return false;
#endif
    }
    constexpr bool onBSD() noexcept {
#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || defined(__DragonFlyBSD__)
        return true;
#else
        return false;
#endif
    }
    constexpr bool onMacOS() noexcept {
#ifdef __APPLE__
        return true;
#else
        return false;
#endif
    }
    constexpr bool onRaspberryPi() noexcept {
#ifdef __RASPBERRY_PI__
        return true;
#else
        return false;
#endif
    }
    constexpr bool onRaspberryPiOS() noexcept {
        return onRaspberryPi() && onLinux();
    }
} // end namespace Neutron
#endif // end defined(__cplusplus)
#endif //MAYA_OS_H
