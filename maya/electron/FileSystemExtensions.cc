/**
 * @file
 * Implementation of the extensions to electron which provide file system related abilities
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

#include "electron/FileSystemExtensions.h"
#include "electron/Environment.h"
#include "fs/path.h"
#include <string>
extern "C" {
#include "clips/clips.h"
}


namespace Electron
{
/// is the given path a directory?
static void isDirectory(UDF_ARGS__);

/// is the given path a file?
static void isFile(UDF_ARGS__);

/// return the extension of the given file
static void getFileExtension(UDF_ARGS__);

/// replace the extension of the given file
static void replaceFileExtension(UDF_ARGS__);

/// build a path and return it
static void buildPath(UDF_ARGS__);

/// get the stem of the given path and return it
static void stemPath(UDF_ARGS__);

/// get the filename of the given path
static void getFileName(UDF_ARGS__);

void
InitializeFilesystemExtensions(RawEnvironment* env)
{
    auto& theEnv = Environment::fromRaw(env);
    theEnv.addFunction("directoryp", "b", 1, 1, "sy;sy", isDirectory, "isDirectory");
    theEnv.addFunction("filep", "b", 1, 1, "sy;sy", isFile, "isFile");
    theEnv.addFunction("get-file-extension", "sy", 1, 1, "sy;sy", getFileExtension, "getFileExtension");
    theEnv.addFunction("replace-file-extension", "sy", 2, 2, "sy;sy;sy", replaceFileExtension, "replaceFileExtension");
    theEnv.addFunction("build-fs-path", "sy", 0, UNBOUNDED, "sy", buildPath, "buildPath");
    theEnv.addFunction("fs-stem", "sy", 1, 1, "sy;sy", stemPath, "stemPath");
    theEnv.addFunction("get-filename", "sy", 1, 1, "sy;sy", getFileName, "getFileName");
}

void
stemPath(UDF_ARGS__) 
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        out->lexemeValue = theEnv.falseSymbol();
        return;
    } else {
        Neutron::Path filePath(arg0.lexemeValue->contents);
        try {
            out->lexemeValue = theEnv.createString(filePath.stem());
        } catch (Neutron::FilesystemError& err) { // parasoft-suppress EXCEPT-04 "If an exception happens then we have to handle it here, we can't throw exceptions back through C code!"
            out->lexemeValue = theEnv.falseSymbol(); 
        }
    }
}

void
replaceFileExtension(UDF_ARGS__) 
{
    auto& theEnv = Environment::fromRaw(env); 
    UDFValue filePathVar;
    UDFValue replacementVar;
    if (!theEnv.firstArgument(context, ArgumentBits::Lexeme, &filePathVar)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else if (!theEnv.nextArgument(context, ArgumentBits::Lexeme, &replacementVar)) {
        out->lexemeValue = theEnv.falseSymbol();
    } else {
        Neutron::Path filePath(filePathVar.lexemeValue->contents);
        Neutron::Path replacement(replacementVar.lexemeValue->contents);
        try {
            out->lexemeValue = theEnv.createString(filePath.replace_extension(replacement));
        } catch (Neutron::FilesystemError& err) { // parasoft-suppress EXCEPT-04 "If an exception happens then we have to handle it here, we can't throw exceptions back through C code!"
            out->lexemeValue = theEnv.falseSymbol();
        }
    }
}

void
buildPath(UDF_ARGS__) 
{
    if (auto& theEnv = Environment::fromRaw(env); theEnv.getArgumentCount(context) == 0) {
        out->lexemeValue = theEnv.createString("");
    } else {
        Neutron::Path result;
        UDFValue temp;
        while (theEnv.hasNextArgument(context)) {
            theEnv.nextArgument(context, ArgumentBits::Lexeme, &temp);
            result /= temp.lexemeValue->contents;
        }
        out->lexemeValue = theEnv.createString(result);
    }
}

void
isDirectory(UDF_ARGS__) 
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        Neutron::Path path(arg0.lexemeValue->contents);
        try {
            out->lexemeValue = theEnv.createBool(Neutron::isDirectory(path));
        } catch (Neutron::FilesystemError& err) { // parasoft-suppress EXCEPT-04 "If an exception happens then we have to handle it here, we can't throw exceptions back through C code!"
            out->lexemeValue = theEnv.falseSymbol();
        }
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
isFile(UDF_ARGS__) 
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        Neutron::Path path(arg0.lexemeValue->contents);
        try {
            out->lexemeValue = theEnv.createBool(Neutron::isRegularFile(path));
        } catch (Neutron::FilesystemError& err) { // parasoft-suppress EXCEPT-04 "If an exception happens then we have to handle it here, we can't throw exceptions back through C code!"
            out->lexemeValue = theEnv.falseSymbol();
        }
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}


void
getFileExtension(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        Neutron::Path path(arg0.lexemeValue->contents);
        out->lexemeValue = theEnv.createString(path.extension().string().c_str());
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

void
getFileName(UDF_ARGS__)
{
    auto& theEnv = Environment::fromRaw(env);
    UDFValue arg0;
    if (theEnv.firstArgument(context, ArgumentBits::Lexeme, &arg0)) {
        Neutron::Path path(arg0.lexemeValue->contents);
        out->lexemeValue = theEnv.createString(path.filename().string().c_str());
    } else {
        out->lexemeValue = theEnv.falseSymbol();
    }
}

} // end namespace Electron
