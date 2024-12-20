/*
i960SxChipset
Copyright (c) 2020-2021, Joshua Scoggins
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
/*
 * Fault handler routines
 */
#include "../cortex/IODevice.h"
#include "../cortex/Faults.h"
#include "../cortex/SysExamine.h"
#include <string>
void
basicDisplay(const std::string& kind, cortex::FaultData& record, cortex::ProcedureStackFrame& pfp) {
    // we need to be careful not to cause an infinite loop
    cortex::ChipsetBasicFunctions::Console::write(kind.c_str());
    cortex::ChipsetBasicFunctions::Console::writeLine(" FAULT RAISED!");
    cortex::ChipsetBasicFunctions::Console::writeLine("{");
    record.display(pfp);
    cortex::ChipsetBasicFunctions::Console::writeLine("}");
}
inline void
basicOperation(const std::string& kind, cortex::FaultData* record, cortex::ProcedureStackFrame* fp, cortex::FaultHandler handler) {
    if (handler)  {
        handler(*record, *fp);
    } else {
        basicDisplay(kind, *record, *fp);
    }
}
#define X(kind, code, locase, hicase) \
extern "C"                      \
void                            \
user_ ## locase (cortex::FaultData* record, cortex::ProcedureStackFrame* fp) { \
    basicOperation( "USER " #hicase , record, fp, cortex::getUser ## kind ## FaultHandler ()); \
}
#include "cortex/Faults.def"
#undef X
