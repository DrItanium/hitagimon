//
// Created by jwscoggins on 8/15/21.
//

#include "MonitorExtensions.h"
#include "clips.h"
/// @todo fix this
#include "../chipset/ChipsetInteract.h"
extern "C" void ExamineByte(Environment*, UDFContext*, UDFValue*);
extern "C" void ExamineShort(Environment*, UDFContext*, UDFValue*);
extern "C" void ExamineWord(Environment*, UDFContext*, UDFValue*);
extern "C" void ExamineLongWord(Environment*, UDFContext*, UDFValue*);

extern "C"
void
InstallMonitorExtensions(Environment* env) {
    AddUDF(env, "examine-byte", "ld", 1, 1, "ld", ExamineByte, "ExamineByte",NULL);
    AddUDF(env, "examine-short", "ld", 1, 1, "ld", ExamineShort, "ExamineShort",NULL);
    AddUDF(env, "examine-word", "ld", 1, 1, "ld", ExamineWord, "ExamineWord",NULL);
    AddUDF(env, "examine-long-word", "ld", 1, 1, "ld", ExamineLongWord, "ExamineLongWord",NULL);
}

extern "C"
void
ExamineByte(Environment* theEnv, UDFContext* context, UDFValue* retVal) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint8_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}
extern "C"
void
ExamineShort(Environment* theEnv, UDFContext* context, UDFValue* retVal) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint16_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}
extern "C"
void
ExamineWord(Environment* theEnv, UDFContext* context, UDFValue* retVal) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint32_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}
extern "C"
void
ExamineLongWord(Environment* theEnv, UDFContext* context, UDFValue* retVal) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        // this could be negative but who cares
        retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(memory<uint64_t>(static_cast<uint32_t>(retVal->integerValue->contents))));
    }
}
