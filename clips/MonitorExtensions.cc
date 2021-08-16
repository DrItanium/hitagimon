//
// Created by jwscoggins on 8/15/21.
//

#include "MonitorExtensions.h"
#include "clips.h"
/// @todo fix this
#include "../chipset/ChipsetInteract.h"

#define X(title) void title (Environment*, UDFContext*, UDFValue*)
X(ExamineByte);
X(ExamineShort);
X(ExamineWord);
X(ExamineLongWord);
X(ShiftLeft);
X(ShiftRight);
X(BinaryAnd);
X(BinaryOr);
X(BinaryNot);
X(BinaryNor);
X(BinaryNand);
X(BinaryXor);
X(BinaryAndNot);
X(BinaryNotAnd);
X(BinaryOrNot);
X(BinaryNotOr);
#undef X

extern "C"
void
InstallMonitorExtensions(Environment* env) {
    AddUDF(env, "examine-byte", "l", 1, 1, "l", ExamineByte, "ExamineByte",NULL);
    AddUDF(env, "examine-short", "l", 1, 1, "l", ExamineShort, "ExamineShort",NULL);
    AddUDF(env, "examine-word", "l", 1, 1, "l", ExamineWord, "ExamineWord",NULL);
    AddUDF(env, "examine-long-word", "l", 1, 1, "l", ExamineLongWord, "ExamineLongWord",NULL);
    AddUDF(env, "shift-left", "l", 2, 2, "l", ShiftLeft, "ShiftLeft", NULL);
    AddUDF(env, "shift-right", "l", 2, 2, "l", ShiftRight, "ShiftRight", NULL);
    AddUDF(env, "binary-and", "l", 2, 2, "l", BinaryAnd, "BinaryAnd", NULL);
    AddUDF(env, "binary-or", "l", 2, 2, "l", BinaryOr, "BinaryOr", NULL);
    AddUDF(env, "binary-xor", "l", 2, 2, "l", BinaryXor, "BinaryXor", NULL);
    AddUDF(env, "binary-nor", "l", 2, 2, "l", BinaryNor, "BinaryNor", NULL);
    AddUDF(env, "binary-nand", "l", 2, 2, "l", BinaryNand, "BinaryNand", NULL);
    AddUDF(env, "binary-and-not", "l", 2, 2, "l", BinaryAndNot, "BinaryAndNot", NULL);
    AddUDF(env, "binary-not-and", "l", 2, 2, "l", BinaryNotAnd, "BinaryNotAnd", NULL);
    AddUDF(env, "binary-not", "l", 1, 1, "l", BinaryNot, "BinaryNot", NULL);
    AddUDF(env, "binary-or-not", "l", 2, 2, "l", BinaryOrNot, "BinaryOrNot", NULL);
    AddUDF(env, "binary-not-or", "l", 2, 2, "l", BinaryNotOr, "BinaryNotOr", NULL);
}
#define DefClipsFunction(name) void name (Environment* theEnv, UDFContext* context, UDFValue* retVal)

DefClipsFunction(ExamineByte) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint8_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineShort) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint16_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineWord) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, memory<uint32_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineLongWord) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        // this could be negative but who cares
        retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(memory<uint64_t>(static_cast<uint32_t>(retVal->integerValue->contents))));
    }
}

DefClipsFunction(ShiftLeft) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t base = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t shiftAmount = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(base << shiftAmount));
}

DefClipsFunction(ShiftRight) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t base = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t shiftAmount = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(base >> shiftAmount));
}

DefClipsFunction(BinaryNor) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((~a) & (~b)));
}

DefClipsFunction(BinaryNand) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((~a) | (~b)));
}
DefClipsFunction(BinaryAnd) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((a) & (b)));
}

DefClipsFunction(BinaryOr) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((a) | (b)));
}
DefClipsFunction(BinaryXor) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((a) ^ (b)));
}
DefClipsFunction(BinaryAndNot) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((a) & (~b)));
}
DefClipsFunction(BinaryNotAnd) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((~a) & (b)));
}

DefClipsFunction(BinaryNot) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    retVal->integerValue = CreateInteger(theEnv, ~CVCoerceToInteger(retVal));
}

DefClipsFunction(BinaryOrNot) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((a) | (~b)));
}
DefClipsFunction(BinaryNotOr) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    uint64_t a = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    uint64_t b = CVCoerceToInteger(&second);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>((~a) | (b)));
}

#undef DefClipsFunction
