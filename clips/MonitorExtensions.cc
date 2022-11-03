//
// Created by jwscoggins on 8/15/21.
//

#include "MonitorExtensions.h"
#include "clips.h"
/// @todo fix this
#include "../cortex/ChipsetInteract.h"
#include "../cortex/EnvironmentInterface.h"
#include "../cortex/SysExamine.h"
#include "../cortex/IODevice.h"

#define X(title) extern "C++" void title (Environment*, UDFContext*, UDFValue*)
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
X(BinaryXnor);
X(BinaryAndNot);
X(BinaryNotAnd);
X(BinaryOrNot);
X(BinaryNotOr);
X(ExaminePC);
X(ExamineAC);
X(ExamineTC);
X(shrdi960);
X(DoSYNLD);
X(AddressICR);
//X(DoSYNMOV);
#ifdef __i960SB__
X(CallCos960);
X(CallSin960);
X(CallTan960);
#endif
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
    AddUDF(env, "binary-xnor", "l", 2, 2, "l", BinaryXnor, "BinaryXnor", NULL);
    AddUDF(env, "binary-nand", "l", 2, 2, "l", BinaryNand, "BinaryNand", NULL);
    AddUDF(env, "binary-and-not", "l", 2, 2, "l", BinaryAndNot, "BinaryAndNot", NULL);
    AddUDF(env, "binary-not-and", "l", 2, 2, "l", BinaryNotAnd, "BinaryNotAnd", NULL);
    AddUDF(env, "binary-not", "l", 1, 1, "l", BinaryNot, "BinaryNot", NULL);
    AddUDF(env, "binary-or-not", "l", 2, 2, "l", BinaryOrNot, "BinaryOrNot", NULL);
    AddUDF(env, "binary-not-or", "l", 2, 2, "l", BinaryNotOr, "BinaryNotOr", NULL);
    AddUDF(env, "examine-pc", "l", 0, 0, NULL, ExaminePC, "ExaminePC", NULL);
    AddUDF(env, "examine-ac", "l", 0, 0, NULL, ExamineAC, "ExamineAC", NULL);
    AddUDF(env, "examine-tc", "l", 0, 0, NULL, ExamineTC, "ExamineTC", NULL);
    AddUDF(env, "shrdi960", "l", 2, 2, "l", shrdi960, "shrdi960", NULL);
    AddUDF(env, "synld960", "l", 1,1, "l", DoSYNLD, "DoSYNLD", NULL);
    AddUDF(env, "address:interrupt-control-register", "l", 0, 0, NULL, AddressICR, "AddressICR", NULL);
    //AddUDF(env, "synmov960", "l", 1, 1, "l", DoSYNMOV, "DoSNMOV", NULL);
#ifdef __i960SB__
    AddUDF(env, "cos960","d",1,1,"ld",CallCos960,"CallCos960",NULL);
    AddUDF(env, "sin960","d",1,1,"ld",CallSin960,"CallSin960",NULL);
    AddUDF(env, "tan960","d",1,1,"ld",CallTan960,"CallTan960",NULL);
#endif
}
#define DefClipsFunction(name) void name (Environment* theEnv, UDFContext* context, UDFValue* retVal)
DefClipsFunction(ExamineByte) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, cortex::memory<uint8_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineShort) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, cortex::memory<uint16_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineWord) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        retVal->integerValue = CreateInteger(theEnv, cortex::memory<uint32_t>(static_cast<uint32_t>(retVal->integerValue->contents)));
    }
}

DefClipsFunction(ExamineLongWord) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        // this could be negative but who cares
        retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(cortex::memory<uint64_t>(static_cast<uint32_t>(retVal->integerValue->contents))));
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

union DecomposedType {
    uint64_t total;
    int64_t totalI;
    uint32_t halves[sizeof(uint64_t)/sizeof(uint32_t)];
};

DefClipsFunction(BinaryNor) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    DecomposedType a;
    a.total = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    DecomposedType b;
    b.total = CVCoerceToInteger(&second);
    DecomposedType c;
    c.halves[0] = ~(a.halves[0] | b.halves[0]);
    c.halves[1] = ~(a.halves[1] | b.halves[1]);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(c.total));
}
DefClipsFunction(BinaryXnor) {
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    DecomposedType a;
    a.total = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    DecomposedType b;
    b.total = CVCoerceToInteger(&second);
    DecomposedType c;
    c.halves[0] = ~(a.halves[0] ^ b.halves[0]);
    c.halves[1] = ~(a.halves[1] ^ b.halves[1]);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(c.total));
}
DefClipsFunction(BinaryNand) {
    // forces the use of the i960's nand instruction
    UDFValue first, second;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&first)) {
        return;
    }
    DecomposedType a;
    a.total = CVCoerceToInteger(&first);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &second)) {
        return;
    }
    DecomposedType b;
    b.total = CVCoerceToInteger(&second);
    DecomposedType c;
    c.halves[0] = ~(a.halves[0] & b.halves[0]);
    c.halves[1] = ~(a.halves[1] & b.halves[1]);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(c.total));
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

DefClipsFunction(ExaminePC) {
    cortex::ProcessControls pc;
    GetProcessControls(pc);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(pc.raw));
}
DefClipsFunction(ExamineAC) {
    cortex::ArithmeticControls ac;
    GetArithmeticControls(ac);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(ac.raw));
}

DefClipsFunction(ExamineTC) {
    cortex::TraceControls tc;
    GetTraceControls(tc);
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(tc.raw));
}
DefClipsFunction(shrdi960) {
    UDFValue lenA, srcA;
    if (! UDFNthArgument(context,1,NUMBER_BITS,&srcA)) {
        return;
    }
    int32_t a = CVCoerceToInteger(&srcA);
    if (!UDFNthArgument(context, 2, NUMBER_BITS, &lenA)) {
        return;
    }
    int32_t b = CVCoerceToInteger(&lenA);
    int32_t result = 0;
    __asm__("shrdi %1, %2, %0" : "=r" (result) : "r" (b),  "r" (a));
    retVal->integerValue = CreateInteger(theEnv, static_cast<int64_t>(result));
}
#ifdef __i960SB__
DefClipsFunction(CallCos960) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        retVal->floatValue = CreateFloat(context->environment,0.0);
        return;
    }

    double floatValue = CVCoerceToFloat(retVal);
    double result = 0.0;
    __asm__("cosrl %1, %0" : "=r" (result) : "r" (floatValue));
    // okay now we need to force the assembler to call the builtin cosine function
    retVal->floatValue = CreateFloat(theEnv,result);
}
DefClipsFunction(CallSin960) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        retVal->floatValue = CreateFloat(context->environment,0.0);
        return;
    }

    double floatValue = CVCoerceToFloat(retVal);
    double result = 0.0;
    __asm__("sinrl %1, %0" : "=r" (result) : "r" (floatValue));
    // okay now we need to force the assembler to call the builtin cosine function
    retVal->floatValue = CreateFloat(theEnv,result);
}
DefClipsFunction(CallTan960) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        retVal->floatValue = CreateFloat(context->environment,0.0);
        return;
    }

    double floatValue = CVCoerceToFloat(retVal);
    double result = 0.0;
    __asm__("tanrl %1, %0" : "=r" (result) : "r" (floatValue));
    // okay now we need to force the assembler to call the builtin cosine function
    retVal->floatValue = CreateFloat(theEnv,result);
}
#endif


DefClipsFunction(DoSYNLD) {
    if (! UDFNthArgument(context,1,NUMBER_BITS,retVal)) {
        return;
    }
    if (CVIsType(retVal, INTEGER_BIT)) {
        uint32_t value = static_cast<uint32_t>(retVal->integerValue->contents);
        uint32_t result = 0;
        cortex::ArithmeticControls ac;
        asm volatile ("synld %1, %0" : "=r" (result) : "r" (value));
        GetArithmeticControls(ac);
        printf("Condition Code: 0x%x\n", static_cast<int>(ac.cc));
        retVal->integerValue = CreateInteger(theEnv, static_cast<int>(result));
    }
}

DefClipsFunction(AddressICR) {
    retVal->integerValue = CreateInteger(theEnv, 0xFF000004);
}


#undef DefClipsFunction
