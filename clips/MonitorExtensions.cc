//
// Created by jwscoggins on 8/15/21.
//

#include "MonitorExtensions.h"
#include "clips.h"
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
ExamineByte(Environment*, UDFContext*, UDFValue*) {

}
extern "C"
void
ExamineShort(Environment*, UDFContext*, UDFValue*) {

}
extern "C"
void
ExamineWord(Environment*, UDFContext*, UDFValue*) {

}
extern "C"
void
ExamineLongWord(Environment*, UDFContext*, UDFValue*) {

}
