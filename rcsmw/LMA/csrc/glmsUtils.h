#ifndef GLMS_UTILS_H
#define GLMS_UTILS_H

#include "stdint.h"
#include "timetFix.h"

GlmsBool         compareMoKeys(const char *first, const char *second);
GlmsBool         compareKeyIds(const char *first, const char *second);
GlmsBool         verifyMoStringValidity(const char *moId);
GlmsBool         isSignalResultOk(GlmsResult result);
int32_t          strInStr(const char *needle, const char *haystack);
GlmsBool         validLkDates(time_t32 startTime, time_t32 stopTime);
void             glms_logEvent(GlmsLogLevel logLevel, const char *fmt, ...);
char *           getNextParameter(const char *needle, char *strIn);
void             fillParameter(char *buf, const char *str, GlmsBool last);
void             cleanupEscapeCharacters(char *str);
uint32_t         glms_increment_uint32(uint32_t u32_variable);
GlmsBool         noStopDate(time_t32 time);

uint32_t         rd32(uint32_t in);
uint16_t         rd16(uint16_t in);
uint32_t         wr32(uint32_t in);
uint16_t         wr16(uint16_t in);


#endif /* GLMS_UTILS_H */
