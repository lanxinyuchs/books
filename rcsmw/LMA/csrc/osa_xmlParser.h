/**
 *   Copyright (C) 2013 by Ericsson AB. All rights reserved. The
 *   information in this document is the property of Ericsson. Except
 *   as specifically authorized in writing by Ericsson, the receiver
 *   of this document shall keep the information contained herein
 *   confidential and shall protect the same in whole or in part from
 *   disclosure and dissemination to third parties. Disclosure and
 *   disseminations to the receiver's employees shall only be made on
 *   a strict need to know basis.
 */

/* ========================================================================
 *   History of development:
 *   -----------------------
 *   Revised : 2013-06-29 Ramakrushna Mishra
 *   Change  : Imported from OSA-UTIL_CNX9010416 .
 *
 * ========================================================================
 */

#ifndef OSA_XML_PARSER_H
#define OSA_XML_PARSER_H

#ifdef __cplusplus
extern "C" {
#endif

/* ---------------------------------------------------------------------------
 * Imported Interfaces, Types & Definitions
 * ---------------------------------------------------------------------------
 */

#include <stdint.h>
#include <sys/types.h>
#include <sys/stat.h>
#include "glmsadpi/glmsDataTypes.h"
/* ---------------------------------------------------------------------------
 * Exported Types & Definitions
 * ---------------------------------------------------------------------------
 */

/* Result codes from the function osaXmlParser */
#define OSA_XML_SUCCESS     (0) /* Success                                      */
#define OSA_XML_EXIT        (1) /* The function osaXmlParserExit has been used  */
#define OSA_XML_FS_ERROR    (2) /* File system error                            */
#define OSA_XML_MEM_ERROR   (3) /* Memory allocation error                      */
#define OSA_XML_ERROR       (4) /* Programming error                            */
#define OSA_XML_USER_ERROR  (5) /* A parse function has returned false          */

#define OSA_XML_MAX_ERROR_STR_LEN (128)

/* The Tag Structure */
typedef struct Tag
{
   char *startTag;
   char *endTag;
   GlmsBool (*parse)(const void *xmlParser);
} Tag;

/* The OSA_XML_PARSER macro */
#define OSA_XML_PARSER(osaXmlParser)         \
struct                                       \
{                                            \
   const Tag *tagTable;                      \
   Tag *tag;                                 \
   Tag *latest;                              \
   FILE *file;                               \
   struct stat fileStat;                     \
   uint32_t result;                          \
   uint32_t tagTableSize;                    \
   uint32_t bufSize;                         \
   uint32_t bufPrevSize;                     \
   long offset;                              \
   size_t nObj;                              \
   long prevOffset;                          \
   char *start_p;                            \
   char *end_p;                              \
   char *buf_p;                              \
   GlmsBool bufExtension;                        \
   GlmsBool (*bufHandler)(const void *xmlParser);\
   char errorStr[OSA_XML_MAX_ERROR_STR_LEN]; \
}  osaXmlParser


/* ---------------------------------------------------------------------------
 * Exported Interfaces
 * ---------------------------------------------------------------------------
 */

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserInit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlParserInit(const void *xmlParser,
                 const Tag *tagTable,
                 uint32_t tagTableSize,
                 uint32_t bufSize);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlFileParser
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlFileParser(const char *fileName, const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlOpenParser
 *
 * Descr.: Open the parser, osaXmlParserInit has to be called first
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlOpenParser(const char *fileName, const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParser
 *
 * Descr.: Parse the file, osaXmlOpenParser has to be called first.
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlParser(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlCloseParser
 *
 * Descr.: Close the parser, osaXmlOpenParser has to be called first.
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlCloseParser(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserGetTag
 *
 * Descr.: Get the tag
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserTag(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserDupTag
 *
 * Descr.: Get a duplicated tag
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserDupTag(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserTagLen
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlParserTagLen(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserBuf
 *
 * Descr.: Get the buffert
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserBuf(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserExit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
void
osaXmlParserExit(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlSetBufHandler
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
void
osaXmlSetBufHandler(const void *xmlParser, GlmsBool (*bufHandler)(const void *xmlParser));

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlSetBufExtension
 *
 * Descr.: Set buffert extension. If true and a tag is to large for the selected
 *         buffert size the buffert is extendend.
 *
 * Args  : osaXmlParser - osaXmlParser struct pointer
 *         bufExtension - True or false
 *
 * Return: -
 *
 * ---------------------------------------------------------------------------
 */
void
osaXmlSetBufExtension(const void *xmlParser, GlmsBool bufExtension);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlGetResult
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlGetResult(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlGetErrorStr
 *
 * Descr.:
 *
 * Args  : osaXmlParser - osaXmlParser struct pointer
 *
 * Return: -
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlGetErrorStr(const void *xmlParser);

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlSetTagTable
 *
 * Descr.: Set a tag table
 *
 * Args  :
 *
 * Return: -
 *
 * ---------------------------------------------------------------------------
 */
void
osaXmlSetTagTable(const void *xmlParser,
                  const Tag *tagTable,
                  uint32_t tagTableSize);

#ifdef __cplusplus
}
#endif

#endif /* OSA_XML_PARSER_H */
