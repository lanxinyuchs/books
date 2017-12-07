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
/* ---------------------------------------------------------------------------
 * Imported Interfaces, Types & Definitions
 * ---------------------------------------------------------------------------
 */
#include "osa_xmlParser.h"
#include <stdio.h>
#include <sys/stat.h>
#include <string.h>
#include <errno.h>
#include <malloc.h>
#include <stdlib.h>


/* ---------------------------------------------------------------------------
 * Static Interfaces
 * ---------------------------------------------------------------------------
 */
static void
setResult(const void *xmlParser, uint32_t errorType, uint32_t line);

static char *
strstrlimit(char *haystack_p, char *needle_p, const char *limit_p);

static char *
strrchrlimit(char *cs_p, const char *c_p, const char *limit_p);

static GlmsBool
searchForStartTag(const void *xmlParser);

static uint32_t
searchForEndTag(const void *xmlParser);

static GlmsBool
parseBuffert(const void *xmlParser);

static void
callBufHandler(const void *xmlParser);

static GlmsBool
updateFileOffset(const void *xmlParser);

static GlmsBool
fileReadToBuffert(const void *xmlParser);

static GlmsBool
openFile(const char *filename, const void *xmlParser);

static void
closeFile(const void *xmlParser);

static GlmsBool
allocateBuffert(const void *xmlParser, uint32_t line);

static void
freeBuffert(const void *xmlParser, uint32_t line);

static uint32_t
xmlParserInit(const void *xmlParser);

/* ---------------------------------------------------------------------------
 * Class Based Data
 * ---------------------------------------------------------------------------
 */
typedef OSA_XML_PARSER(parser);

#define RESULT(p)           ((parser *)p)->result
#define ERROR_STR(p)        ((parser *)p)->errorStr

#define TAG(p)              ((parser *)p)->tag
#define TAG_LATEST(p)       ((parser *)p)->latest
#define TAG_TABLE(p)        ((parser *)p)->tagTable
#define TAG_TABLE_SIZE(p)   ((parser *)p)->tagTableSize
#define TAG_START(p)               TAG(p)->startTag
#define TAG_END(p)                 TAG(p)->endTag
#define TAG_PARSE(p)               TAG(p)->parse

#define FILE_P(p)           ((parser *)p)->file
#define FILE_OFFSET(p)      ((parser *)p)->offset
#define FILE_PREV_OFFSET(p) ((parser *)p)->prevOffset
#define FILE_NOBJ(p)        ((parser *)p)->nObj
#define FILE_STAT(p)        ((parser *)p)->fileStat
#define FILE_SIZE(p)         FILE_STAT(p).st_size

#define BUF(p)              ((parser *)p)->buf_p
#define BUF_START(p)        ((parser *)p)->start_p
#define BUF_END(p)          ((parser *)p)->end_p
#define BUF_SIZE(p)         ((parser *)p)->bufSize
#define BUF_PREV_SIZE(p)    ((parser *)p)->bufPrevSize
#define BUF_HANDLER(p)      ((parser *)p)->bufHandler
#define BUF_EXTENSION(p)    ((parser *)p)->bufExtension

#define END_TAG_FOUND       (0)
#define END_TAG_NOT_FOUND   (1)
#define END_TAG_NOT_PRESENT (2)

/* ---------------------------------------------------------------------------
 *
 * Name  : setResult
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static void
setResult(const void *xmlParser, uint32_t errorType, uint32_t line)
{

   /* Do not override any result except OSA_XML_SUCCESS */
   if (RESULT(xmlParser) == OSA_XML_SUCCESS)
   {
      char format[] = "%s (Detected in %s at line %lu)";

      RESULT(xmlParser) = errorType;

      switch (RESULT(xmlParser))
      {
      case OSA_XML_EXIT:
         /* The function xmlParserExit has been used */
         break;
      case OSA_XML_USER_ERROR:
         (void)snprintf(ERROR_STR(xmlParser),
                        OSA_XML_MAX_ERROR_STR_LEN,
                        format, "User called error", __FILE__, line);
         break;
      case OSA_XML_FS_ERROR:
         (void)snprintf(ERROR_STR(xmlParser),
                        OSA_XML_MAX_ERROR_STR_LEN,
                        format, strerror(errno), __FILE__, line);
         break;
      case OSA_XML_MEM_ERROR:
         (void)snprintf(ERROR_STR(xmlParser),
                        OSA_XML_MAX_ERROR_STR_LEN,
                        format, "Memory problem", __FILE__, line);
         break;
      case OSA_XML_ERROR:
         (void)snprintf(ERROR_STR(xmlParser),
                        OSA_XML_MAX_ERROR_STR_LEN,
                        format, "Programming fault", __FILE__, line);
         break;
      case OSA_XML_SUCCESS:
         break;
      default:
         (void)snprintf(ERROR_STR(xmlParser),
                        OSA_XML_MAX_ERROR_STR_LEN,
                        "Programming fault, %s line %u, error code %u is unknown",
                        __FILE__, line, RESULT(xmlParser));
         RESULT(xmlParser) = OSA_XML_ERROR;
         break;
      }
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : strstrlimit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static char *
strstrlimit(char *haystack_p, char *needle_p, const char *limit_p)
{
   char *start_p;


   for (start_p = haystack_p; start_p < limit_p; start_p++)
   {
      char *p1 = needle_p;
      char *p2 = start_p;

      while (*p1 != '\0')
      {
         if (*p1 != *p2)
         {
            /* Characters differ */
            break;
         }

         p1++;
         p2++;
      }

      if (*p1 == '\0')
      {
         /* Found match */

         return(start_p);
      }
   }

   /* Match not found */
   return(NULL);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : strrchrlimit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static char *
strrchrlimit(char *cs_p, const char *c_p, const char *limit_p)
{
   char *start_p;

   for (start_p = cs_p; start_p >= limit_p; start_p--)
   {
      if (*start_p == *c_p)
      {
         /* Character found */
         return(start_p);
      }
   }

   /* Character not found */
   return(NULL);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : buffertEnd
 *
 * Descr.: Return an end of the buffert pointer
 *
 * Args  :
 *
 * Return: Buffert end pointer
 *
 * ---------------------------------------------------------------------------
 */
static char *
buffertEnd(const void *xmlParser)
{

   return(&BUF(xmlParser)[FILE_NOBJ(xmlParser)]);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : searchForStartTag
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
searchForStartTag(const void *xmlParser)
{
   char *start_p;
   char *limit_p = buffertEnd(xmlParser);
   Tag *tag_p = (Tag *)TAG_TABLE(xmlParser);
   Tag *lastTag_p = (Tag *)TAG_TABLE(xmlParser) + TAG_TABLE_SIZE(xmlParser)/sizeof(Tag);

   /* Not found */
   TAG(xmlParser) = NULL;

   /* Search for the latest found tag */
   if (TAG_LATEST(xmlParser) != NULL)
   {
      /* Search for the tag until the limit_p */
      start_p = strstrlimit(BUF_START(xmlParser), TAG_LATEST(xmlParser)->startTag, limit_p);

      if (start_p != NULL)
      {
         /* Limit the next search if a tag is found */
         limit_p = start_p;
         TAG(xmlParser) = TAG_LATEST(xmlParser);
      }
   }

   /* Check for other tags */
   while (tag_p < lastTag_p)
   {
      if (tag_p != TAG_LATEST(xmlParser))
      {
         /* Search for the tag until the limit_p */
         start_p = strstrlimit(BUF_START(xmlParser), tag_p->startTag, limit_p);

         if (start_p != NULL)
         {
            /* Limit the next search */
            limit_p = start_p;
            TAG(xmlParser) = tag_p;
         }
      }

      /* Get the next tag */
      tag_p++;
   }

   if (TAG(xmlParser) != NULL)
   {
      /* The tag is found, set start to limit */
      BUF_START(xmlParser) = limit_p;

      /* The end of buffert is now start + lenght of the tag */
      BUF_END(xmlParser) = BUF_START(xmlParser) + strlen(TAG_START(xmlParser));

      /* Set the latest found start tag */
      TAG_LATEST(xmlParser) = TAG(xmlParser);
      return(GLMS_TRUE);
   }
   else
   {
      /* Start tag is not found */
      BUF_END(xmlParser) = buffertEnd(xmlParser);
      return(GLMS_FALSE);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : searchForEndTag
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static uint32_t
searchForEndTag(const void *xmlParser)
{
   char *endtag_p;

   // codechecker_suppress [core.NullDereference]
   // Access to field 'endTag' results in a dereference of a null pointer (loaded from field 'tag')
   if (TAG_END(xmlParser) != NULL)
   {
      /* Search for the end tag */

      endtag_p = strstrlimit(BUF_END(xmlParser), TAG_END(xmlParser), buffertEnd(xmlParser));

      if(endtag_p != NULL)
      {

         BUF_END(xmlParser) = endtag_p + strlen(TAG_END(xmlParser));
         return(END_TAG_FOUND);
      }
   }
   else		/* The end tag is not used */
   {
      /* Search for the end of the start tag */

      endtag_p = strstrlimit(BUF_START(xmlParser), ">", buffertEnd(xmlParser));

      if (endtag_p != NULL)
      {

         BUF_END(xmlParser) = endtag_p + 1;
         return(END_TAG_FOUND);
      }
   }

   /* End tag not found, check if we have searched throw the file completley */
   if (FILE_OFFSET(xmlParser) + (long)FILE_NOBJ(xmlParser) < FILE_SIZE(xmlParser))
   {
      return(END_TAG_NOT_FOUND);
   }
   else
   {
      return(END_TAG_NOT_PRESENT);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : tagParse
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static void
tagParse(const void *xmlParser)
{
   if (TAG_PARSE(xmlParser) != NULL)
   {
      /* The end tag is found in this buffert */
      char tmp;

      /* Terminate the buffert */
      tmp = *(BUF_END(xmlParser));
      *(BUF_END(xmlParser)) = '\0';

      /* Call the parser function */
      if (!TAG_PARSE(xmlParser)(xmlParser))
      {

         setResult(xmlParser, OSA_XML_USER_ERROR, __LINE__);
      }

      /* Restore the buffert */
      *(BUF_END(xmlParser)) = tmp;
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : searchForLastPossibleTag
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static void
searchForLastPossibleTag(const void *xmlParser)
{
   char *lastpossibletag_p;

   /* Search for the last possible tag */
   lastpossibletag_p = strrchrlimit(BUF_END(xmlParser), "<", BUF_START(xmlParser));

   if (lastpossibletag_p == NULL)
   {
      /* No possible tag in this buffert, go to the end of the buffert */
      BUF_END(xmlParser) = buffertEnd(xmlParser);
   }
   else
   {
      /* A possible tag is found */
      char *completetag_p;

      /* Check if the tag is complete */
      completetag_p = strstrlimit(lastpossibletag_p, ">", buffertEnd(xmlParser));

      if (completetag_p != NULL)
      {
         /* The tag is complete and unknown, continue after this tag */
         BUF_END(xmlParser) = completetag_p;
      }
      else
      {
         /* The tag was not complete, continue from the start of this tag */
         BUF_END(xmlParser) = lastpossibletag_p;
      }
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : parseBuffert
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
parseBuffert(const void *xmlParser)
{
   if (RESULT(xmlParser) == OSA_XML_SUCCESS)
   {
      if (searchForStartTag(xmlParser))
      {
         switch (searchForEndTag(xmlParser))
         {
            case END_TAG_FOUND:
               tagParse(xmlParser);
               /* Continue after this tag */

               BUF_START(xmlParser) = BUF_END(xmlParser);
               return(GLMS_TRUE);
            case END_TAG_NOT_FOUND:
               /* End tag is not found in this chunk */

               BUF_END(xmlParser) = BUF_START(xmlParser);
               break;
            case END_TAG_NOT_PRESENT:
               /* End tag is not present in the file, skip this tag */

               BUF_START(xmlParser) += strlen(TAG_START(xmlParser));
               return(GLMS_TRUE);
            default:
               /* Programming fault */

               setResult(xmlParser, OSA_XML_ERROR, __LINE__);
               return(GLMS_FALSE);
         }
      }
      else
      {
         /* Start tag is not found */
         searchForLastPossibleTag(xmlParser);
      }

      /* Update file offset */
      FILE_OFFSET(xmlParser) += BUF_END(xmlParser) - BUF(xmlParser);

      /* Read a new chunk from the file */
      return(GLMS_FALSE);
   }
   else
   {
      return(GLMS_FALSE);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : bufHandler
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static void
callBufHandler(const void *xmlParser)
{

   if (BUF_HANDLER(xmlParser) != NULL)
   {
      /* Teminate the string */
      *(BUF_END(xmlParser)) = '\0';

      /* Call the user function */
      if (!BUF_HANDLER(xmlParser)(xmlParser))
      {
         setResult(xmlParser, OSA_XML_USER_ERROR, __LINE__);
      }
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : allocateBuffert
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
allocateBuffert(const void *xmlParser, uint32_t line)
{
   if (BUF(xmlParser) == NULL)
   {
      BUF(xmlParser) = (char *)malloc(BUF_SIZE(xmlParser) + 1);

      if (BUF(xmlParser) != NULL)
      {
         return(GLMS_TRUE);
      }
      else /* Out of memory */
      {
         setResult(xmlParser, OSA_XML_MEM_ERROR, line);
      }
   }
   else
   {
      /* Programming fault, memory leak */
      setResult(xmlParser, OSA_XML_ERROR, line);
   }

   return(GLMS_FALSE);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : freeBuffert
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static void
freeBuffert(const void *xmlParser, uint32_t line)
{
   if (BUF(xmlParser) != NULL)
   {
      free(BUF(xmlParser));
      BUF(xmlParser) = NULL;
   }
   else
   {
      setResult(xmlParser, OSA_XML_ERROR, line);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : extendBuffert
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
extendBuffert(const void *xmlParser)
{

   if (FILE_OFFSET(xmlParser) < FILE_SIZE(xmlParser))
   {
      int32_t bytesLeftOfFile;

      BUF_PREV_SIZE(xmlParser) = BUF_SIZE(xmlParser);

      /* Set a new buffert size */
      BUF_SIZE(xmlParser) = BUF_SIZE(xmlParser) * 2;
      bytesLeftOfFile = FILE_SIZE(xmlParser) - FILE_OFFSET(xmlParser);

      if (bytesLeftOfFile < 0)
      {
         setResult(xmlParser, OSA_XML_ERROR, __LINE__);
         return(GLMS_FALSE);
      }

      /* Do not allocated more than needed */
      if (BUF_SIZE(xmlParser) > (uint32_t)bytesLeftOfFile)
      {
         BUF_SIZE(xmlParser) = (uint32_t)bytesLeftOfFile;
      }

      /* Check that we did not tried this before */
      if (BUF_PREV_SIZE(xmlParser) != BUF_SIZE(xmlParser))
      {
         BUF_PREV_SIZE(xmlParser) = BUF_SIZE(xmlParser);

         /* Allocate a new buffert */
         freeBuffert(xmlParser, __LINE__);
         return(allocateBuffert(xmlParser, __LINE__));
      }
      else
      {
         callBufHandler(xmlParser);
         return(GLMS_FALSE);
      }
   }
   else if (FILE_OFFSET(xmlParser) == FILE_SIZE(xmlParser))
   {
      /* End of file, nothing more to read */
      return(GLMS_FALSE);
   }
   else /* FILE_OFFSET(xmlParser) > FILE_SIZE(xmlParser) */
   {
      setResult(xmlParser, OSA_XML_ERROR, __LINE__);
      return(GLMS_FALSE);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : updateFileOffset
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
updateFileOffset(const void *xmlParser)
{

   if (RESULT(xmlParser) == OSA_XML_SUCCESS)
   {
      if (FILE_OFFSET(xmlParser) == FILE_PREV_OFFSET(xmlParser))
      {
         /* We are reading from the same offset, go to the end of this buffert */
         BUF_END(xmlParser) = buffertEnd(xmlParser);

         /* The buffert could be to small, extend the buffert */
         if (BUF_EXTENSION(xmlParser))
         {
            return(extendBuffert(xmlParser));
         }

         /* Update file offset */
         FILE_OFFSET(xmlParser) += BUF_END(xmlParser) - BUF(xmlParser);

         /* Check that file offset is increasing */
         if (FILE_OFFSET(xmlParser) == FILE_PREV_OFFSET(xmlParser))
         {
            return(GLMS_FALSE);
         }
      }

      /* Remember the offset */
      FILE_PREV_OFFSET(xmlParser) = FILE_OFFSET(xmlParser);

      /* We are done with this chunk, call the buffert handler */
      callBufHandler(xmlParser);
      return(GLMS_TRUE);
   }
   else
   {
      return(GLMS_FALSE);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : fileReadToBuffert
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
fileReadToBuffert(const void *xmlParser)
{

   if (RESULT(xmlParser) == OSA_XML_SUCCESS)
   {
      if (fseek(FILE_P(xmlParser), FILE_OFFSET(xmlParser), SEEK_SET) == 0)
      {
         FILE_NOBJ(xmlParser) = fread(BUF(xmlParser),
                                      sizeof(char),
                                      BUF_SIZE(xmlParser),
                                      FILE_P(xmlParser));
         BUF(xmlParser)[FILE_NOBJ(xmlParser)] = '\0';
         BUF_START(xmlParser) = BUF(xmlParser);
         BUF_END(xmlParser) = buffertEnd(xmlParser);

         if (FILE_NOBJ(xmlParser) == BUF_SIZE(xmlParser))
         {
            return(GLMS_TRUE);
         }
         else
         {
            if ((feof(FILE_P(xmlParser)) != 0) && (ferror(FILE_P(xmlParser)) == 0))
            {
               return(GLMS_TRUE); /* End of file */
            }
            else
            {
               setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__); /* fread fails */
            }
         }
      }
      else
      {
         setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__); /* fseek fails */
      }
   }

   return(GLMS_FALSE);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : xmlParserInit
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static uint32_t
xmlParserInit(const void *xmlParser)
{
   RESULT(xmlParser) = OSA_XML_SUCCESS;

   osaXmlSetTagTable(xmlParser, TAG_TABLE(xmlParser), TAG_TABLE_SIZE(xmlParser));

   FILE_P(xmlParser) = NULL;
   FILE_NOBJ(xmlParser) = 0;
   FILE_OFFSET(xmlParser) = 0;
   FILE_PREV_OFFSET(xmlParser) = 0;

   BUF(xmlParser) = NULL;
   BUF_START(xmlParser) = NULL;
   BUF_END(xmlParser) = NULL;

   return(RESULT(xmlParser));
}

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
                 uint32_t bufSize)
{

   TAG_TABLE(xmlParser) = tagTable;
   TAG_TABLE_SIZE(xmlParser) = tagTableSize;
   BUF_HANDLER(xmlParser) = NULL;
   BUF_SIZE(xmlParser) = bufSize;
   BUF_PREV_SIZE(xmlParser) = 0;

   /* Default setting for buffert extension */
   BUF_EXTENSION(xmlParser) = GLMS_FALSE;

   return(xmlParserInit(xmlParser));
}


/* ---------------------------------------------------------------------------
 *
 * Name  : openFile
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
static GlmsBool
openFile(const char *filename, const void *xmlParser)
{
   /* Open the file */
   FILE_P(xmlParser) = fopen(filename, "r");

   if (FILE_P(xmlParser) != NULL)
   {
      if (stat(filename, &FILE_STAT(xmlParser)) == 0)
      {
         return(GLMS_TRUE); /* Success */
      }
      else /* stat fails */
      {
         setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__);
      }

      closeFile(xmlParser);
   }
   else /* fopen fails */
   {
      setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__);
   }

   return(GLMS_FALSE);
}

/* ---------------------------------------------------------------------------
 *
 * Name  : closeFile
 *
 * Descr.: Close the file opend by function fileOpen
 *
 * Args  : xmlParser - Parsing intstructions
 *
 * Return: -
 *
 * ---------------------------------------------------------------------------
 */
static void
closeFile(const void *xmlParser)
{
   if (FILE_P(xmlParser) != NULL)
   {
      /* Check for any file system problem */
      if (ferror(FILE_P(xmlParser)) != 0)
      {
         setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__);
      }

      if (fclose(FILE_P(xmlParser)) != 0)
      {
         setResult(xmlParser, OSA_XML_FS_ERROR, __LINE__);
      }

      FILE_P(xmlParser) = NULL;
   }
   else
   {
      setResult(xmlParser, OSA_XML_ERROR, __LINE__);
   }
}

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
                  uint32_t tagTableSize)
{
   Tag *tag_p = (Tag *)tagTable;
   Tag *lastTag_p = (Tag *)tagTable + tagTableSize / sizeof(Tag);

   /* Check that start tag is used (mandatory) */
   while (tag_p < lastTag_p)
   {
      if (tag_p->startTag == NULL)
      {
         setResult(xmlParser, OSA_XML_ERROR, __LINE__);
         return;
      }

      /* Get the next tag */
      tag_p++;
   }

   TAG(xmlParser) = NULL;
   TAG_LATEST(xmlParser) = NULL;
   TAG_TABLE(xmlParser) = tagTable;
   TAG_TABLE_SIZE(xmlParser) = tagTableSize;
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlFileParser
 *
 * Descr.: Pare the file according to the osa parser struct
 *
 * Args  : filename  - File to be parsed
 *         xmlParser - Parsing instructions
 *
 * Return: OSA_XML_SUCCESS      - Success
 *         OSA_XML_FS_ERROR     - File system problem
 *         OSA_XML_EXIT         - Function xmlParserExit was used
 *         OSA_XML_MEM_ERROR    - Memory problem
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlFileParser(const char *filename, const void *xmlParser)
{
   /* Open the file, allocated the buffert and read the first chunk */

   if (osaXmlOpenParser(filename, xmlParser) == OSA_XML_SUCCESS)
   {
      /* Parse the file */

      RESULT(xmlParser) = osaXmlParser(xmlParser);
   }

   /* Close the parser and return the result */
   return(osaXmlCloseParser(xmlParser));
}


/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlOpenParser
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
osaXmlOpenParser(const char *filename, const void *xmlParser)
{
   /* Init the parser */
   if (xmlParserInit(xmlParser) == OSA_XML_SUCCESS)
   {
      /* Open the file */
      if (openFile(filename, xmlParser))
      {
         /* Allocate the buffert */
         if (allocateBuffert(xmlParser, __LINE__))
         {
            /* Read the first chunk from the file */
            if (fileReadToBuffert(xmlParser))
            {
               /* Success */
               return(RESULT(xmlParser));
            }
         }
      }
   }

   /* Problem, clean up and return the result */
   return(osaXmlCloseParser(xmlParser));
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParser
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
osaXmlParser(const void *xmlParser)
{
   RESULT(xmlParser) = OSA_XML_SUCCESS;

   do /* Parse the file */
   {
      /* Parse the buffert */
      while (parseBuffert(xmlParser)){};

      /* Update the file offset before reading from the file */
      if (!updateFileOffset(xmlParser))
      {
         break;
      }

      /* Read from the file to the buffert */
   } while (fileReadToBuffert(xmlParser));

   return(RESULT(xmlParser));
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlCloseParser
 *
 * Descr.: Close the file and free allocated memory
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
uint32_t
osaXmlCloseParser(const void *xmlParser)
{

   /* Free the allocated buffert */
   freeBuffert(xmlParser, __LINE__);

   /* Close the file */
   closeFile(xmlParser);

   /* And finally return the result */
   return(RESULT(xmlParser));
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserTag
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserTag(const void *xmlParser)
{
   return(BUF_START(xmlParser));
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserDupTag
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserDupTag(const void *xmlParser)
{
   char *tag_p;
   uint32_t len;

   /* Add space for string terminator */
   len = osaXmlParserTagLen(xmlParser) + 1;
   tag_p = (char *)malloc(len);

   if (tag_p != NULL)
   {
      strncpy(tag_p, BUF_START(xmlParser), len);
      tag_p[len - 1] = '\0';
   }
   else
   {
      setResult(xmlParser, OSA_XML_MEM_ERROR, __LINE__);
   }

   return(tag_p);
}

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
osaXmlParserTagLen(const void *xmlParser)
{
   int32_t len;

   len = BUF_END(xmlParser) - BUF_START(xmlParser);

   if (len >= 0)
   {
      return((uint32_t)len);
   }
   else
   {
      setResult(xmlParser, OSA_XML_ERROR, __LINE__);
      return(0);
   }
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlParserBuf
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlParserBuf(const void *xmlParser)
{
   return(BUF(xmlParser));
}

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
osaXmlParserExit(const void *xmlParser)
{
   setResult(xmlParser, OSA_XML_EXIT, __LINE__);
}

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
osaXmlSetBufHandler(const void *xmlParser, GlmsBool (*bufHandler)(const void *xmlParser))
{
   BUF_HANDLER(xmlParser) = bufHandler;
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlSetBufExtension
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
osaXmlSetBufExtension(const void *xmlParser, GlmsBool bufExtension)
{
   BUF_EXTENSION(xmlParser) = bufExtension;
}

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
osaXmlGetResult(const void *xmlParser)
{
   return(RESULT(xmlParser));
}

/* ---------------------------------------------------------------------------
 *
 * Name  : osaXmlGetErrorStr
 *
 * Descr.:
 *
 * Args  :
 *
 * Return:
 *
 * ---------------------------------------------------------------------------
 */
char *
osaXmlGetErrorStr(const void *xmlParser)
{
   return(ERROR_STR(xmlParser));
}



/* ### End of file osa_XmlParser.c ### */
