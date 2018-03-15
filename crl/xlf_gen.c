/******************************************************************************
 *
 *      COPYRIGHT (C)                 Ericsson Radio Systems AB, Sweden
 *
 *      The copyright to the computer program(s) herein is the property
 *      of Ericsson Radio Systems AB.
 *
 *      The program(s) may be used and/or copied only with the written
 *      permission from Ericsson Radio Systems AB or in accordance with
 *      the terms and conditions stipulated in the agreement/contract
 *      under which the program(s) have been supplied.
 *
 *****************************************************************************/

/******************************************************************************
 *
 * Product name:
 *      XLF_GEN
 *
 * File:
 *      xlf_gen.c
 *
 * Author:
 *      Jonas Weståker     3 May 2000
 *
 * Description:
 *      Manages the generation of XP-loadmodules.
 *
 *      The program supports and is generated for:
 *      - solaris2  (big endian)
 *      - linux     (little endian)
 *
 * Reviewed:
 *      2000-10-06, Jonas Weståker (QRAJSWR)
 *      2001-06-13, Anders Hallqvist (QRAHALT) IR: 4/1776-26/FCP1033502
 *
 * Revision history:
 *      2000-05-09, Rickard Fahlquist (QRAFAST)
 *              Added functions:
 *                      writeXpl()
 *                      writeIbootFooter()
 *                      calcSum()
 *                      wrapAndWriteFiles()
 *
 *              Modified functions:
 *                      writeIbootHeader()
 *                      parseCmdLine()
 *
 *      2000-07-07, Jonas Weståker      (QRAJSWR)
 *              Modified functions:
 *                      calcSum()
 *                      - Made identical to the one used by IBOOT
 *                      writeIbootHeader()
 *                      - Spotted and removed a faulty '&'
 *                      openFiles()
 *                      - Open XPL-file with 'w+b' properties in order to
 *                        be able to read from the file as well.
 *                      fileCalcsum()
 *                      - Added error-control on 'fread()'
 *
 *      2000-08-16, Rickard Fahlquist, (QRAFAST)
 *             - Cleaned up output to screen.
 *             - Ran the code through "FlexeLint"
 *
 *      2000-10-09, Rickard Fahlquist, (QRAFAST)
 *              - Corrected code after review.
 *
 *      2000-10-30, Rickard Fahlquist, (QRAFAST)
 *              - Changed the command line args so that an LMID-string can be
 *                provided instead of a SUID-string.
 *              - Corrected according to TR:s
 *                      WRNaa18953, Check for multiple instances of RPDOUT-files
 *                                  marked "OS".
 *                      WRNaa18949, Use "local time" when time-stamping file.
 *                      WRNaa18959, Set the LMID as default output file name.
 *
 *      2000-11-03, Rickard Fahlquist, (QRAFAST)
 *              - Corrected accordingly to TR
 *                      WRNaa19414, When the "-auboot_ini" flag is set, set
 *                                  the "update"-field in iboot-header to
 *                                  0xfffffffe.
 *
 *      2000-11-14, Rickard Fahlquist, (QRAFAST)
 *              - Corrected code according to TR
 *                      WRNAaa20827, Changed the LMID-string length check
 *                                   to 32 instead of 15. Also modified the
 *                                   SUID-string generation by adding the
 *                                   function 'generateSuidString()'.
 *
 *      2001-05-10, Peter Bergsten, (QRAPEBE)
 *              - Updated to new XLF format supporting compressed rpdout files
 *              - Corrected bugg with to few command options.
 *              - Added text in Usage printout.
 *              - The -l option can be used instead of the -lmid to uniform
 *                the options to one character. The long form is still
 *                available for backwards compability in the user interface.
 *              - Removed the now obsolete -au_boot_ini option. For backward
 *                comp. reasons the option will not cause an error if selected
 *                but a printout saying that the option is obsolete.
 *              - Added check that at least one of the rpdout files are
 *                marked as OS in the XLF output file.
 *              - Unified printout when copying the XPL image to similar
 *                printout as for the rpdout/input files.
 *              - Changed '/' characters to '%' in the output file name string
 *                when the lmid is used as output file name.
 *              - Fixed the problem that caused segmentation fault when
 *                a lmid was given with no underscore '_' in it.
 *              - Fixed the problem that caused segmentation fault when the
 *                options -r or -i was the last options on the command line.
 *
 *      2001-05-14, Peter Bergsten, (QRAPEBE)
 *              - Updated to support the declarations made in the new
 *                iboot_if.h (included by xpl.h) for the modified XLF format.
 *
 *      2001-06-13, Peter Bergsten, (QRAPEBE)
 *              - Updated according to IR: 4/1776-26/FCP1033502
 *
 *      2001-06-19, Peter Bergsten, (QRAPEBE)
 *              - Changed revision number
 *
 *      2001-10-15, Peter Bergsten (QRAPEBE)
 *              - Corrected TR:WRNaa53296,
 *              "Magic number is always 0x01111111 for user data LM's"
 *
 *      2006-06-26, Sven Löfgren (xedsven)
 *              Uses RV_STRING from version.h in XLF_GEN_VERSION_STRING.
 *              Removed compilation warning in printUsageAndDie().
 *
 *      2010-02-12, Sven Löfgren (xedsven)
 *              CR WRNae68076, Add Linux/i386 support for im_gen, xlf_gen.
 *
 *      2010-04-21, Sven Löfgren (qlofsve)
 *              TR WRNae83168, xlf_gen sometimes generates incorrect output
 *              file name, corrected.
 *
 *      2011-03-30, Ting Li (ellitng)
 *              WP NCI 098:Added XLF splitter for Solaris and Linux host.
 *
 *      2011-08-02, Ting Li (ellitng)
 *              TR HO57873: XLF_GEN does not validate the rpdout addresses properly.
 *
 *      2012-10-05, Zabihullah Bayat (ezabbay)
 *              Added support to pack an IBOOT in a XLF for Warp4 booting.
 *
 *      2013-02-25, wenxin yao(ewenyao)
 *              CR17063 reconfigure signal buffer sizes in XP Platform.
 *
 *      2013-10-15, wenxin yao(ewenyao)
 *              Bug in xlf_gen preventing use of -D option, need check filetype
 *              otherwise have error when some LM(e.g. txl.tbl.bin) marked OS.
 *
 *      2014-04-29, Hans Beckerus (qhanbec)
 *              Added the possibility to specify a target alignment.
 *
 *      2015-08-28 Amir Mohammad Koosha (eamikoo)
 *              New blob type is introduced
 *
 *      2015-10-22 Amir Mohammad Koosha (eamikoo)
 *              Append feature (manipulate) is added
 *
 *****************************************************************************/

/*----------------------------  Include files  ------------------------------*/

#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <endian.h>
#include <errno.h>
#include <time.h>
#include <ctype.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <libgen.h>

#include "zlib.h"
#include "xpl_if.h"
#include "version.h"
#include "crc32_lut.h"

/*----------------------------  CONSTANTS  ----------------------------------*/
#define XLF_GEN_VERSION_STRING          RV_STRING

#define XLF_FORMAT_VERSION_MAJOR        0
#define XLF_FORMAT_VERSION_MINOR        0

#define COMPRESSION_TYPE_ZLIB_113       0x02

#define NO_FLAG            (U8)0
#define INPUT_FLAG         (U8)1
#define OUTPUT_FLAG        (U8)2

#define XPL_FLAG           (U8)3
#define TYPE_FLAG          (U8)4
#define RPDOUT_FLAG        (U8)5
#define XLF_LMID_FLAG      (U8)6
#define XLF_L_FLAG         (U8)7

#define AUBOOT_INI_FLAG    (U8)8
#define XLF_RPDOUT_FILE    (U8)9
#define XLF_APPLIC_FILE    (U8)10

#define VERBOSE_FLAG       (U8)11
#define VERSION_FLAG       (U8)12

#define YES                (U8)13
#define NO                 (U8)14

#define SPLIT_FLAG         (U8)15

#define DEFINE_FLAG        (U8)16

#define BLOB_FLAG          (U8)17
#define XLF_BLOB_FILE      (U8)18

#define DO_NOT_CLEAN       (U8)19
#define APPEND_MODE        (U8)20

#define XLF_IBOOT_WPR_APPLIC_MAGIC  (U32)0x01111111

#define LMID_MAX_STRING_LENGTH (U16)32

#define SUID_STRING_LENGTH (U16)32

#define DEFAULT_SUID "DEFAULT                R1A01    "
#define OS_MASK (U16)0x0010

#define MAX_PATH             512

#define BUFF_SIZE            512

#define SIG_BUF_RECONFIG_ALLOWED_NUMBERS  7
#define DECOMP_BUF_SIZE                   500
#define SIG_BUF_RECONFIGURED_LM           "XPP_SIG_BUF_RECONFIG.out"
#define SIG_BUF_CONFIGURATION_SIGN        "krn/buffer_sizes="

#ifdef USHRT_MAX
#undef USHRT_MAX
#endif

#define USHRT_MAX                         65535

#define ALIGN_BYTES                       4
#define MAX_ALIGN                         ((U16)(USHRT_MAX + 1) - ALIGN_BYTES)

#define MAX_CREATE_FOLDER_ATTEMPTS        10000
#define STRLEN_ULONG_MAX                  10

/*----------------------------  MACROS  -------------------------------------*/
/* CRC32 macro updates crc32 value according to character c and previous crc32
 * values by using crc32Lut. */
#define CRC32(c, crc32) ((crc32)=crc32Lut[((crc32) ^ (c)) & 0xff] ^ ((crc32) >> 8))

/*----------------------------  Structs and typedefs  -----------------------*/
static const char *blobTypeStr[IBOOT_XlfBlobTypeE_LAST] =
{ /* Order should match IBOOT_XlfBlobTypeE_t */
	"UENVIMAGE",
	"DTB",
	"ROOTFS",
	"UIMAGE",
	"ASCII_DB",
	"XILINX_BIT"
};

static const char * const lmHeaderTagStr[] =
{
	"LM_HEADER_FORMAT:",
	"LM_TYPE:",
	"LM_MISC_VERSION:",
	"LM_ID:",
	"LM_NAME:"
};

enum lmHeaderTagStrE
{
	LM_HEADER_FORMAT,
	LM_TYPE,
	LM_MISC_VERSION,
	LM_ID,
	LM_NAME
};

static const char * const operationStr[] =
{
	"read from",
	"write to",
	"seek",
	"opening",
	"closing"
};

enum fileOperationE {
	READ_FILE,
	WRITE_FILE,
	SEEK_FILE,
	OPEN_FILE,
	CLOSE_FILE
};

/******************************************************************************
 * Type:
 *       FileRec_t
 *
 * Description:
 *       Contains a pointer to the file, the associated file-name and the
 *       associated file-type.
 *
 *****************************************************************************/
typedef struct fileRecS
{
	FILE *ptr;
	char *name;
	U8 fileType;
	U16 align; /* possible alignment request in output */
	U32 magic;
} FileRec_t;

typedef struct sigBufConfigS
{
	U32 numOfValues;
	U32 value[SIG_BUF_RECONFIG_ALLOWED_NUMBERS];
}sigBufConfig_t;

/******************************************************************************
 * Type:
 *       xlfDataS_t
 *
 * Description:
 *       Contains the data used by 'xlf_gen' in order to produce a valid
 *       '.xlf'-output file.
 *       The last member is an array whose size is determined by
 *       the last but one parameter.
 *
 *****************************************************************************/
typedef struct xlfDataS
{
	U8 type;
	U8 verboseMode;
	U8 outputFileSpecified;
	U8 xplSpecified;
	U8 aubootIni;
	U8 lmidPresent;
	U8 splitMode;
	U8 splitType;
	U8 manipulateMode;
	U8 blobTypeExist[IBOOT_XlfBlobTypeE_LAST];
	U16 osPresent;
	U16 inputFileCount;
	U16 cmdLineArgNo;
	U32 lmHeaderType;
	void *lmHeaderPtr;
	char lmid[LMID_MAX_STRING_LENGTH + 1];
	sigBufConfig_t sigBufCon;
	char *tmpFolderName;
	size_t        xlfIbootHdrSize;
	FileRec_t     xlfFile;
	FileRec_t     xplFile;
	FileRec_t     inputFile[1];
} xlfDataS_t;

/******************************************************************************
 * Type:
 *       XlfMemElemS_t
 *
 * Description:
 *      Holds information about the target RAM areas that will be
 *      occupied by the RPDOUT-files once they are loaded.
 *****************************************************************************/
typedef struct xlfMemoryElementS
{
	U32 offset;
	U32 size;
	struct xlfMemoryElementS *nextElem;
} XlfMemElemS_t;

/*----------------------------  Definition of Global Variables  -------------*/

/*----------------------------  Definition of Local Variables  --------------*/
static const char swPid[] = WHAT_STRING;

static const char * const blobHeaderFormatStr = "BLOB";

static const char * const originalXlfFileName = "original_xlf_file.xlf";

static char *xlfgenNameStr;
/*----------------------------  Declaration of Local Functions  -------------*/

static void parseCmdLine(int argc,
                         char **argv,
                         xlfDataS_t **xlfData);

static U8 getCmdLineFlag(char *cmdLineArg);

static void openFiles(xlfDataS_t **xlfData);

static void closeFiles(xlfDataS_t **xlfData);

static void writeXpl(xlfDataS_t **xlfData,
                     U32 *xlfOffset);

static void writeIbootFooter(xlfDataS_t **xlfData,
                             U32 *xlfOffset);

static void writeIbootHeader(xlfDataS_t *xlfData,
                             IBOOT_XlfIbootHeaderS_t *xlfIbootHdr,
                             U32 *xlfOffset);

static U32 calcSum(U8 *p, U32 size);

static void wrapAndWriteFiles(xlfDataS_t **xlfData,
                              U32 *xlfOffset,
                              IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static U32 fileCalcsum(U32 size, xlfDataS_t **xlfData);

static void append(U32 offset,
                   U32 size,
                   XlfMemElemS_t **memList,
                   xlfDataS_t **xlfData);

static void storeFileInfo(U8 fileType, char **argv, xlfDataS_t **xlfData, int argc);

static void parseLmHeaderFile(char *lmcHdrFileName, xlfDataS_t **xlfData);

static void initXlfData(xlfDataS_t **xlfData);

static void generateSuid(xlfDataS_t **xlfData, char *d, char *s);

static void convProdNoRevNo(char *dest, char *src, char delimiter);

static void getIbootHeader(xlfDataS_t **xlfData,
                           IBOOT_XlfIbootHeaderS_t **xlfIbootHdr);

static void getIbootFooter(xlfDataS_t *xlfData,
                           IBOOT_XlfIbootFooterS_t **xlfIbootFooter,
                           IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static void getIbootWrapper(xlfDataS_t *xlfData,
                            IBOOT_XlfIbootHeaderS_t *xlfIbootHdr,
                            IBOOT_XlfIbootWrapperS_t **xlfIbootWrapper,
                            U32 lmNo);

static void verifyCrc1(xlfDataS_t *xlfData,
                       IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static void verifyCrc2(xlfDataS_t *xlfData,
                       IBOOT_XlfIbootFooterS_t *xlfIbootFooter,
                       IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static void copyXpl(xlfDataS_t *xlfData, IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static void splitXlf(xlfDataS_t **xlfData,
                     IBOOT_XlfIbootHeaderS_t *xlfIbootHdr);

static unsigned short swap2Bytes(unsigned short d);

static U32 mallocVerbose(void **ptr, U32 size, U32 line);

static void mallocAndCopyString(char **ptr, const char *str);

static void mallocAndConcatStrings(char **ptr, const char *str1, const char *str2);

static int restoreAndCleanUpFiles(xlfDataS_t **xlfData);

static int readAndWriteCharFromInputFileToXlf(xlfDataS_t **xlfData,
                                              char *fileName,
                                              FILE *inputFP);

/*----------------------------  Function Definitions  -----------------------*/

/******************************************************************************
 *
 * Local function:
 *      printUsageAndDie
 *
 * Parameters:
 *      exitCode        Code passed to the exit system call
 *
 * Description:
 *      Prints the usage text on standard output and terminate the program
 *
 *  Side effects:
 *      Program terminates.
 *
 *****************************************************************************/
static void printUsageAndDie(int exitCode)
{
	printf("\
Usage:\n\
    XLF Generator:\n\
       %-7s [-r <[magic:]rpdout_file(s)> ...]\n\
               [-b <[magic:]blob_file(s)> ...]\n\
               [-x <XPL-image file>]\n\
               [-l <lmid-string>]\n\
               [-o <output_file>]\n\
               [-i <[magic:]input_file[@align]> ...]\n\
               [-t <au_applic|au_boot|boot|au_boot_linux|au_applic_linux|au_env|boot_env>]\n\
               [-v (verbose mode)]\n\
               [-V (%s version)]\n\
               [-D krn/buffer_sizes=<size1>,<size2>,<size3>,<size4>,<size5>,<size6>,<size7>]\n\
\n", xlfgenNameStr, xlfgenNameStr);
	printf("\
    LM Generator:\n\
       %-7s --lm <header_file> <input_file> [-o <output_file>]\n\
\n", xlfgenNameStr);
	printf("\
    XLF Append: (All XLF alignments [@align] will be removed)\n\
       %-7s --append <xlf_file> -r <[magic:]rpdout_file> |\n\
%35s-b <[magic:]blob_file>   |\n\
%35s-i <[magic:]input_file[@align]>\n\
\n", xlfgenNameStr,
	       "",
	       "");

	printf("\
    XLF Splitter:\n\
       %-7s -s <xlf_file>\n\
       %-7s -s -b <input_blob_lm> <header_file> <output_file>\n\
\n", xlfgenNameStr, xlfgenNameStr);
	printf("\
Notes:\n\
If -o is omitted (XLF and blob LM), the outfile is named according to the lmid string.\n\
If -l is omitted, the outfile is assigned the default lmid \"DEFAULT_R1A01\"\n\
If -t is omitted, the outfile is assigned the default type \"au_boot\".\n\
");
	printf("\
If magic is omitted, defaults will be used (-r 0x%08x, -b 0x%08x, -i 0x%08x).\n",
	       XLF_IBOOT_WPR_RPDOUT_MAGIC,
	       XLF_IBOOT_WPR_BLOB_MAGIC,
	       XLF_IBOOT_WPR_APPLIC_MAGIC);
	printf("\
If align is specified it will be rounded upwards to the nearest multiple\n \
of 4 bytes to a maximum value of 65532.\n");
	printf("\
WARNING: If [@align] is used in input XLF, --append will skip all [@align].\n\
Use --append only for XLF files that are built without [@align].\n");
	exit(exitCode);
}

/******************************************************************************
 *
 * Local function:
 *      printFileErrorAndTerminate
 *
 * Parameters:
 *      fp        - File pointer
 *      fileName  - File name
 *      xlfData   - Pointer to a struct holding information about the XLF.
 *      line      - Line of code that calls the function
 *      op        - Operation performed on the file
 *
 * Description:
 *      Prints an error message, closes files and exits.
 *
 *  Side effects:
 *      Closes open files and exit
 *
 *****************************************************************************/
static void printFileErrorAndTerminate(FILE *fp,
                                       char *fileName,
                                       xlfDataS_t **xlfData,
                                       unsigned int line,
                                       int op)
{
	fprintf(stderr,
	        "ERROR> An error (%d) occured while %s file '%s' (line %u)",
	        ferror(fp),
	        operationStr[op],
	        fileName,
	        line);

	closeFiles(xlfData);
	exit(1);

	return;
}

/******************************************************************************
 *
 * Local function:
 *      readFromFile
 *
 * Parameters:
 *      ptr       - Pointer to an already allocated buffer
 *      size      - Size to read from current file pointer location to the buffer
 *      fp        - File pointer adjusted to the right location to be read
 *      fileName  - File name
 *      xlfData   - Pointer to a struct holding information about the XLF.
 *      line      - Line of code that calls the function
 *
 * Description:
 *      Reads requested size from current file pointer location to the buffer
 *
 *****************************************************************************/
static void readFromFile(void *ptr,
                         size_t size,
                         FILE *fp,
                         char *fileName,
                         xlfDataS_t **xlfData,
                         unsigned int line)
{
	if ((fread(ptr, size, 1, fp)) != 1)
	{
		printFileErrorAndTerminate(fp,
		                           fileName,
		                           xlfData,
		                           line,
		                           READ_FILE);
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      writeToFile
 *
 * Parameters:
 *      ptr       - Pointer to an already filled buffer
 *      size      - Size to write from ptr to current file pointer location
 *      fp        - File pointer adjusted to the right location to be written to
 *      fileName  - File name
 *      xlfData   - Pointer to a struct holding information about the XLF.
 *      line      - Line of code that calls the function
 *
 * Description:
 *      Writes requested size from the ptr to the current file pointer location
 *
 *****************************************************************************/
static void writeToFile(void *ptr,
                        size_t size,
                        FILE *fp,
                        char *fileName,
                        xlfDataS_t **xlfData,
                        unsigned int line)
{
	if ((fwrite(ptr, size, 1, fp)) != 1)
	{
		printFileErrorAndTerminate(fp,
		                           fileName,
		                           xlfData,
		                           line,
		                           WRITE_FILE);
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      printError
 *
 * Parameters:
 *      fileName - File name
 *      op       - Operation performed on the file
 *      line     - Line of code that calls the function
 *
 * Description:
 *      Prints an error when an operation fails.
 *
 *****************************************************************************/
static void printError(char *fileName, int op, unsigned int line)
{
	fprintf(stderr,
	        "ERROR> An error occured while %s file '%s' (line %u) : %s",
	        operationStr[op],
	        fileName,
	        line,
	        strerror(errno));
}

/******************************************************************************
 *
 * Local function:
 *      fileSize
 *
 * Parameters:
 *      fp              File handling pointer
 *
 * Description:
 *      Returns the total size of a file
 *
 *  Side effects:
 *
 *****************************************************************************/
static long fileSize(FILE *fp)
{
	long size, old = ftell(fp);
	fseek(fp,0,SEEK_END);
	size = ftell(fp);
	fseek(fp,old,SEEK_SET);
	return(size);
}

/******************************************************************************
 *
 * Local function:
 *      append
 *
 * Parameters:
 *        offset     - The offset of the RPDOUT-file in target RAM.
 *        size       - The size of the RPDOUT-file.
 *        memList    - Pointer to the linked list.
 *        xlfData    - Pointer to the struct that holds information
 *                     about the XLF.
 * Description:
 *      Appends a new memory status element to the memList memory list
 *
 *  Side effects:
 *
 *****************************************************************************/
static void append(U32 offset,
                   U32 size,
                   XlfMemElemS_t **memList,
                   xlfDataS_t **xlfData)
{
	XlfMemElemS_t *memListTmp = *memList;

	if (*memList != NULL)
	{
		while ((*memList)->nextElem)
		{
			*memList = (*memList)->nextElem;
		}

		if (mallocVerbose((void**)&((*memList)->nextElem),
		                  sizeof(XlfMemElemS_t), __LINE__))
		{
			closeFiles(xlfData);
			exit(1);
		}

		*memList = (*memList)->nextElem;

		(*memList)->offset = offset;
		(*memList)->size = size;
		(*memList)->nextElem = NULL;

		/* Restore the pointer to the list's first element. */
		*memList = memListTmp;
		return;
	}
	else
	{
		if (mallocVerbose((void**)&memList,
		                  sizeof(XlfMemElemS_t), __LINE__))
		{
			closeFiles(xlfData);
			exit(1);
		}
		(*memList)->offset = offset;
		(*memList)->size = size;
		(*memList)->nextElem = NULL;
		return;
	}
}

/******************************************************************************
 *
 * Local function:
 *      fileCalcsum
 *
 * Parameters:
 *        size       - The number of bytes to include in the checksum.
 *        xlfData    - Pointer to the struct that holds information
 *                     about the XLF.
 * Return value:
 *      The checksum(U32).
 *
 * Description:
 *      Calculates the checksum based on 'size' number of bytes in the
 *      xlfFile.
 *
 *****************************************************************************/
static U32 fileCalcsum(U32 size, xlfDataS_t **xlfData)
{
	U32 sum = 0;
	U8 rv;

	while (size--)
	{
		readFromFile((void *) &rv,
		             sizeof(rv),
		             (*xlfData)->xlfFile.ptr,
		             (*xlfData)->xlfFile.name,
		             xlfData,
		             __LINE__);

		sum += (U32)rv;
		if (sum > 0xffffL)
		{
			sum++;
			/* Mask out MSB */
			sum &= 0xffffL;
		}
	}
	return sum;
}

/******************************************************************************
 *
 * Local function:
 *      calcSum
 *
 * Parameters:
 *        p          - Pointer to the area for which the checksum is calculated
 *        size       - Size of the area for which the checksum is calculated.
 *
 * Return value:
 *      The checksum (U32).
 *
 * Description:
 *      Calculates the checksum based on 'size' number of bytes in the
 *      file pointed to by fp.
 *
 *****************************************************************************/
static U32 calcSum(U8 *p, U32 size)
{
	U32 sum = 0;

	while (size--)
	{
		sum += (U32)*p++;
		if (sum > 0xffffL)
		{
			sum++;
			/* Mask out MSB */
			sum &= 0xffffL;
		}
	}
	return sum;
}

/******************************************************************************
 *
 * Local function:
 *      getCmdLineFlag
 *
 * Parameters:
 *        cmdLineArg - Pointer to the command line argument.
 *
 * Return value:
 *      An index that corresponds to the found command flag.
 *
 * Description:
 *      Returns the corresponding index for a command line argument.
 *
 *****************************************************************************/
static U8 getCmdLineFlag(char *cmdLineArg)
{
	struct CmdLineFlag
	{
		U8 index;
		char *str;
	} cmdLineFlag[] =
		  {
			  {INPUT_FLAG,        "-i"},
			  {RPDOUT_FLAG,       "-r"},
			  {OUTPUT_FLAG,       "-o"},
			  {XPL_FLAG,          "-x"},
			  {BLOB_FLAG,         "-b"},
			  {TYPE_FLAG,         "-t"},
			  {AUBOOT_INI_FLAG,   "-au_boot_ini"},
			  {XLF_LMID_FLAG,     "-lmid"},
			  {XLF_L_FLAG,        "-l"},
			  {VERBOSE_FLAG,      "-v"},
			  {VERSION_FLAG,      "-V"},
			  {SPLIT_FLAG,        "-s"},
			  {DEFINE_FLAG,       "-D"},
			  {NO_FLAG,           ""}
		  };
	U8 i = 0;

	while (cmdLineFlag[i].index != NO_FLAG)
	{
		if (strcmp(cmdLineFlag[i].str, (char *)cmdLineArg) == 0)
		{
			return cmdLineFlag[i].index;
		}
		i++;
	}

	return NO_FLAG;
}

/******************************************************************************
 *
 * Local function:
 *      storeSigBufs
 *
 * Parameters:
 *        char **argv, xlfDataS_t **xlfData, int argc
 *
 * Return value:
 *      no
 *
 * Description:
 *   get the sig buf values from input command
 *
 *****************************************************************************/
static void storeSigBufs(char **argv, xlfDataS_t **xlfData, int argc)
{
	char *str = NULL;
	char *end;
	if ((*xlfData)->cmdLineArgNo >= argc -1)
	{
		goto Err;
	}
	(*xlfData)->cmdLineArgNo ++;
	str = argv[(*xlfData)->cmdLineArgNo];
	if (!strncmp(str, SIG_BUF_CONFIGURATION_SIGN,
	             sizeof(SIG_BUF_CONFIGURATION_SIGN) - 1))
	{
		str = str + strlen(SIG_BUF_CONFIGURATION_SIGN) - 1;
		do
		{
			if ((*xlfData)->sigBufCon.numOfValues == SIG_BUF_RECONFIG_ALLOWED_NUMBERS)
			{
				goto Err;
			}
			str++;
			(*xlfData)->sigBufCon.value[(*xlfData)->sigBufCon.numOfValues] = strtoul(str, &end, 10);
			if ((*xlfData)->sigBufCon.value[(*xlfData)->sigBufCon.numOfValues] == 0 ||
			    (*xlfData)->sigBufCon.value[(*xlfData)->sigBufCon.numOfValues] > USHRT_MAX ||
			    (*end != ',' && *end != '\0'))
			{

				goto Err;
			}
			(*xlfData)->sigBufCon.numOfValues ++;
			str = end;
		} while (*str != '\0');
		if ((*xlfData)->sigBufCon.numOfValues < SIG_BUF_RECONFIG_ALLOWED_NUMBERS)
		{
			goto Err;
		}
	}
	else
	{
		goto Err;
	}
	return;
Err:
	fprintf(stderr, "ERROR> '%s %s': not a valid input, please input with seven sig buf values "\
	        "between <0> and <65535>, the input should be like this : "\
	        "-D krn/buffer_sizes=<size1>,<size2>,<size3>,<size4>,<size5>,<size6>,<size7>\n",
	        argv[(*xlfData)->cmdLineArgNo - 1], argv[(*xlfData)->cmdLineArgNo]);
	exit(1);
}

/******************************************************************************
 *
 * Local function:
 *      traverseCmdLineGeneratorFlags
 *
 * Parameters:
 *      initArgN   - The argv index that it should begin parsing from
 *      argc       - Number of command line arguments.
 *      argv       - The argument vector.
 *      xlfData    - Pointer to the struct that holds information
 *                   about the XLF.
 *
 * Description:
 *      Parses the command line and executes the directives for XLF generator.
 *      Stores results in xlfData
 *
 *  Side effects:
 *
 *****************************************************************************/
static void traverseCmdLineGeneratorFlags(int initArgN,
                                          int argc,
                                          char **argv,
                                          xlfDataS_t **xlfData)
{
	U8 cmdLineFlag = NO_FLAG;
	/* For generator */
	/* Loop through the arguments of the command-line in */
	/* order to get the input data to 'xlf_gen'. */
	for ((*xlfData)->cmdLineArgNo = initArgN;
	     (*xlfData)->cmdLineArgNo < argc;
	     (*xlfData)->cmdLineArgNo++) /*lint !e574*/
	{
		cmdLineFlag = getCmdLineFlag(argv[(*xlfData)->cmdLineArgNo]);
		switch (cmdLineFlag)
		{
		case INPUT_FLAG:
			storeFileInfo(XLF_APPLIC_FILE, argv, xlfData, argc);
			break;

		case RPDOUT_FLAG:
			storeFileInfo(XLF_RPDOUT_FILE, argv, xlfData, argc);
			break;

		case BLOB_FLAG:
			storeFileInfo(XLF_BLOB_FILE, argv, xlfData, argc);
			break;

		case OUTPUT_FLAG:
			/* Save the name of the output-file */
			mallocAndCopyString(&((*xlfData)->xlfFile.name),
			                    argv[(*xlfData)->cmdLineArgNo + 1]);
			(*xlfData)->outputFileSpecified = YES;
			/* Move on to the next command-line flag */
			(*xlfData)->cmdLineArgNo += 1;
			break;

		case XPL_FLAG:
			/* Save the name of the XPL-file */
			mallocAndCopyString(&((*xlfData)->xplFile.name),
			                    argv[(*xlfData)->cmdLineArgNo + 1]);
			(*xlfData)->xplSpecified = YES;
			/* Move on to the next command-line flag */
			(*xlfData)->cmdLineArgNo += 1;
			break;

		case TYPE_FLAG:
			/* Save the type of the XLF to be generated */
			if ((strcmp("au_applic", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_AU_APPLIC;
			}
			else if ((strcmp("au_boot", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_AU_BOOT;
			}
			else if ((strcmp("boot", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_BOOT;
			}
			else if ((strcmp("au_boot_linux", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_AU_BOOT_LINUX;
			}
			else if ((strcmp("au_applic_linux", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_AU_APPLIC_LINUX;
			}
			else if ((strcmp("au_env", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_AU_ENV;
			}
			else if ((strcmp("boot_env", argv[(*xlfData)->cmdLineArgNo + 1])) == 0)
			{
				(*xlfData)->type = XLF_IBOOT_HDR_BOOT_ENV;
			}
			else
			{
				fprintf(stderr,
				        "ERROR> '%s': not a valid argument to '-t' flag.\n ",
				        argv[(*xlfData)->cmdLineArgNo + 1]);
				printUsageAndDie(1);
			}

			/* Move on to the next command-line flag */
			(*xlfData)->cmdLineArgNo += 1;
			break;

		case XLF_LMID_FLAG:
		case XLF_L_FLAG:
			if (strlen(argv[(*xlfData)->cmdLineArgNo + 1]) > LMID_MAX_STRING_LENGTH)
			{
				fprintf(stderr,
				        "ERROR> The LMID-string provided is more than 32 characters long.\n");
				printUsageAndDie(1);
			}
			/* Store the user provided LMID */
			strcpy((*xlfData)->lmid, argv[(*xlfData)->cmdLineArgNo + 1]);
			(*xlfData)->lmidPresent = YES;
			/* Move on to the next command-line flag */
			(*xlfData)->cmdLineArgNo += 1;
			break;

		case AUBOOT_INI_FLAG:
			/* Set the seqNo-field in iboot header to 0x00000000
			**  since it is the first auboot-file.
			*/
			(*xlfData)->aubootIni = YES;
			/* Move on to the next command-line flag */
			/*(*xlfData)->cmdLineArgNo += 1;*/
			break;

		case VERBOSE_FLAG:
			(*xlfData)->verboseMode = YES;
			break;

		case VERSION_FLAG:
			fprintf(stdout,
			        "%s version: %s\n",
			        xlfgenNameStr, XLF_GEN_VERSION_STRING);
			exit(0);
			break;

		case SPLIT_FLAG:
			fprintf(stderr, "ERROR> -s is used for XLF splitter.\n");
			printUsageAndDie(1);
			break;

		case DEFINE_FLAG:
			storeSigBufs(argv, xlfData, argc);
			break;

		default:
			fprintf(stderr, "ERROR> '%s' is not a valid flag \n", argv[(*xlfData)->cmdLineArgNo]);
			printUsageAndDie(1);
			break;
		} /* switch (cmdLineFlag) */
	}
	return;
}

/******************************************************************************
 *
 * Local function:
 *      parseCmdLine
 *
 * Parameters:
 *        argc       - Number of command line arguments.
 *        argv       - The argument vector.
 *        xlfData    - Pointer to the struct that holds information
 *                     about the XLF.
 *
 * Description:
 *      Parses the command line and executes the directives.
 *      Stores results in xlfData
 *
 *  Side effects:
 *
 *****************************************************************************/
static void parseCmdLine(int argc, char **argv, xlfDataS_t **xlfData)
{
	initXlfData(xlfData);

	/* For splitter */
	if (strcmp(argv[1], "-s") == 0)
	{
		(*xlfData)->splitMode = YES;
		if (argc == 3)
		{
			(*xlfData)->xlfFile.name = argv[2];
			return;
		}
		else if (argc == 6)
		{
			if (strcmp(argv[2], "-b"))
			{
				printUsageAndDie(1);
			}
			(*xlfData)->splitType = XLF_BLOB_FILE;
			(*xlfData)->xlfFile.name = argv[3];
			return;
		}
		else
		{
			goto error;
		}
	}

	/* For LM gen */
	if (strcmp(argv[1], "--lm") == 0)
	{
		if(argc != 4 && argc != 6)
		{
			goto error;
		}

		if (argc == 6)
		{
			if (!strcmp(argv[4], "-o"))
			{
				(*xlfData)->xlfFile.name = argv[5];
				(*xlfData)->outputFileSpecified = YES;
			}
			else
			{
				printUsageAndDie(1);
			}
		}

		/* Parse header file */
		printf("Generating lm file from %s:\n", argv[2]);
		parseLmHeaderFile(argv[2], xlfData);

		/* Assign input file */
		(*xlfData)->inputFile[0].name = argv[3];
		(*xlfData)->inputFileCount = 1;

		/* Make it always verbose */
		(*xlfData)->verboseMode = YES;

		return;
	}

	/* For append mode */
	/* The implementation of this option gives the ability to manipulate
	 * the XLF file significantly. Limitations on the input arguments
	 * prevent users to misuse it.
	 */
	if (strcmp(argv[1], "--append") == 0)
	{
		if (argc != 5)
		{
			goto error;
		}

		if (strcmp(argv[3], "-r") & strcmp(argv[3], "-b") & strcmp(argv[3], "-i"))
		{
			fprintf(stderr, "ERROR> invalid --append argument (%s).\n",
			        argv[3]);
			printUsageAndDie(1);
		}

		(*xlfData)->manipulateMode = APPEND_MODE;
		(*xlfData)->splitMode = YES;
		(*xlfData)->outputFileSpecified = YES;
		(*xlfData)->xlfFile.name = argv[2];
		printf("Append %s to the end of %s\n", argv[4], argv[2]);
		return;
	}

	/* Check XLF generator */
	traverseCmdLineGeneratorFlags(1, argc, argv, xlfData);
	return;

error:
	fprintf(stderr, "ERROR> wrong number of arguments (%u).\n",
	        argc);
	printUsageAndDie(1);
}

/******************************************************************************
 *
 * Local function:
 *      reallocXlfData
 *
 * Parameters:
 *      xlfData        - Pointer to the struct that holds information
 *                       about the XLF.
 *      inputFileCount - Number of files that are going to be stored in xlfData
 *
 * Description:
 *      realloc XlfData according to number of input files
 *
 *  Side effects:
 *      (*xlfData)->inputFile[] array size will be equal to
 *      (*xlfData)->inputFileCount
 *
 *****************************************************************************/
static void reallocXlfData(xlfDataS_t **xlfData, int inputFileCount)
{
	/*
	** Allocate memory to hold pointers to files
	** if there are more than one.
	*/
	if (inputFileCount > 1)
	{
		*xlfData = (xlfDataS_t *)realloc(*xlfData,
		                                 (sizeof(xlfDataS_t) +
		                                  (((inputFileCount)  -  1) *
		                                   sizeof(FileRec_t))));
	}

	if (*xlfData == NULL)
	{
		fprintf(stderr, "ERROR> 'realloc' failed, errno: %d\n", errno);
		perror("ERROR> Failed to 'realloc' xlfData");
		exit(1);
	}
	return;
}

/******************************************************************************
 *
 * Local function:
 *        prodDateTime
 *
 * Parameters:
 *        None.
 *
 * Return value:
 *        Local date and time to be stored in an unsigned int. Bits are:
 *        31-24: The number of years since 1900
 *        23-16: Month
 *        15-8 : Day of the month
 *        7-0  : Hour in Day
 *
 * Description:
 *        Returns date and time to be interpreted on target
 *
 *****************************************************************************/
static unsigned int prodDateTime(void)
{
	struct tm *locTime;
	time_t calTime;

	calTime = time((time_t *)NULL);
	locTime = localtime(&calTime);
	return (((locTime->tm_year) << 24) | /*lint !e701*/
	        ((locTime->tm_mon) << 16) |  /*lint !e701*/
	        ((locTime->tm_mday) << 8) |  /*lint !e701*/
	        (locTime->tm_hour));
}

/******************************************************************************
 *
 * Local function:
 *        mallocVerbose
 *
 * Parameters:
 *        ptr   - Pointer to a buffer that should be assigned
 *        size  - Size to be assigned
 *        line  - Line number the function is called from (__LINE__)
 *
 * Description:
 *        Allocates size bytes and set ptr to the allocated memory
 *
 * Return value:
 *        0 if succesful, 1 otherwise
 *
 * Side effects:
 *        Set errno to 0.
 *
 *****************************************************************************/
static U32 mallocVerbose(void **ptr, U32 size, U32 line)
{
	char errMsg[BUFF_SIZE];
	*ptr = malloc((size_t) size);

	if (*ptr == NULL)
	{
		snprintf(errMsg, BUFF_SIZE, "ERROR> 'malloc' failed at line %u.",
		         line);
		perror(errMsg);
		errno = 0;
		return 1;
	}

	return 0;
}

/******************************************************************************
 *
 * Local function:
 *        mallocAndCopyString
 *
 * Parameters:
 *        ptr   - Pointer to a char* that should be assigned
 *        str   - String that may be copied into the allocated space
 *
 * Description:
 *        Allocates strlen(str) + 1 bytes to the ptr and copy str to it.
 *
 * Side effects:
 *
 *****************************************************************************/
static void mallocAndCopyString(char **ptr, const char *str)
{
	int len = 0;

	if (str)
	{
		len = strlen(str);
	}

	if (mallocVerbose((void **)ptr, len + 1, __LINE__))
	{
		exit(1);
	}

	if (str)
	{
		strcpy(*ptr, str);
	}
	else
	{
		**ptr = 0;
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *        mallocAndConcatStrings
 *
 * Parameters:
 *        ptr   - Pointer to a char* that should be assigned
 *        str1  - String that may be copied into the allocated space
 *        str2  - String that may be concatenate into the allocated space
 *
 * Description:
 *        Allocates enough bytes to the ptr, copy str1 and concatenate
 *        str2 to it.
 *
 * Side effects:
 *
 *****************************************************************************/
static void mallocAndConcatStrings(char **ptr, const char *str1, const char *str2)
{
	int len = 0;

	if (str1)
	{
		len = strlen(str1);
	}

	if (str2)
	{
		len += strlen(str2);
	}

	if (mallocVerbose((void **)ptr, len + 1, __LINE__))
	{
		exit(1);
	}

	**ptr = 0;

	if (str1)
	{
		strcpy(*ptr, str1);
	}

	if (str2)
	{
		strcat(*ptr, str2);
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      getBlobType
 *
 * Parameters:
 *      blobTypeArg - Pointer to the blob argument.
 *
 * Return value:
 *      The value that corresponds to the found blob type.
 *
 * Description:
 *      Returns the corresponding index for a blob type argument.
 *
 *****************************************************************************/
static U32 getBlobType(char *blobTypeArg)
{
	U32 i;

	for (i = 0; i < IBOOT_XlfBlobTypeE_LAST; i++)
	{
		if (!strcmp(blobTypeStr[i], blobTypeArg))
		{
			break;
		}
	}

	return i;
}

/******************************************************************************
 *
 * Local function:
 *      getBlobTypeString
 *
 * Parameters:
 *      blobType - BlobType field in big endian
 *
 * Return value:
 *      Pointer to the blob type string
 *
 * Description:
 *      Returns the corresponding string pointer for a blob type.
 *
 *****************************************************************************/
static const char *getBlobTypeString(U32 blobType)
{
#ifdef __linux__
	/* Set correct endian. big endian in file. */
	blobType = be32toh(blobType);
#endif

	if (blobType < IBOOT_XlfBlobTypeE_LAST)
	{
		return blobTypeStr[blobType];
	}

	return "";
}

/******************************************************************************
 *
 * Local function:
 *      PrintLmVersionError
 *
 * Parameters:
 *      None.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Prints LM version format is wrong and terminates the program.
 *
 *****************************************************************************/
static void printLmVersionError()
{
	fprintf(stderr,
	        "ERROR> LM version should be in %s <major>.<minor> format.\n",
	        lmHeaderTagStr[2]);
	exit(1);
}

/******************************************************************************
 *
 * Local function:
 *      setLmVersion
 *
 * Parameters:
 *      data     - String that is exported from LM header file
 *      MajorVer - Pointer to major version in header to be set
 *      MinorVer - Pointer to minor version in header to be set
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Set major and minor version based on data.
 *
 *****************************************************************************/
static void setLmVersion(char *data,
                         unsigned int *majorVer,
                         unsigned int *minorVer)
{
	char *endPtr;

	*majorVer = (unsigned int)strtoul(data, &endPtr, 10);
	if (*endPtr == '.')
	{
		endPtr = endPtr + 1;
	}
	else
	{
		printLmVersionError();
	}

	*minorVer = (unsigned int)strtoul(endPtr, &endPtr, 10);
	if (*endPtr || (endPtr != data + strlen(data)))
	{
		printLmVersionError();
	}

	return;
}


/******************************************************************************
 *
 * Local function:
 *      exportTagDataFromFile
 *
 * Parameters:
 *      lmHdrFilePtr  - Already opened header file pointer
 *      tag           - Pointer to store tag
 *      data          - Pointer to store tag data
 *      lmHdrFileName - Name (path) of the LM header file
 *
 * Return value:
 *      EOF if file ends, else returns the number of input items
 *      successfully matched and assigned
 *
 * Description:
 *      Exports next tag and data from a file. Comment lines should
 *      start with '#'. In-line comment is not suppoerted.
 *
 *****************************************************************************/
static int exportTagDataFromFile(FILE *lmHdrFilePtr,
                                 char *tag,
                                 char *data,
                                 char *lmHdrFileName)
{
	int n;
	int comment;

	/* Get Tag */
	do
	{
		comment = 0;
		n = fscanf(lmHdrFilePtr, "%s", tag);

		if (n == EOF)
		{
			if (errno)
			{
				printError(lmHdrFileName, READ_FILE, __LINE__);
				exit(1);
			}

			*tag = 0;
			return n;
		}

		if (*tag == '#')
		{
			comment = 1;
			n = fscanf(lmHdrFilePtr, "%[^\n]", tag);
			if (n == EOF && errno)
			{
				printError(lmHdrFileName, READ_FILE, __LINE__);
				exit(1);
			}
		}
	}
	while (comment);

	/* Get Data */
	if (n != EOF)
	{
		n = fscanf(lmHdrFilePtr, "%s[^\n]", data);

		if (n == EOF)
		{
			if (errno)
			{
				printError(lmHdrFileName, READ_FILE, __LINE__);
				exit(1);
			}
			fprintf(stderr,
			        "ERROR> EOF reached before any data for"
			        " tag '%s' in %s found.\n",
			        tag, lmHdrFileName);
			*data = 0;
			return n;
		}
	}

	/* Print what is read */
	printf("\t%-40s%s\n", tag, data);

	return n;
}

/******************************************************************************
 *
 * Local function:
 *      createBlobHeader
 *
 * Parameters:
 *      headerFilePtr - Already opened blob header file pointer
 *      xlfData       - Pointer to the struct that holds information
 *                      about the XLF.
 *      lmHdrFileName - Name (path) of the LM header file
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Creates and populates 'IBOOT_XlfBlobHeaderS_t' and sets
 *      (*xlfData)->lmHeaderPtr to use it.
 *
 * Side effects:
 *      Blob header will be created and xlfData will be updated.
 *
 *****************************************************************************/
static void createBlobHeader(FILE *headerFilePtr,
                             xlfDataS_t **xlfData,
                             char *lmHdrFileName)
{
	char tag[BUFF_SIZE], data[BUFF_SIZE];
	IBOOT_XlfBlobHeaderS_t *blobHeader;
	int versionNotDefined = 1;

	/* Allocate memory for IBOOT_XlfBlobHeaderS_t and update *xlfData */
	(*xlfData)->lmHeaderType = XLF_BLOB_FILE;
	(*xlfData)->lmHeaderPtr = malloc(sizeof(IBOOT_XlfBlobHeaderS_t));

	blobHeader = (IBOOT_XlfBlobHeaderS_t *) (*xlfData)->lmHeaderPtr;

	/* Initialize Header */
	blobHeader->crc32 = 0xFFFFFFFF;
	blobHeader->headerSize = sizeof(IBOOT_XlfBlobHeaderS_t) -
		offsetof(IBOOT_XlfBlobHeaderS_t, headerSize);
	blobHeader->type = IBOOT_XlfBlobTypeE_LAST + 1;
	blobHeader->majorVer = 0;
	blobHeader->minorVer = 0;
	memset(blobHeader->name,
	       0,
	       XLF_IBOOT_BLOB_NAME_LENGTH);
	memset(blobHeader->suid,
	       0,
	       XLF_IBOOT_HDR_SUID_LEN);

	blobHeader->prodDateTime = prodDateTime();

	/* Decode and store blob parameters */
	do
	{
		if (exportTagDataFromFile(headerFilePtr, tag, data, lmHdrFileName) == EOF)
		{
			break;
		}

		if ((blobHeader->type >
		     IBOOT_XlfBlobTypeE_LAST) &&
		    !strcmp(tag, lmHeaderTagStr[1]))
		{
			/* Type */
			blobHeader->type = getBlobType(data);
			if (blobHeader->type >=
			    IBOOT_XlfBlobTypeE_LAST)
			{
				fprintf(stderr,
				        "ERROR> %s is not set to a valid blob type (%s).\n",
				        lmHeaderTagStr[1], data);
				exit(1);
			}
		}
		else if (versionNotDefined &&
		         !strcmp(tag, lmHeaderTagStr[2]))
		{
			/* Version */
			setLmVersion(data,
			             &blobHeader->majorVer,
			             &blobHeader->minorVer);
			versionNotDefined = 0;
		}
		else if ((blobHeader->suid[0] == '\0')&&
		         !strcmp(tag, lmHeaderTagStr[3]))
		{
			/* SUID */
			generateSuid(xlfData,
			             blobHeader->suid,
			             data);
			strncpy((*xlfData)->lmid,
			        data,
			        LMID_MAX_STRING_LENGTH);
			(*xlfData)->lmid[LMID_MAX_STRING_LENGTH] = '\0';
			(*xlfData)->lmidPresent = YES;
		}
		else if ((blobHeader->name[0] == '\0') &&
		         !strcmp(tag, lmHeaderTagStr[4]))
		{
			/* name */
			strncpy(blobHeader->name,
			        data,
			        XLF_IBOOT_BLOB_NAME_LENGTH - 1);
		}
		else
		{
			/* error */
			fprintf(stderr,
			        "ERROR> %s is not a valid blob argument or duplicated.\n",
			        tag);
			exit(1);
		}
	} while (1);

	/* Check mandatory parameters are set */
	if (blobHeader->type >=
	    IBOOT_XlfBlobTypeE_LAST)
	{
		fprintf(stderr,
		        "ERROR> %s is mandatory in %s.\n",
		        lmHeaderTagStr[1],
		        lmHdrFileName);
		exit(1);
	}

	if (versionNotDefined)
	{
		/* Set default major version. */
		blobHeader->majorVer = 1;
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      parseLmHeaderFile
 *
 * Parameters:
 *      lmHdrFileName - Name (path) of the LM header file
 *      xlfData       - Pointer to the struct that holds information
 *                      about the XLF.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Open and check LM header file type and call appropriate function to
 *      parse the rest of the header file and create requested header format.
 *
 * Side effects:
 *
 *****************************************************************************/
static void parseLmHeaderFile(char *lmHdrFileName, xlfDataS_t **xlfData)
{
	FILE *headerFilePtr;
	char tag[BUFF_SIZE], data[BUFF_SIZE];

	/* Open the binary XPL-file for reading */
	headerFilePtr = fopen(lmHdrFileName, "rb");
	if (headerFilePtr == NULL)
	{
		printError(lmHdrFileName, OPEN_FILE, __LINE__);
		exit(1);
	}

	printf ("\t%-40s%s\n", "\"tag:\"", "\"data\"");

	if (exportTagDataFromFile(headerFilePtr, tag, data, lmHdrFileName) != EOF
	    && !strcmp(tag, lmHeaderTagStr[0]))
	{
		if (!strncmp(data, blobHeaderFormatStr, 4))
		{
			createBlobHeader(headerFilePtr, xlfData, lmHdrFileName);
		}
		else
		{
			fprintf(stderr,
			        "ERROR> %s is not a supported %s\n",
			        data, lmHeaderTagStr[0]);
			exit(1);
		}
	}
	else
	{
		fprintf(stderr,
		        "ERROR> %s has to be the first entry in %s\n",
		        lmHeaderTagStr[0], lmHdrFileName);
		exit(1);
	}

	if (fclose(headerFilePtr) != 0)
	{
		printError(lmHdrFileName, CLOSE_FILE, __LINE__);
		exit(1);
	}

	printf("\n");

	return;
}

/******************************************************************************
 *
 * Local function:
 *      storeFileInfo
 *
 * Parameters:
 *        fileType   - RPDOUT or APPLIC
 *        argv       - The argument vector
 *        argc       - The argument count
 *        xlfData    - Pointer to the struct that holds information
 *                     about the XLF.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Stores info such as name, type and file pointer about the input
 *      files given on the command line.
 *
 * Side effects:
 *
 *****************************************************************************/
static void storeFileInfo(U8 fileType, char **argv, xlfDataS_t **xlfData, int argc)
{
	U16 i;
	U16 fileNo = 0;
	U16 alreadyStored = (*xlfData)->inputFileCount;
	char *mSep;                   /* Magic number and/or align separator */
	U16 align;

	/* Spin on cmdLineArg until next flag is found */
	while (getCmdLineFlag(argv[(*xlfData)->cmdLineArgNo+fileNo+1]) == NO_FLAG)
	{
		fileNo++;
		if ((*xlfData)->cmdLineArgNo+fileNo+1 >= argc)
			break;
	}

	if (!fileNo)
	{
		fprintf(stderr, "ERROR> %s requires at least one valid input file.\n",
		        argv[(*xlfData)->cmdLineArgNo]);
		printUsageAndDie(1);
	}

	(*xlfData)->inputFileCount += fileNo;

	reallocXlfData(xlfData, (*xlfData)->inputFileCount);

	/* Save the names and type of the files */
	for(i = 0;
	    i < fileNo;
	    i++)
	{
		align = 0;

		mSep = strstr(argv[(*xlfData)->cmdLineArgNo + i + 1],"@");

		if (mSep)
		{
			char *endPtr;
			unsigned long int align_tmp;
			align_tmp = strtoul(mSep+1, &endPtr, 0);
			if (*endPtr || fileType != XLF_APPLIC_FILE)
				align = 0;
			else
			{
				if (align_tmp > (U16)MAX_ALIGN)
					align = USHRT_MAX;
				else
					align = (U16)align_tmp;
				align = (((align - 1) | (ALIGN_BYTES - 1)) + 1);
			}
			*mSep=0;
		}
		(*xlfData)->inputFile[i + alreadyStored].align = align;

		mSep = strstr(argv[(*xlfData)->cmdLineArgNo + i + 1],":");

		if (mSep == NULL)
		{/* No magic -> Set default magic number and save file name.*/
			mallocAndCopyString(&((*xlfData)->inputFile[i + alreadyStored].name),
			                    argv[(*xlfData)->cmdLineArgNo + i + 1]);
			switch (fileType)
			{
			case XLF_RPDOUT_FILE:
				(*xlfData)->inputFile[i + alreadyStored].magic =
					XLF_IBOOT_WPR_RPDOUT_MAGIC;
				break;
			case XLF_BLOB_FILE:
				(*xlfData)->inputFile[i + alreadyStored].magic =
					XLF_IBOOT_WPR_BLOB_MAGIC;
				break;
			default:
				(*xlfData)->inputFile[i + alreadyStored].magic =
					XLF_IBOOT_WPR_APPLIC_MAGIC;
			}
		}
		else
		{/* Magic -> Convert and set magic number and save file name */
			mallocAndCopyString(&((*xlfData)->inputFile[i + alreadyStored].name), mSep+1);
			(*xlfData)->inputFile[i + alreadyStored].magic =
				strtoul(argv[(*xlfData)->cmdLineArgNo + i + 1], NULL, 0);
		}

		if (access((*xlfData)->inputFile[i + alreadyStored].name, R_OK) != 0)
		{
			fprintf(stderr, "ERROR> %s is not an available or readable file.\n",
			        (*xlfData)->inputFile[i + alreadyStored].name);
			restoreAndCleanUpFiles(xlfData);
			printUsageAndDie(1);
		}

		(*xlfData)->inputFile[i + alreadyStored].fileType = fileType;
	} /* for */

	(*xlfData)->cmdLineArgNo += fileNo;

	return;
}

/******************************************************************************
 *
 * Local function:
 *      initXlfData
 *
 * Parameters:
 *        xlfData    - Pointer to the struct that holds information
 *                     about the XLF.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Allocate memory for and initialises the xlfData-struct.
 *
 * Side effects:
 *
 *****************************************************************************/
static void initXlfData(xlfDataS_t **xlfData)
{
	int i;
	*xlfData = (xlfDataS_t *)realloc(*xlfData,
	                                 (sizeof(xlfDataS_t)));
	if (*xlfData == NULL)
	{
		fprintf(stderr, "ERROR> 'realloc' failed. errno = %d\n", errno);
		exit(1);
	}

	/* Default is non initial auboot */
	(*xlfData)->aubootIni = NO;
	/* Default is no verbose output */
	(*xlfData)->verboseMode = NO;
	(*xlfData)->inputFileCount = 0;
	(*xlfData)->cmdLineArgNo = 0;
	(*xlfData)->xplSpecified = NO;
	(*xlfData)->osPresent = 0;
	(*xlfData)->lmidPresent = NO;
	(*xlfData)->outputFileSpecified = NO;
	(*xlfData)->splitMode = NO;
	(*xlfData)->splitType = 0;
	(*xlfData)->manipulateMode = NO;
	(*xlfData)->tmpFolderName = NULL;
	(*xlfData)->lmHeaderType = 0;

	(*xlfData)->sigBufCon.numOfValues = 0;

	for (i = 0; i < IBOOT_XlfBlobTypeE_LAST; i++)
	{
		(*xlfData)->blobTypeExist[i] = 0;
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      dumpXlfData
 *
 * Parameters:
 *        xlfData    - Pointer to the struct holding information about
 *                     the XLF.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Dumps relevant information regarding the xlfif the -v (verbose-mode)
 *      flag is set.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
static void dumpXlfData(xlfDataS_t *xlfData)
{
	U16 i;

	if (xlfData->xplSpecified == YES)
	{
		fprintf(stdout,
		        "XPL file-name \t\t: %s\n", xlfData->xplFile.name);
	}

	for (i = 0; i < xlfData->inputFileCount; i++)
	{
		fprintf(stdout,
		        "Input file-name %d \t: %s", i, xlfData->inputFile[i].name);
		fprintf(stdout,"\n");
	}

	fprintf(stdout,
	        "Output file-name \t: %s\n", xlfData->xlfFile.name);
	return;
}

/******************************************************************************
 *
 * Local function:
 *      restoreAndCleanUpFiles
 *
 * Parameters:
 *      xlfData    - Pointer to the struct holding information about
 *                   the XLF.
 *
 * Return value:
 *      0 if ok, other values on error
 *
 * Description:
 *      Restore the original XLF file if exist and manipulateMode is not NO,
 *      and delete the temp folder if exist.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
static int restoreAndCleanUpFiles(xlfDataS_t **xlfData)
{
	int error = 0;

	/* If tmpFolderName exist */
	if ((*xlfData)->manipulateMode != DO_NOT_CLEAN && (*xlfData)->tmpFolderName)
	{
		char *tmpStr;
		/* Restore XLF file if exist and needed */
		if ((*xlfData)->manipulateMode != NO)
		{
			mallocAndConcatStrings(&tmpStr,
			                       (*xlfData)->tmpFolderName,
			                       originalXlfFileName);

			if (access(tmpStr, (R_OK|W_OK)) != -1)
			{
				error = rename(tmpStr,
				               (*xlfData)->xlfFile.name);
			}

			free(tmpStr);
		}

		/* Delete temp folder */
		(*xlfData)->tmpFolderName[strlen((*xlfData)->tmpFolderName)-1] = '\0';
		mallocAndConcatStrings(&tmpStr,
		                       "rm -rf ",
		                       (*xlfData)->tmpFolderName);
		if (system(tmpStr))
		{
			error = 1;
		}

		free(tmpStr);
	}

	return error;
}

/******************************************************************************
 *
 * Local function:
 *      openFiles
 *
 * Parameters:
 *        xlfData    - Pointer to the struct holding information about
 *                     the XLF.
 * Description:
 *      Opens the files with names given on the command line.
 *      If no xlf file name is provided the LMID-string will be set as name
 *      by default.
 *      Status is stored in xlfData
 *
 *  Side effects:
 *
 *****************************************************************************/
static void openFiles(xlfDataS_t **xlfData)
{
	U16 fileNo;
	unsigned int n;

	/* For split mode, open XLF binary file */
	if((*xlfData)->splitMode == YES)
	{
		(*xlfData)->xlfFile.ptr = fopen( (*xlfData)->xlfFile.name, "rb");

		if ((*xlfData)->xlfFile.ptr == NULL)
		{
			printError((*xlfData)->xlfFile.name, OPEN_FILE, __LINE__);
			exit(1);
		}
		return;
	}

	/*
	** For generate XLF
	** Open a new binary file for writing (any existing file
	** with the same name will be overwritten)
	*/
	if ((*xlfData)->outputFileSpecified == NO)
	{
		/* Set default name to the provided LMID if there is one*/
		if (((*xlfData)->lmidPresent) == YES)
		{
			if (mallocVerbose((void**)&((*xlfData)->xlfFile.name),
			                  strlen((*xlfData)->lmid), __LINE__))
			{
				goto error;
			}

			/*
			** Replace all '/' characters with '%' in the output file name
			** when the name source is the lmid string. This is done because
			** the file system can not handle '/' in filenames.
			*/
			for(n=0; n<strlen((*xlfData)->lmid)+1; n++)
			{
				if ((*xlfData)->lmid[n] == '/')
					(*xlfData)->xlfFile.name[n] = '%';
				else
					(*xlfData)->xlfFile.name[n] = (*xlfData)->lmid[n];
			}
		}
		else
		{
			fprintf(stderr,
			        "ERROR> No output file name or LMID specified.\n");
			restoreAndCleanUpFiles(xlfData);
			printUsageAndDie(1);
		}
	}

	/* Open the binary input-files for reading (*xlfData)->inputFileCount */
	for (fileNo = 0; fileNo < ((*xlfData)->inputFileCount); fileNo++)
	{
		(*xlfData)->inputFile[fileNo].ptr =
			fopen((*xlfData)->inputFile[fileNo].name,
			      "rb");

		if ((*xlfData)->inputFile[fileNo].ptr == NULL)
		{
			printError((*xlfData)->inputFile[fileNo].name, OPEN_FILE, __LINE__);
			goto error;
		}
	}

	if ((*xlfData)->xplSpecified == YES)
	{
		/* Open the binary XPL-file for reading */
		(*xlfData)->xplFile.ptr = fopen((*xlfData)->xplFile.name, "rb");
		if ((*xlfData)->xplFile.ptr == NULL)
		{
			printError((*xlfData)->xplFile.name, OPEN_FILE, __LINE__);
			goto error;
		}
	}

	(*xlfData)->xlfFile.ptr = fopen((*xlfData)->xlfFile.name, "w+b");
	if ((*xlfData)->xlfFile.ptr == NULL)
	{
		printError((*xlfData)->xlfFile.name, OPEN_FILE, __LINE__);
		goto error;
	}

	return;

error:
	restoreAndCleanUpFiles(xlfData);
	exit(1);
}

/******************************************************************************
 *
 * Local function:
 *      closeFiles
 *
 * Parameters:
 *        xlfData    - Pointer to the struct holding information about
 *                     the XLF.
 * Description:
 *      Closes the files named in xlfData. We do not free allocated
 *      memory since the program terminates after calling this function. OS
 *      will take care of the allocated memory.
 *
 *****************************************************************************/
static void closeFiles(xlfDataS_t **xlfData)
{
	int rv;
	U16 fileNo;
	U8 success = YES;

	rv = fflush(NULL);
	if (rv == EOF)
	{
		fprintf(stderr,
		        "ERROR> 'fflush()' failed\n");
		success = NO;
	}

	rv = fclose((*xlfData)->xlfFile.ptr);
	if (rv == EOF)
	{
		fprintf(stderr,
		        "ERROR> 'fclose()' xlfFile failed\n");
		success = NO;
	}

	if ((*xlfData)->xplSpecified == YES)
	{
		rv = fclose((*xlfData)->xplFile.ptr);
		if (rv == EOF)
		{
			fprintf(stderr,
			        "ERROR> 'fclose()' xplFile failed\n");
			success = NO;
		}
	}

	for (fileNo = 0; fileNo < (*xlfData)->inputFileCount; fileNo++)
	{
		rv = fclose((*xlfData)->inputFile[fileNo].ptr);

		if (rv == EOF)
		{
			fprintf(stderr,
			        "ERROR> 'fclose()' inputFile failed\n");
			success = NO;
		}
		if (!strcmp((*xlfData)->inputFile[fileNo].name, SIG_BUF_RECONFIGURED_LM))
		{
			remove(SIG_BUF_RECONFIGURED_LM);
		}
	}

	if (restoreAndCleanUpFiles(xlfData))
	{
		exit (1);
	}

	if (success == NO)
	{
		exit (2);
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      generateSuid
 *
 * Parameters:
 *        xlfData    - Struct holding info concerning the XLF generation.
 *        destSuid   - The to be generated SUID-string.
 *        srcSuid    - String to be converted to SUID-string.
 *
 * Return value:
 *      None.
 *
 * Description:
 *      Generates a SUID-formatted string from an LMID-formatted string.
 *
 * Side effects:
 *      None.
 *
 *****************************************************************************/
static void generateSuid(xlfDataS_t **xlfData, char* destSuid, char *srcSuid)
{
	U16 i;
	U16 charsCopied = 0;
	char *tmpLmid = srcSuid;

	while (*tmpLmid != '_' && *tmpLmid != '\0' )
	{
		*destSuid = *tmpLmid;
		destSuid++;
		tmpLmid++;
		charsCopied++;
	}

	if (*tmpLmid == '\0')
	{
		fprintf(stderr,"\
ERROR> The lmid (-l) MUST include an underscore '_' character between the \n\
product number and the revision number (Ex. CXC123456_R1A01).\n");
		closeFiles(xlfData);
		exit(1);
	}

	if (charsCopied > IBOOT_PROD_NUMBER_LENGTH)
	{
		fprintf(stderr,
		        "ERROR> The product number in the LMID-string must not be longer than 23 characters.\n");
		closeFiles(xlfData);
		exit(1);
	}

	for (i = 0; i < (IBOOT_PROD_NUMBER_LENGTH - charsCopied); i++)
	{
		*destSuid++ = ' ';
	}

	tmpLmid++;
	charsCopied = 0;
	while (*tmpLmid != '\0')
	{
		*destSuid++ = *tmpLmid++;
		charsCopied++;
	}

	if (charsCopied > IBOOT_REV_NUMBER_LENGTH)
	{
		fprintf(stderr,
		        "ERROR> The revision number in the LMID-string must not be longer than 9 characters.\n");
		closeFiles(xlfData);
		exit(1);
	}
	for (i = 0; i < (IBOOT_REV_NUMBER_LENGTH - charsCopied); i++)
	{
		*destSuid++ = ' ';
	}
	return;
}


/******************************************************************************
 *
 * Local function:
 *      initIbootHeader
 *
 * Parameters:
 *        xlfData    - Pointer to the struct holding information about the XLF.
 *        xlfIbootHdr - Pointer to the IBOOT-header,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 *
 * Description:
 *      Initialises the fields in the IBOOT-header.
 *
 *  Side effects:
 *      Modifies 'xlfIbootHdr'
 *
 *****************************************************************************/
static void initIbootHeader(xlfDataS_t **xlfData,
                            IBOOT_XlfIbootHeaderS_t **xlfIbootHdr)
{
	U16 fileCount;
	char suid[SUID_STRING_LENGTH];

	if (((*xlfData)->lmidPresent) == YES)
	{
		generateSuid(xlfData, suid, (*xlfData)->lmid);
	}
	else
	{
		fprintf(stdout,
		        "NOTE: No LMID-string provided, using default (DEFAULT_R1A01).\n");
		strncpy(suid, DEFAULT_SUID, SUID_STRING_LENGTH);
	}

	/* Determine the size needed for the IBOOT-header */
	(*xlfData)->xlfIbootHdrSize = (sizeof(IBOOT_XlfIbootHeaderS_t) +
	                               ((*xlfData)->inputFileCount - 1) *
	                               sizeof((*xlfIbootHdr)->fileOffset));

	/*
	** Allocate a struct large enough to contain
	** the IBOOT-header of the XPL to be generated
	*/
	*xlfIbootHdr = (IBOOT_XlfIbootHeaderS_t *)realloc(*xlfIbootHdr,
	                                                  (*xlfData)->xlfIbootHdrSize);
	if (*xlfIbootHdr == NULL)
	{
		fprintf(stderr,
		        "ERROR> 'realloc()' failed.\n");
		closeFiles(xlfData);
		exit(1);
	}

	/* Set the values of the XLF IBOOT-header */
	(*xlfIbootHdr)->magic = XLF_IBOOT_HDR_MAGIC;
	(*xlfIbootHdr)->type = (U32) (*xlfData)->type;
	(*xlfIbootHdr)->majorVersion = (U8) XLF_FORMAT_VERSION_MAJOR;
	(*xlfIbootHdr)->minorVersion = (U8) XLF_FORMAT_VERSION_MINOR;
	(*xlfIbootHdr)->spare1 = (U16) XLF_IBOOT_HDR_SPARE1;
	(*xlfIbootHdr)->prodDateTime =prodDateTime();

	strncpy((*xlfIbootHdr)->suid, suid, SUID_STRING_LENGTH);

	if ((*xlfData)->xplSpecified == YES)
	{
		(*xlfIbootHdr)->xplOffset = (*xlfData)->xlfIbootHdrSize;
	}
	else
	{
		(*xlfIbootHdr)->xplOffset = 0;
	}

	/*
	** crc2Offset and crc1 will be written when the IBOOT-footer
	** has been written
	*/
	(*xlfIbootHdr)->crc2Offset = 0;
	(*xlfIbootHdr)->crc1 = (U16) 0;
	(*xlfIbootHdr)->spare2 = (U16) XLF_IBOOT_HDR_SPARE2;

	(*xlfIbootHdr)->update = XLF_IBOOT_HDR_INIT_UPDATE;
	(*xlfIbootHdr)->seqNo = XLF_IBOOT_HDR_INIT_SEQNO;

	if ((*xlfData)->aubootIni == YES)
	{
		if (((*xlfIbootHdr)->type) == XLF_IBOOT_HDR_AU_BOOT_LINUX)
		{
			/* For Linux, AUBOOT partion is protected and thus sequence number need to correct from beginning, can not be updated during runtime */
			(*xlfIbootHdr)->update = XLF_IBOOT_HDR_INIT_UPDATE; /* XXX Should this be INI as well, ask Göran Nordin */
			(*xlfIbootHdr)->seqNo = XLF_IBOOT_HDR_INIT_SEQNO_INI;
		}
		else
		{
			printf("\
                 NOTE: The -au_boot_ini option is now obsolete and will have no impact\n        \
                 on your XLF file. Please deselect this option from your build!!\n");

		}
	}

	(*xlfIbootHdr)->fileCount = (*xlfData)->inputFileCount;

	/*
	** fileOffset[i] will be written when the input-file #i
	** has been written to the XLF
	*/
	for (fileCount = 0;
	     fileCount < (*xlfData)->inputFileCount;
	     fileCount++)
	{
		(*xlfIbootHdr)->fileOffset[fileCount] = 0;
	}

	return;
}

/******************************************************************************
 *
 * Local function:
 *      writeIbootHeader
 *
 * Parameters:
 *        xlfData    - Pointer to a struct holding information about the XLF.
 *        xlfIbootHdr - Pointer to the XLF header.
 *        xlfOffset  - Number of bytes written to teh XLF.
 *
 * Description:
 *      Writes the IBOOT-header to the XLF and updates 'xlfOffset'.
 *
 *****************************************************************************/
static void writeIbootHeader(xlfDataS_t *xlfData,
                             IBOOT_XlfIbootHeaderS_t *xlfIbootHdr,
                             U32 *xlfOffset)
{
#ifdef __linux__
	U16 fileCount;
#endif

	/*
	** Fill out missing info in iboot header:
	** crc2offset field and crc1 field.
	*/
	xlfIbootHdr->crc2Offset = (U32)(*xlfOffset);
	xlfIbootHdr->crc1 = (U16)calcSum((U8 *)xlfIbootHdr,
	                                 offsetof(IBOOT_XlfIbootHeaderS_t, crc1)); /*lint !e413*/
	/* Set correct endian. big endian in file. */
#ifdef __linux__
	xlfIbootHdr->magic        = htobe32(xlfIbootHdr->magic);
	xlfIbootHdr->type         = htobe32(xlfIbootHdr->type);
	xlfIbootHdr->spare1       = htobe16(xlfIbootHdr->spare1);
	xlfIbootHdr->prodDateTime = htobe32(xlfIbootHdr->prodDateTime);
	xlfIbootHdr->xplOffset    = htobe32(xlfIbootHdr->xplOffset);
	xlfIbootHdr->crc2Offset   = htobe32(xlfIbootHdr->crc2Offset);
	xlfIbootHdr->crc1         = htobe16(xlfIbootHdr->crc1);
	xlfIbootHdr->spare2       = htobe16(xlfIbootHdr->spare2);
	xlfIbootHdr->update       = htobe32(xlfIbootHdr->update);
	xlfIbootHdr->seqNo        = htobe32(xlfIbootHdr->seqNo);
	xlfIbootHdr->fileCount    = htobe32(xlfIbootHdr->fileCount);

	for (fileCount = 0; fileCount < xlfData->inputFileCount; fileCount++)
	{
		xlfIbootHdr->fileOffset[fileCount] = htobe32(xlfIbootHdr->fileOffset[fileCount]);
	}
#endif

	/* Start writing at the beginning of the output-file */
	if (fseek(xlfData->xlfFile.ptr,
	          0L,
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s\n",
		        xlfData->xlfFile.name);
		closeFiles(&xlfData);
		exit(1);
	}

	writeToFile((void *)xlfIbootHdr,
	            (size_t)xlfData->xlfIbootHdrSize,
	            xlfData->xlfFile.ptr,
	            xlfData->xlfFile.name,
	            &xlfData,
	            __LINE__);

	return;
}

/******************************************************************************
 *
 * Local function:
 *      writeXpl
 *
 * Parameters:
 *        xlfData    - Pointer to a struct holding information about the XLF.
 *        xlfOffset  -  Number of bytes written to the XLF.
 *
 * Description:
 *      Writes the XPL image to the XLF.
 *
 *  Side effects:
 *      Updates 'xlfOffset'.
 *
 *****************************************************************************/
static void writeXpl(xlfDataS_t **xlfData,
                     U32 *xlfOffset)
{
	U32 totalBytesWritten = 0;
	int rv;
	U32 padCount = 0;
	U32 padIndex = 0;
	U8 padByte = 0xff;

	/* Start reading the data from the XPL IM-file */
	if (fseek((*xlfData)->xplFile.ptr,
	          0L,
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek()' on file '%s' failed.\n",
		        (*xlfData)->xplFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	/*
	** Start writing the data read from the XPL IM-file
	** to the proper location in the output XLF-file
	*/
	if (fseek((*xlfData)->xlfFile.ptr,
	          (long)(*xlfData)->xlfIbootHdrSize,
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek()' on file '%s' failed.\n",
		        (*xlfData)->xplFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	for (;;)
	{
		rv = readAndWriteCharFromInputFileToXlf(
			xlfData,
			(*xlfData)->xplFile.name,
			(*xlfData)->xplFile.ptr);
		if (rv == EOF)
		{
			break;
		}
		totalBytesWritten++;
	}

	/* Pad to align end  of XPL in XLF on a 4 byte boundary */
	padCount = (4 - (totalBytesWritten % 4)) & 0x03;

	for (padIndex = 0; padIndex < padCount; padIndex++)
	{
		writeToFile((void *)&padByte,
		            sizeof(padByte),
		            (*xlfData)->xlfFile.ptr,
		            (*xlfData)->xlfFile.name,
		            xlfData,
		            __LINE__);

		totalBytesWritten++;
	}

	if (padCount)
	{
		if ((*xlfData)->verboseMode == YES)
		{
			fprintf(stdout,
			        "Padded the XPL-field of '%s' with 0x%x bytes\n"
			        "(in order to respect the 4-byte alignment requirement)\n",
			        (*xlfData)->xlfFile.name,
			        padCount);
		}
	}

	*xlfOffset = (*xlfData)->xlfIbootHdrSize + (sizeof(U8) * totalBytesWritten);

	fprintf(stdout,
	        "Copied contents from '%s' to '%s' OK\n",
	        (*xlfData)->xplFile.name,
	        (*xlfData)->xlfFile.name);
	return;
}

/******************************************************************************
 *
 * Local function:
 *        osRequired
 *
 * Parameters:
 *        type    -  expected output xlf type
 *
 * Return value:
 *        If xlf type should be checked for OS it returns 0, otherwise 1.
 *
 * Description:
 *        The return value can be set to xlfData->osPresent for later
 *        OS verification (look at checkForOS)
 *
 *  Side effects:
 *
 *****************************************************************************/
static U8 osRequired(U8 type)
{
	switch (type)
	{
	case XLF_IBOOT_HDR_AU_APPLIC:
	case XLF_IBOOT_HDR_AU_BOOT:
		return 0;
	default:
		return 1;
	}
}

/******************************************************************************
 *
 * Local function:
 *        checkForOS
 *
 * Parameters:
 *        xlfData    -  Pointer to a struct holding information about the XLF.
 *
 * Description:
 *        There must be one rpdout file that is marked with the OS flag in
 *        the category rpdout header field for XLF_gen.
 *        If there is no rpdout file that is marked with the OS flag, there
 *        have to be at least one instance of  'dtb', 'rootfs' and 'uimage'
 *        at the same time.
 *        If none of the above conditions are satisfied, issue a fault
 *        message and terminate.
 *
 *  Side effects:
 *
 *****************************************************************************/
static void checkForOS(xlfDataS_t *xlfData)
{
	if (xlfData->osPresent == 0 &&
	    !(xlfData->blobTypeExist[IBOOT_XlfBlobTypeE_DTB] &&
	      xlfData->blobTypeExist[IBOOT_XlfBlobTypeE_ROOTFS] &&
	      xlfData->blobTypeExist[IBOOT_XlfBlobTypeE_UIMAGE]))
	{
		fprintf(stderr,
		        "ERROR> No load module marked 'OS' and one blob file "
		        "for all types do not exist\n");
		closeFiles(&xlfData);
		exit(1);
	}
	return;
}

/******************************************************************************
 *
 * Local function:
 *        printSuccessCopyMsg
 *
 * Parameters:
 *        inFile     - Path to the input file
 *        outFile    - Path to the output file
 *
 * Description:
 *        Print copy successful message from input file to the output file
 *        to the stdout
 *
 *  Side effects:
 *
 *****************************************************************************/
static void printSuccessCopyMsg(char *inFile, char *outFile)
{
	fprintf(stdout,
	        "Copied contents from '%s' to '%s' OK\n",
	        inFile, outFile);
	return;
}

/******************************************************************************
 *
 * Local function:
 *      readAndWriteCharFromInputFileToXlf
 *
 * Parameters:
 *      xlfData        - Pointer to a struct holding information about the XLF
 *      inputFileName  - Input file name
 *      inputFP        - Input file pointer
 *
 * Return:
 *     Character read or EOF.
 *
 * Description:
 *      Writes the input-file to the output file (xlfFile).
 *
 *  Side effects:
 *
 *****************************************************************************/
static int readAndWriteCharFromInputFileToXlf(xlfDataS_t **xlfData,
                                              char *fileName,
                                              FILE *inputFP)
{
	int rv = fgetc(inputFP);
	if (rv == EOF)
	{
		if (ferror(inputFP))
		{
			printFileErrorAndTerminate(inputFP,
			                           fileName,
			                           xlfData,
			                           __LINE__,
			                           READ_FILE);
		}
		return rv;
	}
	else
	{
		if (fputc(rv, (*xlfData)->xlfFile.ptr) == EOF)
		{
			printFileErrorAndTerminate((*xlfData)->xlfFile.ptr,
			                           (*xlfData)->xlfFile.name,
			                           xlfData,
			                           __LINE__,
			                           WRITE_FILE);
		}
	}

	return rv;
}

/******************************************************************************
 *
 * Local function:
 *      writeBlobLMFile
 *
 * Parameters:
 *        xlfData    -  Pointer to a struct holding information about the XLF.
 *
 * Description:
 *      Completes the blob LM header and writes the input-file and its header
 *      to the output file.
 *
 *  Side effects:
 *
 *****************************************************************************/
static void writeBlobLmFile(xlfDataS_t **xlfData)
{
	int rv;
	unsigned int i;
	unsigned char *headerPtr;
	IBOOT_XlfBlobHeaderS_t *blobHeader = (*xlfData)->lmHeaderPtr;

	if (fseek((*xlfData)->xlfFile.ptr, sizeof(blobHeader->crc32), SEEK_SET) != 0)
	{
		printError((*xlfData)->xlfFile.name, SEEK_FILE, __LINE__);
		goto ERROR;
	}

#ifdef __linux__
	/* Set correct endian. big endian in file. */
	blobHeader->headerSize = htobe32(blobHeader->headerSize);
	blobHeader->type = htobe32(blobHeader->type);
	blobHeader->majorVer = htobe32(blobHeader->majorVer);
	blobHeader->minorVer = htobe32(blobHeader->minorVer);
#endif

	headerPtr = (unsigned char *)&(blobHeader->headerSize);
	for ( i = 0;
	      i < (sizeof(IBOOT_XlfBlobHeaderS_t) - sizeof(blobHeader->crc32));
	      i++)
	{
		if (fputc(*headerPtr, (*xlfData)->xlfFile.ptr) == EOF)
		{
			printFileErrorAndTerminate((*xlfData)->xlfFile.ptr,
			                           (*xlfData)->xlfFile.name,
			                           xlfData,
			                           __LINE__,
			                           WRITE_FILE);
		}
		CRC32(*headerPtr, blobHeader->crc32);
		headerPtr++;
	}

	/* Copy the entire blob-file to the load module. */
	for (;;)
	{
		rv = readAndWriteCharFromInputFileToXlf(xlfData,
		                                        (*xlfData)->inputFile[0].name,
		                                        (*xlfData)->inputFile[0].ptr);
		if (rv == EOF)
		{
			break;
		}

		CRC32(rv, blobHeader->crc32);
	} /* for (;;) */

	blobHeader->crc32 = ~blobHeader->crc32;

	/* Write CRC32 */
#ifdef __linux__
	/* Set correct endian. big endian in file. */
	blobHeader->crc32 = htobe32(blobHeader->crc32);
#endif
	if (fseek((*xlfData)->xlfFile.ptr, 0, SEEK_SET) != 0)
	{
		printError((*xlfData)->xlfFile.name, SEEK_FILE, __LINE__);
		goto ERROR;
	}

	writeToFile((void *)&blobHeader->crc32,
	            sizeof(blobHeader->crc32),
	            (*xlfData)->xlfFile.ptr,
	            (*xlfData)->xlfFile.name,
	            xlfData,
	            __LINE__);

	printSuccessCopyMsg((*xlfData)->inputFile[0].name,
	                    (*xlfData)->xlfFile.name);

	if ((*xlfData)->verboseMode == YES)
	{
		fprintf(stdout,
		        "Copied 0x%x bytes from %s to '%s'\n\n",
		        (unsigned int)fileSize((*xlfData)->inputFile[0].ptr),
		        (*xlfData)->inputFile[0].name,
		        (*xlfData)->xlfFile.name);
	}

	return;

ERROR:
	closeFiles(xlfData);
	exit(1);
} /*lint !e550*/


/******************************************************************************
 *
 * Local function:
 *       blobCheckResultsVerbose
 *
 * Parameters:
 *       expectedHeaderSize - Expected header size from the file
 *       expectedCrc32      - Expected crc32 value from the file
 *       crc32              - Calculated crc32 value
 *       blobFileName       - File name
 *
 * Return:
 *       0 if everything match
 *       1 if header size does not match
 *       2 if crc32 does not match
 *       3 neither header size nor crc32 match
 *
 * Description:
 *       Checks if actual calculated crc32 match file crc32 and
 *       file header size is the same as xlfgen blob header size.
 *
 *  Side effects:
 *
 *****************************************************************************/
static int blobCheckResultsVerbose(unsigned int expectedHeaderSize,
                                   unsigned int expectedCrc32,
                                   unsigned int crc32,
                                   char *blobFileName)
{
	int rv = 0;
#ifdef __linux__
	/* Set correct endian. big endian in file. */
	expectedHeaderSize = be32toh(expectedHeaderSize);
	crc32 = htobe32(crc32);
#endif
	if (expectedHeaderSize != (sizeof(IBOOT_XlfBlobHeaderS_t) -
	     offsetof(IBOOT_XlfBlobHeaderS_t, headerSize)))
	{
		fprintf(stderr, "WARNING> LM blob header for %s does not match"
		        "expected blob header.\n"
		        "Expected blob header size %u, %s blob header size %u\n",
		        blobFileName,
		        expectedHeaderSize,
		        xlfgenNameStr,
		        (unsigned int)(sizeof(IBOOT_XlfBlobHeaderS_t) -
		                       offsetof(IBOOT_XlfBlobHeaderS_t, headerSize)));
		        rv |= 1;
	}

	if (crc32 != expectedCrc32)
	{
		fprintf(stderr,"ERROR> Calculated crc32 for %s "
		        "does not match expected crc32. Expected 0x%08x, actual 0x%08x\n",
		        blobFileName,
		        expectedCrc32,
		        crc32);
		rv |= 2;
	}

	return rv;
}


/******************************************************************************
 *
 * Local function:
 *      wrapAndWriteFiles
 *
 * Parameters:
 *        xlfData    -  Pointer to a struct holding information about the XLF.
 *        xlfOffset  -  Number of bytes written to the XLF.
 *        xlfIbootHdr - Pointer to the IBOOT-header in the XLF.
 *
 * Description:
 *      Writes the input-files to the XLF and wraps them with suitable magic
 *      numbers. This function also checks if the RPDOUT-files that are to be
 *      included in the XLF, will overlap once they are loaded into target-RAM
 *
 *  Side effects:
 *
 *****************************************************************************/
static void wrapAndWriteFiles(xlfDataS_t **xlfData,
                              U32 *xlfOffset,
                              IBOOT_XlfIbootHeaderS_t *xlfIbootHdr)
{
	IBOOT_XlfIbootWrapperS_t     *xlfIbootWrapper;
	XPL_RpdoutHeaderS_t    tmpRpdoutHdr;
	XlfMemElemS_t         *xlfMemList = NULL;
	XlfMemElemS_t         *xlfMemListCpy = NULL;
	U32  inputFileIndex;
	int  rv;
	Bytef *tmpBuffer;
	z_stream d;
	long fSize;
	int err;
	U32  offsetBefore = *xlfOffset;
	U16 align;

	if (mallocVerbose((void**)&xlfIbootWrapper,
	                  sizeof(IBOOT_XlfIbootWrapperS_t), __LINE__))
	{
		closeFiles(xlfData);
		exit(1);
	}

	/* Set file pointer to correct place to start writing */
	if (fseek((*xlfData)->xlfFile.ptr,
	          (long)*xlfOffset,
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek()' on file '%s' failed.\n",
		        (*xlfData)->xlfFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	/* Spin on input-files and set wrapper magic to accordingly */
	for (inputFileIndex = 0;
	     inputFileIndex < (*xlfData)-> inputFileCount;
	     inputFileIndex++)
	{
		/* Get the input file size */
		fSize = fileSize((*xlfData)->inputFile[inputFileIndex].ptr);

		/* Get possible alignment request */
		align = (*xlfData)->inputFile[inputFileIndex].align;

		/* Fill out fileOffset entry in iboot header with correct offset */
		if (!align)
		{
			xlfIbootHdr->fileOffset[inputFileIndex] = (U32)(*xlfOffset) +
				sizeof(IBOOT_XlfIbootWrapperS_t) - sizeof(U32);
		}
		else
		{
			xlfIbootHdr->fileOffset[inputFileIndex] =
				(U32)((((*xlfOffset) + sizeof(IBOOT_XlfIbootWrapperS_t) +
				        (align - 1)) & ~(align - 1)) - sizeof(U32));
			fseek((*xlfData)->xlfFile.ptr, xlfIbootHdr->fileOffset[inputFileIndex] -
			      sizeof(U32), SEEK_SET);
		}

		/* Fill out the endOffset entry in the IBOOT wrapper */
		xlfIbootWrapper->endOffset = (U32)fSize +
			sizeof(IBOOT_XlfIbootWrapperS_t) - 1;

		/* Do an early update of xlfOffset, there is no need to step it
		 * byte by byte! */
		*xlfOffset = (xlfIbootHdr->fileOffset[inputFileIndex] + sizeof(U32) +
		              fSize);

		/* Set magic */
		xlfIbootWrapper->magic = (*xlfData)->inputFile[inputFileIndex].magic;
		/* Set correct endian. big endian in file. */
#ifdef __linux__
		xlfIbootWrapper->magic = htobe32(xlfIbootWrapper->magic);
		xlfIbootWrapper->endOffset = htobe32(xlfIbootWrapper->endOffset);
#endif
		writeToFile((void *)xlfIbootWrapper,
		            sizeof(IBOOT_XlfIbootWrapperS_t),
		            (*xlfData)->xlfFile.ptr,
		            (*xlfData)->xlfFile.name,
		            xlfData,
		            __LINE__);

		/* XLF_RPDOUT_FILE */
		if ((*xlfData)->inputFile[inputFileIndex].fileType == XLF_RPDOUT_FILE)
		{
			/*
			** Copy rpdout-header from file to host-RAM to access the info
			** stored therein.
			*/
			readFromFile(
				(void *)&tmpRpdoutHdr,
				sizeof(XPL_RpdoutHeaderS_t),
				(*xlfData)->inputFile[inputFileIndex].ptr,
				(*xlfData)->inputFile[inputFileIndex].name,
				xlfData,
				__LINE__);

			/* Set correct endian for first 6 bytes.
			 * hPsVolume in file is little endian
			 * hDsVolume in file is little endian
			 * hCategory in file is little endian
			 */
#ifndef __linux__ /* solaris2 */
			tmpRpdoutHdr.hPsVolume = swap2Bytes(tmpRpdoutHdr.hPsVolume);
			tmpRpdoutHdr.hDsVolume = swap2Bytes(tmpRpdoutHdr.hDsVolume);
			tmpRpdoutHdr.hCategory = swap2Bytes(tmpRpdoutHdr.hCategory);
#endif

			/* If the header is compressed it has to be decompressed */
			switch ((tmpRpdoutHdr.hCategory & 0x000E) >> 1)
			{
			case 0:                     /* No compression applied */
				break;

			case COMPRESSION_TYPE_ZLIB_113:     /* ZLIB compression */

				/* Reset the file pointer */
				if (fseek((*xlfData)->inputFile[inputFileIndex].ptr,0L,SEEK_SET))
				{
					fprintf(stderr, "ERROR> 'fseek()' failed \n");
					closeFiles(xlfData);
					exit(1);
				}

				/* Allocate a temporary area for the decompression source */
				if (mallocVerbose((void **)&tmpBuffer, fSize, __LINE__))
				{
					closeFiles(xlfData);
					exit(1);
				}

				/* Read from the input file to the decompression source */
				readFromFile(
					(void *)tmpBuffer,
					(size_t)fSize,
					(*xlfData)->inputFile[inputFileIndex].ptr,
					(*xlfData)->inputFile[inputFileIndex].name,
					xlfData,
					__LINE__);

				/* Init decompression library */
				d.zalloc = (alloc_func)0;
				d.zfree = (free_func)0;
				d.opaque = (voidp)0;
				d.next_in = (Bytef*)tmpBuffer+6;
				d.avail_in = (unsigned int)fSize - 6;
				d.next_out = (Bytef*)&tmpRpdoutHdr+6;
				d.avail_out = sizeof(XPL_RpdoutHeaderS_t)-6;

				if(inflateInit(&d) != Z_OK)
				{
					fprintf(stderr,"Initialization of decompression library failed");
					exit(1);
				}

				/* Decompress */
				if((err = inflate(&d, Z_SYNC_FLUSH)) != Z_OK)
				{
					fprintf(stderr, "'inflate' of rpdout file failed\n");
					exit(1);
				}

				/* Free temporary data */
				if(inflateEnd(&d) != Z_OK)
				{
					fprintf(stderr, "'inflateEnd' of rpdout file failed\n");
					exit(1);
				}
				free(tmpBuffer);
				break;

			default:            /* Unknown compression algorithm */
				fprintf(stderr,
				        "ERROR> File %s compressed with unknown algorithm\n",
				        (*xlfData)->inputFile[inputFileIndex].name);
				closeFiles(xlfData);
				exit (1);
				break;
			}

			/* Set correct endian.
			 * hCode in file is big endian
			 */
#ifdef __linux__
			tmpRpdoutHdr.hCode = htobe32(tmpRpdoutHdr.hCode);
#endif

			/*
			** Check to see if the rpdout file is marked as OS.
			** If there is more than one file marked as OS in each
			** load file, issue a fault message and terminate.
			*/
			if ((tmpRpdoutHdr.hCategory & OS_MASK) != 0)
			{
				(*xlfData)->osPresent += 1;
				if ((*xlfData)->osPresent > 1)
				{
					fprintf(stderr,
					        "ERROR> More than one load module marked 'OS'!\n");
					closeFiles(xlfData);
					exit(1);
				}
			}


			/* Check for overlapping files in target RAM. */
			xlfMemListCpy  = xlfMemList;

			while (xlfMemListCpy)
			{
				if (!(((tmpRpdoutHdr.hCode & ~XPL_ALIGN_MASK) >=
				       (xlfMemListCpy->offset + xlfMemListCpy->size)) ||
				      (((tmpRpdoutHdr.hCode & ~XPL_ALIGN_MASK) +
				        (tmpRpdoutHdr.hPsVolume * 512) +
				        (tmpRpdoutHdr.hDsVolume * 512)) <= xlfMemListCpy->offset)))
				{
					fprintf(stderr,
					        "\nERROR> File '%s' of size 0x%X and linked on offset 0x%X\n",
					        (*xlfData)->inputFile[inputFileIndex].name,
					        (tmpRpdoutHdr.hPsVolume * 512) +
					        (tmpRpdoutHdr.hDsVolume * 512),
					        (tmpRpdoutHdr.hCode & ~XPL_ALIGN_MASK));
					fprintf(stderr,
					        "will overlap with another file when loaded into target RAM!\n\n");
					closeFiles(xlfData);
					exit(1);
				}

				xlfMemListCpy = xlfMemListCpy->nextElem;
			} /* while (xlfMemList) */

			/* Update target RAM usage. */
			append((tmpRpdoutHdr.hCode & ~XPL_ALIGN_MASK),
			       (tmpRpdoutHdr.hPsVolume * 512) +
			       (tmpRpdoutHdr.hDsVolume * 512),
			       &xlfMemList,
			       xlfData);

			if (fseek((*xlfData)->inputFile[inputFileIndex].ptr,
			          0L,
			          SEEK_SET))
			{
				fprintf(stderr,
				        "ERROR> 'fseek()' failed \n");
				closeFiles(xlfData);
				exit(1);
			}

			/* Copy the entire rpdout-file to the load module. */
			for (;;)
			{
				rv = readAndWriteCharFromInputFileToXlf(
					xlfData,
					(*xlfData)->inputFile[inputFileIndex].name,
					(*xlfData)->inputFile[inputFileIndex].ptr);
				if (rv == EOF)
				{
					break;
				}
			} /* for (;;) */

			printSuccessCopyMsg((*xlfData)->inputFile[inputFileIndex].name,
			                    (*xlfData)->xlfFile.name);

		} /* if ((*xlfData)->inputFile[inputFileIndex].fileType == XLF_RPDOUT_FILE) */

		/* XLF_APPLIC_FILE */
		else if ((*xlfData)->inputFile[inputFileIndex].fileType == XLF_APPLIC_FILE)
		{
			for (;;)
			{
				rv = readAndWriteCharFromInputFileToXlf(
					xlfData,
					(*xlfData)->inputFile[inputFileIndex].name,
					(*xlfData)->inputFile[inputFileIndex].ptr);
				if (rv == EOF)
				{
					break;
				}
			} /* for (;;) */

			printSuccessCopyMsg((*xlfData)->inputFile[inputFileIndex].name,
			                    (*xlfData)->xlfFile.name);
		}
		/* XLF_BLOB_FILE */
		else if ((*xlfData)->inputFile[inputFileIndex].fileType == XLF_BLOB_FILE)
		{
			unsigned int i;
			IBOOT_XlfBlobHeaderS_t tmpBlobHeader;
			unsigned char *headerPtr = (unsigned char *)&tmpBlobHeader;
			unsigned int crc32 = 0xFFFFFFFF;

			/* Fill header from file to tmpBlobHeader */
			for (i = 0; i < sizeof(IBOOT_XlfBlobHeaderS_t); i++)
			{
				rv = readAndWriteCharFromInputFileToXlf(
					xlfData,
					(*xlfData)->inputFile[inputFileIndex].name,
					(*xlfData)->inputFile[inputFileIndex].ptr);
				if (rv == EOF)
				{
					break;
				}
				*headerPtr = rv;
				headerPtr++;
				if (i > 3)
				{
					CRC32(rv, crc32);
				}
			}
#ifdef __linux__
			/* Set correct endian. big endian in file. */
			tmpBlobHeader.type = be32toh(tmpBlobHeader.type);
#endif

			/* Copy the entire blob-file to the load module. */
			for (;;)
			{
				rv = readAndWriteCharFromInputFileToXlf(
					xlfData,
					(*xlfData)->inputFile[inputFileIndex].name,
					(*xlfData)->inputFile[inputFileIndex].ptr);
				if (rv == EOF)
				{
					break;
				}
				CRC32(rv, crc32);
			} /* for (;;) */

			crc32 = ~crc32;

			/* Check crc32 and header size */
			if ((blobCheckResultsVerbose(tmpBlobHeader.headerSize,
			                             tmpBlobHeader.crc32,
			                             crc32,
			                             (*xlfData)->inputFile[inputFileIndex].name)
			     & 0x2) == 2)
			{
				closeFiles(xlfData);
				exit(1);
			}

			/* Mark blob type exist */
			(*xlfData)->blobTypeExist[tmpBlobHeader.type] = 1;

			printSuccessCopyMsg((*xlfData)->inputFile[inputFileIndex].name,
			                    (*xlfData)->xlfFile.name);
		}  /* if ((*xlfData)->inputFile[inputFileIndex].fileType == XLF_BLOB_FILE) */
		else
		{
			fprintf(stderr,
			        "ERROR> Unknown file type occured, unable to write wrapper\n");
			closeFiles(xlfData);
			exit(1);
		}

		/* Align the LM to 4 bytes alignment */
		while((*xlfOffset) % 4)
		{
			if (fputc(0, (*xlfData)->xlfFile.ptr) == EOF)
			{
				printFileErrorAndTerminate((*xlfData)->xlfFile.ptr,
				                           (*xlfData)->xlfFile.name,
				                           xlfData,
				                           __LINE__,
				                           WRITE_FILE);
			}
			(*xlfOffset)++;
		}

	} /*
	  ** for (inputFileIndex = 0;
	  ** inputFileIndex < (*xlfData)-> inputFileCount;
	  ** inputFileIndex++)
	  */

	if ((*xlfData)->verboseMode == YES)
	{
		fprintf(stdout,
		        "Copied 0x%x bytes from applic-, rpdout- and/or blob-files to '%s'\n\n",
		        ((*xlfOffset) - offsetBefore),
		        (*xlfData)->xlfFile.name);
	}
	free(xlfIbootWrapper);
	/*
	** There must be one rpdout file that is marked with the OS flag in
	** the category rpdout header field for XLF_gen.
	** If there is no rpdout file that is marked with the OS flag, there
	** have to be at least a 'dtb', 'rootfs' and 'uimage' all at the same
	** time.
	** If none of the above conditions are satisfied, issue a fault
	** message and terminate. The result from the "multiple OS test" above
	** can be used here.
	*/
	checkForOS(*xlfData);

	return;
} /*lint !e550*/

/******************************************************************************
 *
 * Local function:
 *      writeIbootFooter
 *
 * Parameters:
 *        xlfData    -  Pointer to a struct holding information about the XLF.
 *        xlfOffset  -  Number of bytes written to the XLF.
 *
 * Description:
 *      Writes the IBOOT-footer to the XLF.
 *
 *  Side effects:
 *      Updates 'xlfOffset'.
 *
 *****************************************************************************/
static void writeIbootFooter(xlfDataS_t **xlfData,
                             U32 *xlfOffset)
{
	IBOOT_XlfIbootFooterS_t      *xlfIbootFooter;

	/* Allocate memory to hold the iboot footer   */
	if (mallocVerbose((void**)&xlfIbootFooter,
	                  sizeof(IBOOT_XlfIbootFooterS_t), __LINE__))
	{
		closeFiles(xlfData);
		exit(1);
	}

	/*
	** Fill out missing info in iboot footer:
	** crc2 field calculated from fileCnt (offset from iboot header start: 0x44)
	** to crc2.
	*/
	if (fseek((*xlfData)->xlfFile.ptr,
	          (long)offsetof(IBOOT_XlfIbootHeaderS_t, fileCount),
	          SEEK_SET)) /*lint !e413*/
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s\n",
		        (*xlfData)->xlfFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	xlfIbootFooter->crc2 = (U16)fileCalcsum(
		(U32)((*xlfOffset) -
		      offsetof(IBOOT_XlfIbootHeaderS_t,
		               fileCount)),
		xlfData);/*lint !e413*/

	/* Set correct endian. big endian in file. */
#ifdef __linux__
	xlfIbootFooter->crc2 = htobe16(xlfIbootFooter->crc2);
#endif

	/* Set file pointer to start of IBOOT footer field */
	if (fseek((*xlfData)->xlfFile.ptr,
	          (long)(*xlfOffset),
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s.\n",
		        (*xlfData)->xlfFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	writeToFile((void *)xlfIbootFooter,
	            sizeof(IBOOT_XlfIbootFooterS_t),
	            (*xlfData)->xlfFile.ptr,
	            (*xlfData)->xlfFile.name,
	            xlfData,
	            __LINE__);

	/* Update xlfOffset   */
	*xlfOffset += sizeof(IBOOT_XlfIbootFooterS_t);
	free(xlfIbootFooter);
	return;
}

/******************************************************************************
 *
 * Global function:     convProdNoRevNo
 *
 * Description:         Concatenates the IBOOT pid and revision (in SUID
 *                      format) to a string using delimiter. Use '_' to
 *                      follow LMID format.
 *
 * Parameters:          dest      - pointer to the result string
 *                      src       - pointer to the suid formatted string
 *                      delimiter - Delimiter to be used in between
 *                                  product number and revision
 *
 *****************************************************************************/
static void convProdNoRevNo(char *dest, char *src, char delimiter)
{
	int i;

	for (i = 0; i < IBOOT_PROD_NUMBER_LENGTH; i++)
		if (src[i] > ' ')
			*dest++ = (char)src[i];

	*dest++ = delimiter;

	for (i = IBOOT_PROD_NUMBER_LENGTH;
	     i < IBOOT_PROD_NUMBER_LENGTH + IBOOT_REV_NUMBER_LENGTH;
	     i++)
		if (src[i] > ' ')
			*dest++ = (char)src[i];

	*dest = '\0';
}

/******************************************************************************
 *
 * Local function:
 *      getIbootHeader
 *
 * Parameters:
 *        xlfData      -  Pointer to a struct holding information about the XLF.
 *        xlfIbootHdr  -  Pointer to the IBOOT-header,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 *
 * Return value:
 *       None
 *
 * Description:
 *       Read the IBOOT-Header from the XLF and store in host-RAM.
 *
 *  Side effects:
 *
 *
 *****************************************************************************/
static void getIbootHeader(xlfDataS_t **xlfData,
                           IBOOT_XlfIbootHeaderS_t **xlfIbootHdr)
{

#ifdef __linux__
	U16 fileCount;
#endif

	IBOOT_XlfIbootHeaderS_t tmpIbootHdr;

	readFromFile((void *)&tmpIbootHdr,
	             sizeof(IBOOT_XlfIbootHeaderS_t),
	             (*xlfData)->xlfFile.ptr,
	             (*xlfData)->xlfFile.name,
	             xlfData,
	             __LINE__);

	/* Set correct endian. big endian in file. */
#ifdef __linux__
	tmpIbootHdr.magic = be32toh(tmpIbootHdr.magic);

	tmpIbootHdr.majorVersion = be32toh(tmpIbootHdr.majorVersion);

	tmpIbootHdr.minorVersion = be32toh(tmpIbootHdr.minorVersion);

	tmpIbootHdr.fileCount = be32toh(tmpIbootHdr.fileCount);

	for (fileCount = 0; fileCount < tmpIbootHdr.fileCount; fileCount++)
	{
		tmpIbootHdr.fileOffset[fileCount] = be32toh(tmpIbootHdr.fileOffset[fileCount]);
	}
#endif
	/* Check if XLF header has the same version of XLFGEN */
	if (tmpIbootHdr.magic != XLF_IBOOT_HDR_MAGIC)
	{
		fprintf(stderr,
		        "ERROR> %s XLF magic (0x%x) is not equal to %s (0x%x)\n",
		        (*xlfData)->xlfFile.name,
		        tmpIbootHdr.magic,
		        xlfgenNameStr,
		        XLF_IBOOT_HDR_MAGIC);
		closeFiles(xlfData);
		exit(1);
	}

	if (tmpIbootHdr.majorVersion != XLF_FORMAT_VERSION_MAJOR)
	{
		fprintf(stderr,
		        "ERROR> %s major version (0x%x) is not equal to %s (0x%x)\n",
		        (*xlfData)->xlfFile.name,
		        tmpIbootHdr.majorVersion,
		        xlfgenNameStr,
		        XLF_FORMAT_VERSION_MAJOR);
		closeFiles(xlfData);
		exit(1);
	}

	if (tmpIbootHdr.minorVersion != XLF_FORMAT_VERSION_MINOR)
	{
		fprintf(stderr,
		        "ERROR> %s minor version (0x%x) is not equal to %s (0x%x)\n",
		        (*xlfData)->xlfFile.name,
		        tmpIbootHdr.minorVersion,
		        xlfgenNameStr,
		        XLF_FORMAT_VERSION_MINOR);
		closeFiles(xlfData);
		exit(1);
	}

	/*
	** Allocate memory for storing xlfIbootHdr.
	*/
	(*xlfData)->xlfIbootHdrSize = sizeof(IBOOT_XlfIbootHeaderS_t) +
		(tmpIbootHdr.fileCount - 1) *
		sizeof(tmpIbootHdr.fileOffset);

	if (mallocVerbose((void **)xlfIbootHdr,
	                  (*xlfData)->xlfIbootHdrSize, __LINE__))
	{
		closeFiles(xlfData);
		exit(1);
	}

	/*
	** Allocate memory for xlfData.
	*/
	reallocXlfData(xlfData, tmpIbootHdr.fileCount);

	if(fseek((*xlfData)->xlfFile.ptr, 0L, SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s \n",
		        (*xlfData)->xlfFile.name);
		closeFiles(xlfData);
		exit(1);
	}

	/* copy Iboot header from xlf file to host RAM */
	readFromFile((void *)*xlfIbootHdr,
	             (size_t)(*xlfData)->xlfIbootHdrSize,
	             (*xlfData)->xlfFile.ptr,
	             (*xlfData)->xlfFile.name,
	             xlfData,
	             __LINE__);

	/* Set correct endian. big endian in file. */
#ifdef __linux__
	(*xlfIbootHdr)->crc1         = be16toh((*xlfIbootHdr)->crc1);
	(*xlfIbootHdr)->crc2Offset   = be32toh((*xlfIbootHdr)->crc2Offset);
	(*xlfIbootHdr)->fileCount    = be32toh((*xlfIbootHdr)->fileCount);
	(*xlfIbootHdr)->xplOffset    = be32toh((*xlfIbootHdr)->xplOffset);

	for (fileCount = 0; fileCount < (*xlfIbootHdr)->fileCount; fileCount++)
	{
		(*xlfIbootHdr)->fileOffset[fileCount] = be32toh((*xlfIbootHdr)->fileOffset[fileCount]);
	}

	(*xlfIbootHdr)->type         = be32toh((*xlfIbootHdr)->type);
#endif
	(*xlfData)->type = (U8)(*xlfIbootHdr)->type;
	(*xlfData)->lmidPresent = YES;
	convProdNoRevNo((*xlfData)->lmid, (*xlfIbootHdr)->suid, '_');
	return;
}

/******************************************************************************
 *
 * Local function:
 *      getIbootFooter
 *
 * Parameters:
 *        xlfData         -  Pointer to a struct holding information about the XLF.
 *        xlfIbootFooter  -  Pointer to the IBOOT-Footer,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 * Return value:
 *        None
 *
 * Description:
 *        Read the IBOOT-Footer from the XLF and store in host-RAM.
 *
 * Side effects:
 *
 *****************************************************************************/
static void getIbootFooter(xlfDataS_t *xlfData,
                           IBOOT_XlfIbootFooterS_t **xlfIbootFooter,
                           IBOOT_XlfIbootHeaderS_t *xlfIbootHdr)
{
	if (mallocVerbose((void **)xlfIbootFooter,
	                  sizeof(IBOOT_XlfIbootFooterS_t), __LINE__))
	{
		closeFiles(&xlfData);
		exit(1);
	}

	/* Copy Iboot Footer to host RAM to access the info stored therein. */
	if (fseek(xlfData->xlfFile.ptr, (long)(xlfIbootHdr->crc2Offset), SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s\n",
		        xlfData->xlfFile.name);
		closeFiles(&xlfData);
		exit(1);
	}

	readFromFile((void *)*xlfIbootFooter,
	             sizeof(IBOOT_XlfIbootFooterS_t),
	             xlfData->xlfFile.ptr,
	             xlfData->xlfFile.name,
	             &xlfData,
	             __LINE__);

#ifdef __linux__
	(*xlfIbootFooter)->crc2 = be16toh((*xlfIbootFooter)->crc2);
#endif

	return;
}

/******************************************************************************
 *
 * Local function:
 *      getIbootWrapper
 *
 * Parameters:
 *        xlfData         -  Pointer to a struct holding information about the XLF.
 *        xlfIbootHdr     -  Pointer to the IBOOT-Header.
 *        xlfIbootFooter  -  Pointer to the IBOOT-Footer,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 *        lmNo            -  Load module No.
 *
 * Return value:
 *        None
 *
 * Description:
 *        Read the IBOOT-Wrapper of load module from the XLF and store in host-RAM.
 *
 * Side effects:
 *
 *****************************************************************************/
static void getIbootWrapper(xlfDataS_t *xlfData,
                            IBOOT_XlfIbootHeaderS_t *xlfIbootHdr,
                            IBOOT_XlfIbootWrapperS_t **xlfIbootWrapper,
                            U32 lmNo)
{
	*xlfIbootWrapper = (IBOOT_XlfIbootWrapperS_t *)malloc(sizeof(IBOOT_XlfIbootWrapperS_t));

	if (mallocVerbose((void **)xlfIbootWrapper,
	                  sizeof(IBOOT_XlfIbootWrapperS_t), __LINE__))
	{
		closeFiles(&xlfData);
		exit(1);
	}

	if (fseek(xlfData->xlfFile.ptr,
	          (long)(xlfIbootHdr->fileOffset[lmNo] -
	                 sizeof((*xlfIbootWrapper)->endOffset)),
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s \n",
		        xlfData->xlfFile.name);
		closeFiles(&xlfData);
		exit(1);
	}

	readFromFile((void *)*xlfIbootWrapper,
	             sizeof(IBOOT_XlfIbootWrapperS_t),
	             xlfData->xlfFile.ptr,
	             xlfData->xlfFile.name,
	             &xlfData,
	             __LINE__);

#ifdef __linux__
	(*xlfIbootWrapper)->endOffset = be32toh((*xlfIbootWrapper)->endOffset);
	(*xlfIbootWrapper)->magic = be32toh((*xlfIbootWrapper)->magic);
#endif
	xlfData->inputFile[lmNo].magic = (*xlfIbootWrapper)->magic;

	return;
}

/******************************************************************************
 *
 * Local function:
 *      verifyCrc1
 *
 * Parameters:
 *        xlfData         -  Pointer to a struct holding information about the XLF.
 *        xlfIbootHdr     -  Pointer to the IBOOT-Header,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 *
 * Return value:
 *        None
 *
 * Description:
 *       Verify checksum crc1.
 *
 * Side effects:
 *
 *****************************************************************************/
static void verifyCrc1(xlfDataS_t *xlfData,
                       IBOOT_XlfIbootHeaderS_t *xlfIbootHdr )
{
	U16 calcrc1;

	/* calculate crc1 */
	calcrc1 = (U16)calcSum((U8 *)xlfIbootHdr,
	                       offsetof(IBOOT_XlfIbootHeaderS_t, crc1));

	if (calcrc1 != xlfIbootHdr->crc1)
	{
		fprintf(stderr, "ERROR> Checksum crc1 mismatch.\n");
		closeFiles(&xlfData);
		exit(1);
	}
}

/******************************************************************************
 *
 * Local function:
 *      verifyCrc2
 *
 * Parameters:
 *        xlfData         -  Pointer to a struct holding information about the XLF.
 *        xlfIbootFooter  -  Pointer to the IBOOT-Footer,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 *
 * Return value:
 *        None
 *
 * Description:
 *        Verify checksum crc2.
 *
 * Side effects:
 *
 *****************************************************************************/
static void verifyCrc2(xlfDataS_t *xlfData,
                       IBOOT_XlfIbootFooterS_t *xlfIbootFooter,
                       IBOOT_XlfIbootHeaderS_t *xlfIbootHdr)
{

	U16 calcrc2;

	/* Calculate crc2
	** crc2 field calculated from fileCount (offset from iboot header start: 0x44)
	** to the end of file (exclude Iboot Footer).
	*/
	if (fseek(xlfData->xlfFile.ptr,
	          (long)offsetof(IBOOT_XlfIbootHeaderS_t, fileCount),
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s\n",
		        xlfData->xlfFile.name);
		closeFiles(&xlfData);
		exit(1);
	}

	calcrc2 = (U16)fileCalcsum(
		(xlfIbootHdr->crc2Offset -
		 offsetof(IBOOT_XlfIbootHeaderS_t,
		          fileCount)),
		&xlfData);


	if (calcrc2 != xlfIbootFooter->crc2)
	{
		fprintf(stderr, "ERROR> Checksum crc2 mismatch.\n");
		closeFiles(&xlfData);
		exit(1);
	}
	return;
}

/******************************************************************************
 *
 * Local function:
 *      copyXpl
 *
 * Parameters:
 *        xlfData         -  Pointer to a struct holding information about the XLF.
 *        xlfIbootHdr     -  Pointer to the IBOOT-Header,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 * Return value:
 *        None
 *
 * Description:
 *        Read XPL from xlf file, and write in a new file.
 *
 * Side effects:
 *
 *****************************************************************************/
static void copyXpl(xlfDataS_t *xlfData, IBOOT_XlfIbootHeaderS_t *xlfIbootHdr)
{
	Bytef *tmpBuffer = NULL;

	long fSize;
	char currPath[MAX_PATH];

	/* open xplfile for writing */
	xlfData->xplFile.ptr = fopen(xlfData->xplFile.name, "wb");
	if (xlfData->xplFile.ptr == NULL)
	{
		printError(xlfData->xplFile.name, OPEN_FILE, __LINE__);
		xlfData->xplSpecified = NO;
		closeFiles(&xlfData);
		exit(1);
	}

	/* calculate xpl file size */
	fSize = xlfIbootHdr->fileOffset[0] - xlfIbootHdr->xplOffset - sizeof(U32);

	/* allocate tmp buffer to store the data which will be written to xpl file */
	if (mallocVerbose((void**)&tmpBuffer, fSize, __LINE__))
	{
		closeFiles(&xlfData);
		exit(1);
	}

	if (fseek(xlfData->xlfFile.ptr,
	          xlfData->xlfIbootHdrSize,
	          SEEK_SET))
	{
		fprintf(stderr,
		        "ERROR> 'fseek' failed on file %s\n",
		        xlfData->xlfFile.name);
		closeFiles(&xlfData);
		exit(1);
	}

	readFromFile((void *)tmpBuffer,
	             (size_t)fSize,
	             xlfData->xlfFile.ptr,
	             xlfData->xlfFile.name,
	             &xlfData,
	             __LINE__);

	writeToFile((void *)tmpBuffer,
	            (size_t)fSize,
	            xlfData->xplFile.ptr,
	            xlfData->xplFile.name,
	            &xlfData,
	            __LINE__);

	/* get current Path for print entire xpl file directory info */
	if (getcwd(currPath, MAX_PATH) == NULL)
	{
		fprintf(stderr, "getcwd() error");
		closeFiles(&xlfData);
		exit(1);
	}

	fprintf(stdout, "XPL File: %s/%s\n",
	        currPath, xlfData->xplFile.name);

	free(tmpBuffer);
	return;
}

/******************************************************************************
 *
 * Local function:
 *       replaceCharInStr
 *
 * Arguments:
 *       str         -  Pointer to the string to be processed
 *       replace     -  Character that needs to be replaced in str
 *       with        -  Character that will be used instead of replace
 *
 * Description:
 *       Replace all instances of 'replace' with 'with' character in str
 *
 *
 *****************************************************************************/
static void replaceCharInStr(char *str, char replace, char with)
{
	U32 i;
	for (i = 0; str[i]; i++)
	{
		if (str[i] == replace)
		{
			str[i] = with;
		}
	}
	return;
}

/******************************************************************************
 *
 * Local function:
 *      createNewDir
 *
 * Parameters:
 *      tmpFolderName - Will contain path to the temp directory after
 *                      successful call to this function
 *
 * Return value:
 *      0 on success, 1 if cannot find a unique folder name after
 *      MAX_CREATE_FOLDER_ATTEMPTS
 *
 * Description:
 *      Creates a unique directory name under the current path for temporary
 *      file backup and storage
 *
 * Side effects:
 *      Creates an empty folder in current directory
 *
 *****************************************************************************/
static int createNewDir(char **tmpFolderName)
{
	unsigned int i = 0;
	char tmpDir[STRLEN_ULONG_MAX + 1];
	srand(time(NULL));
	do
	{
		errno = 0;
		snprintf(tmpDir, STRLEN_ULONG_MAX + 1, "%u", (unsigned int)rand());
		i++;
	}
	while (mkdir(tmpDir, 0777) && i < MAX_CREATE_FOLDER_ATTEMPTS);

	if (i == MAX_CREATE_FOLDER_ATTEMPTS)
	{
		return 1;
	}

	mallocAndConcatStrings(tmpFolderName, tmpDir, "/");

	return 0;
}

/******************************************************************************
 *
 * Local function:
 *      mallocAndFillFileToBuffer
 *
 * Parameters:
 *      xlfData         - Pointer to the struct that holds information
 *                        about the XLF.
 *      size            - The size of the data to be read
 *      offsetFromStart - The offset of the target data from the begining of
 *                        the source file
 *
 * Return:
 *      Address to the beginning of the allocated and filled buffer
 *
 * Description:
 *      Read (size bytes) data from source file to a buffer
 *
 *  Side effects:
 *      Allocated memory should be freed by the caller
 *
 *****************************************************************************/
static Bytef *mallocAndFillFileToBuffer(xlfDataS_t **xlfData,
                                        long size,
                                        long offsetFromStart)
{
	Bytef *tmpBuffer;

	/* allocate tmp buffer to store the data which will be written to output file */
	if (mallocVerbose((void**)&tmpBuffer, size, __LINE__))
	{
		closeFiles(xlfData);
		exit(1);
	}

	/* Seek to the provided offset from beginning of the file */
	if (fseek((*xlfData)->xlfFile.ptr, offsetFromStart, SEEK_SET) != 0)
	{
		printError((*xlfData)->xlfFile.name, SEEK_FILE, __LINE__);
		closeFiles(xlfData);
		exit(1);
	}

	/* Read size bytes data to the buffer */
	readFromFile((void *)tmpBuffer,
	             (size_t)size,
	             (*xlfData)->xlfFile.ptr,
	             (*xlfData)->xlfFile.name,
	             xlfData,
	             __LINE__);

	return tmpBuffer;
}

/******************************************************************************
 *
 * Local function:
 *      validateLM
 *
 * Parameters:
 *      LM      - Pointer to the buffer that contains LM data
 *      lmSize  - The size of the data in LM in bytes
 *      lmName  - Name of the LM.
 *
 * Return:
 *      0 if everything match
 *      1 if header size does not match
 *      2 if crc32 does not match
 *      3 neither header size nor crc32 match
 *
 * Description:
 *      Calculate CRC32 of the given LM and validate CRC32 and blob LM header
 *      size by calling blobCheckResultsVerbose.
 *
 *****************************************************************************/
static int validateLM(Bytef *LM, long lmSize, char *lmName)
{
	unsigned char *ptr;
	U32 crc32 = 0xFFFFFFFF;
	IBOOT_XlfBlobHeaderS_t *tmpBlobHeader = (IBOOT_XlfBlobHeaderS_t *)LM;

	for (ptr = LM + sizeof(crc32);
	     ptr < LM + lmSize;
	     ptr++)
	{
		CRC32(*ptr, crc32);
	}

	crc32 = ~crc32;

	/* Check crc32 and header size */
	return blobCheckResultsVerbose(tmpBlobHeader->headerSize,
	                               tmpBlobHeader->crc32,
	                               crc32,
	                               lmName);
}

/******************************************************************************
 *
 * Local function:
 *     splitBlobLm
 *
 * Arguments:
 *     xlfData         -  Pointer to a struct holding information about the XLF.
 *     headerFile      -  Path to the header file (descriptor)
 *     bodyFile        -  Path to the output file (data)
 *
 * Description:
 *      Split an Blob LM File to one LM header and one body (data) file.
 *
 *****************************************************************************/
static void splitBlobLm(xlfDataS_t **xlfData, char *headerFile, char *bodyFile)
{
	FILE *outputFP = NULL;
	char *openFileName;
	char tmpPid[LMID_MAX_STRING_LENGTH + 2];

	Bytef *tmpBuffer = NULL;
	IBOOT_XlfBlobHeaderS_t *tmpBlobHeader;

	long fSize = fileSize((*xlfData)->xlfFile.ptr);

	/* print LM filename */
	fprintf(stdout, "Split %s LM %s to:\nHeader file: %s\nOutput File: %s\n",
	        blobHeaderFormatStr,
	        (*xlfData)->xlfFile.name,
	        headerFile,
	        bodyFile);

	/* Read file into a buffer */
	tmpBuffer = mallocAndFillFileToBuffer(xlfData, fSize, 0);

	/* Validate BLOB LM */
	if (validateLM(tmpBuffer, fSize, (*xlfData)->xlfFile.name))
	{
		goto errorCloseFiles;
	}

	/* Assing Blob header */
	tmpBlobHeader = (IBOOT_XlfBlobHeaderS_t *)tmpBuffer;

	/* Open header file for write */
	openFileName = headerFile;
	outputFP = fopen(openFileName, "wb");
	if (outputFP == NULL)
	{
		printError(openFileName, OPEN_FILE, __LINE__);
		goto errorCloseFiles;
	}

	if (fprintf (outputFP, "# Auto generated output header file by %s from %s\n\n",
	             xlfgenNameStr,
	             (*xlfData)->xlfFile.name) < 2)
	{
		goto errorCloseOutputFile;
	}

	if (fprintf (outputFP, "%s %s\n",
	             lmHeaderTagStr[LM_HEADER_FORMAT],
	             blobHeaderFormatStr) < 2)
	{
		goto errorCloseOutputFile;
	}

	if (fprintf (outputFP, "%s %s\n",
	             lmHeaderTagStr[LM_TYPE],
	             getBlobTypeString(tmpBlobHeader->type)) < 2)
	{
		goto errorCloseOutputFile;
	}


	if (fprintf (outputFP, "%s %d.%d\n",
	             lmHeaderTagStr[LM_MISC_VERSION],
	             be32toh(tmpBlobHeader->majorVer),
	             be32toh(tmpBlobHeader->minorVer)) < 3)
	{
		goto errorCloseOutputFile;
	}

	if (strlen(tmpBlobHeader->suid))
	{
		convProdNoRevNo(tmpPid, (char *)tmpBlobHeader->suid, '_');
		if (fprintf (outputFP, "%s %s\n",
		             lmHeaderTagStr[LM_ID],
		             tmpPid) < 2)
		{
			goto errorCloseOutputFile;
		}
	}

	if (strlen(tmpBlobHeader->name))
	{
		if (fprintf (outputFP, "%s %s\n",
		             lmHeaderTagStr[LM_NAME],
		             tmpBlobHeader->name) < 2)
		{
			goto errorCloseOutputFile;
		}
	}

	/* Close header file */
	if (fclose(outputFP) != 0)
	{
		printError(openFileName, CLOSE_FILE, __LINE__);
		goto errorCloseFiles;
	}

	/* Open output file and write data */
	openFileName = bodyFile;
	outputFP = fopen(openFileName, "wb");
	if (outputFP == NULL)
	{
		printError(openFileName, OPEN_FILE, __LINE__);
		goto errorCloseFiles;
	}

	writeToFile(
		(void *)(tmpBuffer + sizeof(IBOOT_XlfBlobHeaderS_t)),
		((size_t)fSize - sizeof(IBOOT_XlfBlobHeaderS_t)),
		outputFP,
		openFileName,
		xlfData,
		__LINE__);

	/* Close body file */
	if (fclose(outputFP) != 0)
	{
		printError(openFileName, CLOSE_FILE, __LINE__);
		goto errorCloseFiles;
	}

	free(tmpBuffer);

	return;

errorCloseOutputFile:
	if (fclose(outputFP) != 0)
		printError(openFileName, CLOSE_FILE, __LINE__);

errorCloseFiles:
	closeFiles(xlfData);
	exit(1);
}

/******************************************************************************
 *
 * Local function:
 *     splitXlf
 *
 * Arguments:
 *       xlfData         -  Pointer to a struct holding information about the XLF.
 *       xlfIbootHdr     -  Pointer to the IBOOT-Header,
 *                      c.f. /vobs/rbs/hw/bcp/xp/IBOOT/include/iboot_if.h
 * Description:
 *      Split an XP Load File(XLF) to one or more files(rpdout or other type files).
 *
 *
 *****************************************************************************/
static void splitXlf(xlfDataS_t **xlfData,
                     IBOOT_XlfIbootHeaderS_t *xlfIbootHdr)
{
	IBOOT_XlfIbootWrapperS_t *xlfIbootWrapper = NULL;
	IBOOT_XlfBlobHeaderS_t tmpBlobHeader;
	Bytef *tmpBuffer = NULL;
	XPL_RpdoutTailS_t xplRpdoutTail;
	char *suid = NULL;
	char tmpDir[STRLEN_ULONG_MAX + 1 + 1] = {0}; /* STRLEN_ULONG_MAX + / + NULL */
	char outputFilename[255];
	char tmpPid[LMID_MAX_STRING_LENGTH + 1];
	char currPath[MAX_PATH];

	long fSize;
	U16  fileCount;

	if ((*xlfData)->manipulateMode != NO)
	{
		if (createNewDir(&(*xlfData)->tmpFolderName))
		{
			fprintf(stderr, "Cannot create a unique folder\n");
			closeFiles(xlfData);
			exit(1);
		}
		strcpy(tmpDir, (*xlfData)->tmpFolderName);
	}

	/* print XLF filename and lmid */
	convProdNoRevNo(tmpPid, (char *)xlfIbootHdr->suid, '_');

	fprintf(stdout, "Split XLF %s (PID: %s) to:\n", (*xlfData)->xlfFile.name, tmpPid);

	if (xlfIbootHdr->xplOffset != 0)
	{
		/* read xpl from XLF file, and store in xpl.im */
		(*xlfData)->xplSpecified = YES;

		mallocAndConcatStrings(&(*xlfData)->xplFile.name,
		                       tmpDir,
		                       "xpl.im");

		copyXpl(*xlfData, xlfIbootHdr);
	}

	/* get current Path for print entire outputfile directory info */
	if (getcwd(currPath, MAX_PATH) == NULL)
	{
		fprintf(stderr, "getcwd() error");
		closeFiles(xlfData);
		exit(1);
	}

	/* Loop fileCount to copy each LM to outputfile */
	for ( fileCount = 0; fileCount < xlfIbootHdr->fileCount; fileCount++ )
	{
		getIbootWrapper(*xlfData, xlfIbootHdr, &xlfIbootWrapper, fileCount);

		/*
		** For rpdout file,
		** named with "SW product id_SW product revision"
		** which is stored in t_suid of RPDOUT Tail
		*/
		if (xlfIbootWrapper->magic == XLF_IBOOT_WPR_RPDOUT_MAGIC)
		{
			/* Store file info in xlfData */
			(*xlfData)->inputFile[fileCount].magic = XLF_IBOOT_WPR_RPDOUT_MAGIC;
			(*xlfData)->inputFile[fileCount].fileType = XLF_RPDOUT_FILE;

			/* Copy RPDOUT Tail to host-RAM to access the info stored therein */
			if (fseek((*xlfData)->xlfFile.ptr,
			      (long)(xlfIbootHdr->fileOffset[fileCount] +
			             xlfIbootWrapper->endOffset +
			             1 -
			             sizeof(xlfIbootWrapper->endOffset) -
			             sizeof(XPL_RpdoutTailS_t)),
			      SEEK_SET) != 0)
			{
				printError((*xlfData)->xlfFile.name, SEEK_FILE, __LINE__);
				closeFiles(xlfData);
				exit(1);
			}

			readFromFile((void *)&xplRpdoutTail,
			             sizeof(XPL_RpdoutTailS_t),
			             (*xlfData)->xlfFile.ptr,
			             (*xlfData)->xlfFile.name,
			             xlfData,
			             __LINE__);

			/*
			** Produce output filename
			** "SW product id_SW product rev.rpdout"
			*/
			suid = (char *)xplRpdoutTail.tSuid;
		}
		else if (xlfIbootWrapper->magic == XLF_IBOOT_WPR_BLOB_MAGIC)
		{
			/* Store file info in xlfData */
			(*xlfData)->inputFile[fileCount].magic = XLF_IBOOT_WPR_BLOB_MAGIC;
			(*xlfData)->inputFile[fileCount].fileType = XLF_BLOB_FILE;

			/* Copy BLOB head to host-RAM */
			readFromFile((void *)&tmpBlobHeader,
			             sizeof(IBOOT_XlfBlobHeaderS_t),
			             (*xlfData)->xlfFile.ptr,
			             (*xlfData)->xlfFile.name,
			             xlfData,
			             __LINE__);

			if (fseek((*xlfData)->xlfFile.ptr,
			          -1 * sizeof(IBOOT_XlfBlobHeaderS_t),
			          SEEK_CUR) != 0)
			{
				printError((*xlfData)->xlfFile.name, SEEK_FILE, __LINE__);
				closeFiles(xlfData);
				exit(1);
			}

			/*
			** Produce output filename
			** "SW product id_SW product rev.rpdout"
			*/
			suid = tmpBlobHeader.suid;
		}
		/*
		** For other type file
		** named with magic number
		*/
		else
		{
			(*xlfData)->inputFile[fileCount].magic = xlfIbootWrapper->magic;
			(*xlfData)->inputFile[fileCount].fileType = XLF_APPLIC_FILE;

			sprintf(outputFilename, "%d_0x%x.bin", fileCount, (unsigned int)xlfIbootWrapper->magic);
		}

		if (xlfIbootWrapper->magic == XLF_IBOOT_WPR_RPDOUT_MAGIC ||
		    xlfIbootWrapper->magic == XLF_IBOOT_WPR_BLOB_MAGIC)
		{
			convProdNoRevNo(tmpPid, suid, '_');

			/* replace all '/' to '%' in output filename */
			replaceCharInStr(tmpPid, '/', '%');

			/* if suid is empty, named with magic number */
			if (!strcmp(tmpPid, "_"))
			{
				sprintf(outputFilename, "%d_0x%x", fileCount, (unsigned int)xlfIbootWrapper->magic);
			}
			else
			{
				sprintf(outputFilename, "%d_%s", fileCount, tmpPid);
			}

			if (xlfIbootWrapper->magic == XLF_IBOOT_WPR_RPDOUT_MAGIC)
			{
				strcat(outputFilename, ".rpdout");
			}
			else /* XLF_IBOOT_WPR_BLOB_MAGIC */
			{
				strcat(outputFilename, ".bloblm");
			}
		}

		/* Assign the name in xlfData */
		mallocAndConcatStrings(&(*xlfData)->inputFile[fileCount].name,
		                       tmpDir,
		                       outputFilename);
		(*xlfData)->inputFile[fileCount].align = 0;

		/* calculate output file size */
		fSize = xlfIbootWrapper->endOffset - sizeof(IBOOT_XlfIbootWrapperS_t) + 1;

		/* Read file into a buffer */
		tmpBuffer = mallocAndFillFileToBuffer(
			xlfData,
			fSize,
			(long)(xlfIbootHdr->fileOffset[fileCount] +
			       sizeof(xlfIbootWrapper->magic)));

		/* Validate BLOB LM */
		if (xlfIbootWrapper->magic == XLF_IBOOT_WPR_BLOB_MAGIC)
		{
			validateLM(tmpBuffer, fSize,
			           (*xlfData)->inputFile[fileCount].name);
		}

		/* open output file for writing */
		(*xlfData)->inputFile[fileCount].ptr =
			fopen((*xlfData)->inputFile[fileCount].name, "wb");

		if ((*xlfData)->inputFile[fileCount].ptr == NULL)
		{
			printError((*xlfData)->inputFile[fileCount].name,
			           OPEN_FILE, __LINE__);
			closeFiles(xlfData);
			exit(1);
		}

		/* Update xlfData inputFileCount */
		(*xlfData)->inputFileCount += 1;

		writeToFile((void *)tmpBuffer,
		            (size_t)fSize,
		            (*xlfData)->inputFile[fileCount].ptr,
		            (*xlfData)->inputFile[fileCount].name,
		            xlfData,
		            __LINE__);

		fprintf(stdout, "File %d: %s/%s\n",
		        fileCount, currPath, (*xlfData)->inputFile[fileCount].name);

		free(tmpBuffer);
	}

	free(xlfIbootWrapper);
	return;
}

/******************************************************************************
 *
 * Local function:
 *      swap2Bytes
 *
 * Parameters:
 *        d          - 2 bytes
 *
 * Return value:
 *      Two bytes where the most significant and least signinficant byte
 *      have changed place in comparison with the input.
 *
 *****************************************************************************/
static unsigned short swap2Bytes(unsigned short d)
{
	return( ((d>>8)&0x00ff) | (d<<8) ); /*lint !e734*/
}

/******************************************************************************
 *
 * Local function:
 *      memstr
 *
 * Parameters:
 *         find the string in memory
 *
 * Return value:
 *     NULL or the position in the memory
 *
 *****************************************************************************/
static char *memstr(const char *cs, const char *ct, size_t n)
{
	size_t len = strlen(ct);
	char *end = (char *)cs + n;
	while (cs < end)
	{
		if (!memcmp(cs, ct, len))
		{
			return (char *)cs;
		}
		cs ++ ;
	}
	return NULL;
}
/******************************************************************************
 *
 * Local function:
 *      configSigBuf
 *
 * Parameters:
 *         xlfDataS_t **xlfData
 *
 * Return value:
 *
 *
 *****************************************************************************/
static void updateSigBuf(xlfDataS_t **xlfData)
{
	U32 inputFileIndex = 0;
	long inSize = 0;
	/*buffer used to store the compressed data*/
	char *comp_data_buf = NULL;
	/*buffer used to store the uncompress data*/
	char *uncomp_data_buf = NULL;
	U32 bufOffset = 0;
	int err;
	U32 i = 0;
	char *p = NULL;
	U16 chkSum;
	U16 hCategory;
	z_stream d;
	XPL_RpdoutHeaderS_t tmpRpdoutHdr;
	for (inputFileIndex = 0; inputFileIndex < (*xlfData)->inputFileCount; inputFileIndex++)
	{
		if ((*xlfData)->inputFile[inputFileIndex].fileType != XLF_RPDOUT_FILE)
		{
			continue;
		}

		readFromFile(
			(void *)&tmpRpdoutHdr,
			sizeof(XPL_RpdoutHeaderS_t),
			(*xlfData)->inputFile[inputFileIndex].ptr,
			(*xlfData)->inputFile[inputFileIndex].name,
			xlfData,
			__LINE__);

		if (fseek((*xlfData)->inputFile[inputFileIndex].ptr, 0L, SEEK_SET))
		{
			fprintf(stderr, "ERROR> 'fseek' failed on file %s\n", (*xlfData)->inputFile[inputFileIndex].name);
			goto ERROR;
		}
#ifndef __linux__
		tmpRpdoutHdr.hPsVolume = swap2Bytes(tmpRpdoutHdr.hPsVolume);
		tmpRpdoutHdr.hDsVolume = swap2Bytes(tmpRpdoutHdr.hDsVolume);
		tmpRpdoutHdr.hCategory = swap2Bytes(tmpRpdoutHdr.hCategory);
#endif
		if ((tmpRpdoutHdr.hCategory & OS_MASK) != 0)
		{
			(*xlfData)->osPresent += 1;
			if ((*xlfData)->osPresent > 1)
			{
				fprintf(stderr, "ERROR> More than one load module marked 'OS'!\n");
				goto ERROR;
			}
			hCategory = tmpRpdoutHdr.hCategory;
			if (!(((hCategory & 0x000E) >> 1) == COMPRESSION_TYPE_ZLIB_113 ||
			      ((hCategory & 0x000E) >> 1) == 0))
			{
				fprintf(stderr,
				        "ERROR> File %s compressed with unknown algorithm\n",
				        (*xlfData)->inputFile[inputFileIndex].name);
			}
			inSize = fileSize((*xlfData)->inputFile[inputFileIndex].ptr);
			if (mallocVerbose((void**)&uncomp_data_buf,
			                  tmpRpdoutHdr.hPsVolume * 512,
			                  __LINE__))
			{
				goto ERROR;
			}
			/*if the input is the compress XPP LM*/
			if (((hCategory & 0x000E) >>1) == COMPRESSION_TYPE_ZLIB_113)
			{
				if (mallocVerbose((void**)&comp_data_buf, inSize, __LINE__))
				{
					goto ERROR;
				}

				readFromFile(
					(void *)comp_data_buf,
					(size_t)inSize,
					(*xlfData)->inputFile[inputFileIndex].ptr,
					(*xlfData)->inputFile[inputFileIndex].name,
					xlfData,
					__LINE__);

				memcpy(uncomp_data_buf, comp_data_buf,
				       sizeof(tmpRpdoutHdr.hPsVolume) + sizeof(tmpRpdoutHdr.hDsVolume) + sizeof(tmpRpdoutHdr.hCategory));
				/* adjust hCategory as uncompress*/
				tmpRpdoutHdr.hCategory &= 0xFBFF;
				*(uncomp_data_buf + sizeof(tmpRpdoutHdr.hPsVolume) + sizeof(tmpRpdoutHdr.hDsVolume)) &= 0xFB;
				d.zalloc = (alloc_func)0;
				d.zfree = (free_func)0;
				d.opaque = (voidp)0;
				d.next_in = (Bytef*)comp_data_buf + 6;
				d.avail_in = (unsigned int)inSize - 6 - sizeof(XPL_RpdoutTailS_t);
				d.next_out = (Bytef*)uncomp_data_buf + bufOffset + 6;
				d.avail_out = DECOMP_BUF_SIZE ;
				if (inflateInit(&d) != Z_OK)
				{
					fprintf(stderr,"'Initialization' of decompression library failed");
					goto ERROR;
				}
				err = inflate(&d, Z_SYNC_FLUSH);
				bufOffset += DECOMP_BUF_SIZE - d.avail_out + 6;
				while (err==Z_OK || err==Z_STREAM_END)
				{
					if(err == Z_STREAM_END)
						break;
					d.next_out= (Byte*)uncomp_data_buf + bufOffset;
					d.avail_out = DECOMP_BUF_SIZE ;
					err = inflate(&d, Z_SYNC_FLUSH);
					bufOffset += DECOMP_BUF_SIZE - d.avail_out;
				}
				if (inflateEnd(&d) != Z_OK)
				{
					fprintf(stderr, "'inflateEnd' of rpdout file failed\n");
					goto ERROR;
				}
				memcpy(uncomp_data_buf + bufOffset, comp_data_buf + inSize - sizeof(XPL_RpdoutTailS_t), sizeof(XPL_RpdoutTailS_t));
				inSize = bufOffset + sizeof(XPL_RpdoutTailS_t);
			}
			/*if input the uncompress file*/
			else
			{
				readFromFile(
					(void *)uncomp_data_buf,
					(size_t)inSize,
					(*xlfData)->inputFile[inputFileIndex].ptr,
					(*xlfData)->inputFile[inputFileIndex].name,
					xlfData,
					__LINE__);
			}
			if (comp_data_buf != NULL)
			{
				free(comp_data_buf);
				comp_data_buf = NULL;
			}
			/* search in the buffer to find the mark krn/buffer_sizes=*/
			if ((p = memstr(uncomp_data_buf, SIG_BUF_CONFIGURATION_SIGN, inSize)) != NULL)
			{
				p = p + strlen(SIG_BUF_CONFIGURATION_SIGN);
				for (i = 0; i < (*xlfData)->sigBufCon.numOfValues; i++)
				{
					p += sprintf(p, "%05d", (int)((*xlfData)->sigBufCon.value[i]));
					*p++ = ',';
				}
			}
			else
			{
				fprintf(stderr, "The input file %s not be allowed to reconfig the sig buf.\n",
				        (*xlfData)->inputFile[inputFileIndex].name);
				goto ERROR;
			}
			/*adjust the crc*/
			chkSum = (U16)calcSum((U8 *)uncomp_data_buf, (U32)((tmpRpdoutHdr.hPsVolume * 512) - sizeof(chkSum)));
#ifdef __linux__
			chkSum = swap2Bytes(chkSum);
#endif
			*(U16*)(uncomp_data_buf + inSize - sizeof(chkSum)) = chkSum;
			if (fclose((*xlfData)->inputFile[inputFileIndex].ptr) == EOF)
			{
				fprintf(stderr,
				        "'fclose()' failed input file\n");
				goto ERROR;
			}
			(*xlfData)->inputFile[inputFileIndex].ptr = NULL;
			if (((*xlfData)->inputFile[inputFileIndex].ptr = fopen(SIG_BUF_RECONFIGURED_LM, "wb")) == NULL)
			{
				fprintf(stderr,
				        "ERROR> Could not open '%s'\n",
				        SIG_BUF_RECONFIGURED_LM);
				goto ERROR;
			}
			strcpy((*xlfData)->inputFile[inputFileIndex].name, SIG_BUF_RECONFIGURED_LM);
			/* if the input is compressed, compress again*/
			if (((hCategory & 0x000E) >>1) == COMPRESSION_TYPE_ZLIB_113)
			{
				comp_data_buf =(char *)malloc(inSize + inSize/1000 + 12);
				if (comp_data_buf == NULL)
				{
					fprintf(stderr, "'malloc' of compression destination buffer failed\n");
					goto ERROR;
				}
				memcpy(comp_data_buf, uncomp_data_buf, 6);
				d.zalloc = (alloc_func)0;
				d.zfree  = (free_func)0;
				d.opaque = (voidpf)0;
				if (deflateInit(&d, Z_DEFAULT_COMPRESSION) != Z_OK)
				{
					fprintf(stderr, "Initialization of compression library failed");
					goto ERROR;
				}
				d.next_out = (Byte*)comp_data_buf + 6;
				d.avail_out = inSize - 6 - sizeof(XPL_RpdoutTailS_t);
				d.next_in = (Byte*)uncomp_data_buf + 6;
				d.avail_in = inSize - 6 - sizeof(XPL_RpdoutTailS_t);
				if (deflate(&d, Z_FINISH) != Z_STREAM_END)
				{
					fprintf(stderr, "'deflate' of rpdout file failed\n");
					goto ERROR;
				}
				if (deflateEnd(&d) != Z_OK)
				{
					fprintf(stderr, "'deflateEnd' of rpdout file failed\n");
					goto ERROR;
				}
				memcpy(d.next_out, d.next_in, sizeof(XPL_RpdoutTailS_t));
				hCategory = *(U16*)(comp_data_buf + sizeof(tmpRpdoutHdr.hDsVolume) + sizeof(tmpRpdoutHdr.hCategory));
#ifndef __linux__
				hCategory = swap2Bytes(hCategory);
#endif
				hCategory |= (COMPRESSION_TYPE_ZLIB_113 << 1);
#ifndef __linux__
				hCategory = swap2Bytes(hCategory);
#endif
				/* adjust the hCategory in LM as compressed*/
				*(U16*)(comp_data_buf + sizeof(tmpRpdoutHdr.hPsVolume) + sizeof(tmpRpdoutHdr.hDsVolume)) = hCategory;

				writeToFile(
					(void *)comp_data_buf,
					(size_t)(6 + d.total_out + sizeof(XPL_RpdoutTailS_t)),
					(*xlfData)->inputFile[inputFileIndex].ptr,
					(*xlfData)->inputFile[inputFileIndex].name,
					xlfData,
					__LINE__);

				if (fclose((*xlfData)->inputFile[inputFileIndex].ptr) == EOF)
				{
					fprintf(stderr, "ERROR> 'fclose()' failed on file %s\n", (*xlfData)->inputFile[inputFileIndex].name);
					goto ERROR;
				}
			}
			/*if the input is uncompress, after change the sigbuf and adjust the crc, input to temp fie directly*/
			else
			{
				writeToFile(
					(void *)uncomp_data_buf,
					(size_t)inSize,
					(*xlfData)->inputFile[inputFileIndex].ptr,
					(*xlfData)->inputFile[inputFileIndex].name,
					xlfData,
					__LINE__);

				if (fclose((*xlfData)->inputFile[inputFileIndex].ptr) == EOF)
				{
					fprintf(stderr, "ERROR> 'fclose()' failed on file %s\n", (*xlfData)->inputFile[inputFileIndex].name);
					goto ERROR;
				}
			}
			if (((*xlfData)->inputFile[inputFileIndex].ptr = fopen(SIG_BUF_RECONFIGURED_LM, "rb")) == NULL)
			{
				fprintf(stderr, "ERROR> 'fopen()' failed on file %s\n", (*xlfData)->inputFile[inputFileIndex].name);
				goto ERROR;
			}
			if (uncomp_data_buf != NULL)
			{
				free(uncomp_data_buf);
				uncomp_data_buf = NULL;
			}
			if (comp_data_buf != NULL)
			{
				free(comp_data_buf);
				comp_data_buf = NULL;
			}
		}
	}
	(*xlfData)->osPresent = 0;
	return;
ERROR:
	closeFiles(xlfData);
	exit(1);
}

/******************************************************************************
 *
 * Local function:
 *     manipulateXlf
 *
 * Arguments:
 *     argc       - Number of command line arguments.
 *     argv       - The argument vector.
 *     xlfData    - Pointer to a struct holding information about the XLF.
 *
 * Description:
 *      Decodes and perform additional tasks that are needed to manipulate
 *      a specific XLF file.
 *
 *****************************************************************************/
static void manipulateXlf(int argc,
                          char **argv,
                          xlfDataS_t **xlfData)
{
	switch ((*xlfData)->manipulateMode)
	{
	case APPEND_MODE:
	{
		char *backupXlfPath;

		traverseCmdLineGeneratorFlags(3, argc, argv, xlfData);

		mallocAndConcatStrings(&backupXlfPath,
		                       (*xlfData)->tmpFolderName,
		                       originalXlfFileName);
		rename((*xlfData)->xlfFile.name, backupXlfPath);

		openFiles(xlfData);

		free(backupXlfPath);

		break;
	}
	case NO:
		break;

	default:
		fprintf(stderr, "ERROR> %d is an unknown manipulate op-code.\n",
		        (*xlfData)->manipulateMode);
		(*xlfData)->manipulateMode = YES;
		restoreAndCleanUpFiles(xlfData);
		exit(1);
		break;
	}

	return;
}

/******************************************************************************
 *
 * Global function:
 *      main
 *
 * Arguments:
 *        argc       - the number of command-line arguments
 *        argv       - vector of command-line argument-strings
 *
 * Description:
 *      Constructs or splits an XP Load File(XLF).
 *
 *
 *****************************************************************************/
int main(int argc, char **argv)
{
	xlfDataS_t *xlfData = NULL;
	IBOOT_XlfIbootHeaderS_t *xlfIbootHdr = NULL;
	IBOOT_XlfIbootFooterS_t *xlfIbootFooter = NULL;
	U32 xlfOffset;

	xlfgenNameStr = basename(argv[0]);

	if(argc == 1)
	{
		printUsageAndDie(0);
	}

	parseCmdLine(argc, argv, &xlfData);

	openFiles(&xlfData);

	/* For XLF Splitter and XLF Append */
	if ((xlfData->splitMode == YES))
	{
		U8 inputManipulateMode = xlfData->manipulateMode;

		if (xlfData->splitType == XLF_BLOB_FILE)
		{
			/* Split BLOB LM */
			splitBlobLm(&xlfData, argv[4], argv[5]);
		}
		else
		{
			getIbootHeader(&xlfData, &xlfIbootHdr);
			if ( !xlfIbootHdr->fileCount )
			{
				fprintf(stderr, "ERROR> can't find any LM.\n");
				closeFiles(&xlfData);
				exit(1);
			}
			verifyCrc1(xlfData, xlfIbootHdr);

			getIbootFooter(xlfData, &xlfIbootFooter, xlfIbootHdr);
			verifyCrc2(xlfData, xlfIbootFooter, xlfIbootHdr);

			splitXlf(&xlfData, xlfIbootHdr);
		}

		/* Close open files but keep temp folder */
		xlfData->manipulateMode = DO_NOT_CLEAN;
		closeFiles(&xlfData);

		if (inputManipulateMode == NO)
		{
			free(xlfIbootHdr);

			free(xlfIbootFooter);

			return 0;
		}

		/* Restore xlfData->manipulateMode value */
		xlfData->manipulateMode = inputManipulateMode;
		xlfData->splitMode = NO;
	}

	/* If manipulate mode */
	manipulateXlf(argc, argv, &xlfData);

	/* For XLF Generator */
	/* Check if an OS should be present */
	xlfData->osPresent = osRequired(xlfData->type);

	if ((xlfData->verboseMode) == YES)
	{
		dumpXlfData(xlfData);
	}

	/* LM file needs to be created */
	if ((xlfData)->lmHeaderType)
	{
		int ret;
		if ((xlfData)->lmHeaderType == XLF_BLOB_FILE)
		{
			writeBlobLmFile(&xlfData);
			ret = 0;
		}
		else
		{
			fprintf(stderr,
			        "ERROR> LM header type (%u) is not supported.\n",
			        (xlfData)->lmHeaderType);
			ret = 1;
		}
		free((xlfData)->lmHeaderPtr);
		closeFiles(&xlfData);
		return ret;
	}

	if ((xlfData)->sigBufCon.numOfValues)
	{
		updateSigBuf(&xlfData);
	}

	initIbootHeader(&xlfData, &xlfIbootHdr);

	if (xlfData->xplSpecified == YES)
	{
		writeXpl(&xlfData, &xlfOffset);
	}
	else
	{
		xlfOffset = xlfData->xlfIbootHdrSize;
	}

	wrapAndWriteFiles(&xlfData, &xlfOffset, xlfIbootHdr);

	writeIbootHeader(xlfData, xlfIbootHdr, &xlfOffset);

	writeIbootFooter(&xlfData, &xlfOffset);

	/* Set manipulate to NO, to keep the generated XLF file */
	xlfData->manipulateMode = NO;

	closeFiles(&xlfData);

	free(xlfIbootHdr);
	return 0;
}
