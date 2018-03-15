/* ---------------------------------------------------------------------------
 *
 * © Ericsson AB 2015 All rights reserved.
 * The information in this document is the property of Ericsson.  Except
 * as specifically authorized in writing by Ericsson, the receiver of
 * this document shall keep the information contained herein confidential
 * and shall protect the same in whole or in part from disclosure and
 * dissemination to third parties.  Disclosure and disseminations to the
 * receivers employees shall only be made on a strict need to know basis.
 *
 * ---------------------------------------------------------------------------
 */
#include <stdarg.h>
#include <string.h>
#include <math.h>
#include "tpt.h"

#define TPT_MAX_TRACE_LEN 512

char *tpt_format_str(const char *format, ...)
{
	static __thread char internal_trace_buf[TPT_MAX_TRACE_LEN] = {0};
	va_list args;  /* Argument list */

	va_start(args, format);
	vsnprintf(internal_trace_buf, TPT_MAX_TRACE_LEN, format, args);
	va_end(args);

	return internal_trace_buf;
}

/*
 *   The format string has the same syntax as in the sprintf function,
 *   There are however some limitations in the maximum length of the
 *   formatted string as well as in the conversion specifications. If
 *   any of these limitations are too limited then other means of
 *   formatting a string can and should be used. In all normal cases
 *   these limitation should however be of no concern.
 *
 *   A conversion specification must have the following syntax:
 *
 *      %[<width>][.<precision>]<conversion character>
 *
 *      ([] specifies an optional item)
 *
 *   The conversion character must be one of:
 *
 *      d   Signed decimal
 *      u   Unsigned decimal
 *      x   Hexadecimal (uses the characters 'abcdef')
 *      X   Hexadecimal (uses the characters 'ABCDEF')
 *      f   Float (Limited formatting capability. Use with care.)
 *      s   String
 *      c   Character
 *      %   The '%' character itself
 *
 * Example:
 *   STR("Decimal value = %d", value)
 *   STR("Hex value 1 = %x, Hex value 2 = %X", hex1, hex2)
 *   STR("The string = %s", str)
 */
#include <stdbool.h>
/*
** Maximum number of digits for a 32 bit number is 10
** plus a sign and zero terminating character.
*/
#define BUFLEN 12

static char digitsLC[] = "0123456789abcdef";
static char digitsUC[] = "0123456789ABCDEF";

/*
** Maximum precision
*/
#define MAXPREC 10

static double floatShift[MAXPREC] =
{
  1.0,
  10.0,
  100.0,
  1000.0,
  10000.0,
  100000.0,
  1000000.0,
  10000000.0,
  100000000.0,
  1000000000.0,
};
static int intShift[MAXPREC] =
{
  1,
  10,
  100,
  1000,
  10000,
  100000,
  1000000,
  10000000,
  100000000,
  1000000000
};

/*     Macro: SCONV
**
** Description:
**   Macro to convert a signed number into a string.
**
** Parameters:
**   VAL    Value to convert.
**
**   BASE   Base for conversion, must be <= 16.
**
**   WIDTH  Field width.
**
**   PAD    Padding character used when padding the value
**          to WIDTH characters width.
**
**   CHECK  A boolean expression to check if the value to
**          convert is negative or not.
**
*/
#define SCONV(VAL, BASE, WIDTH, PAD, DIGITS, CHECK)\
{\
  neg = false;\
  if (CHECK)\
  {\
    neg = true;\
    VAL = -VAL;\
  }\
  i = BUFLEN;\
  do\
  {\
    buf[--i] = DIGITS[VAL % BASE];\
    VAL /= BASE;\
  } while (VAL && i);\
  if (neg && i)\
  {\
    buf[--i] = '-';\
  }\
  while (len < maxLen && WIDTH-- > BUFLEN - i)\
  {\
    *s++ = PAD;\
    len++;\
  }\
  while (len < maxLen && i < BUFLEN)\
  {\
    *s++ = buf[i++];\
    len++;\
  }\
}

/*     Macro: UCONV
**
** Description:
**   Macro to convert an unsigned number into a string.
**
** Parameters:
**   VAL    Value to convert.
**
**   BASE   Base for conversion, must be <= 16.
**
**   WIDTH  Field width.
**
**   PAD    Padding character used when padding the value
**          to WIDTH characters width.
**
**
*/
#define UCONV(VAL, BASE, WIDTH, PAD, DIGITS)\
{\
  i = BUFLEN;\
  do\
  {\
    buf[--i] = DIGITS[VAL % BASE];\
    VAL /= BASE;\
  } while (VAL && i);\
  while (len < maxLen && WIDTH-- > BUFLEN - i)\
  {\
    *s++ = PAD;\
    len++;\
  }\
  while (len < maxLen && i < BUFLEN)\
  {\
    *s++ = buf[i++];\
    len++;\
  }\
}

static int _fmt_str(
		char         *s,          /* I: The buffer to print to           */
		int           maxLen,     /* I: The maximum length of the buffer */
		const char   *format,     /* I: The format string                */
		va_list args)             /* I: The argument list                */
{
	char        buf[BUFLEN];
	int         i;
	int         len;
	const char *f;
	int         width;
	int         prec;
	bool        neg;
	char        pad;

	maxLen--;

	for (f = format, len = 0; *f && len < maxLen; f++)
	{
		if (*f != '%')
		{
			*s++ = *f;
			len++;
		}
		else
		{
			prec = 0;
			width = 0;
			pad = ' ';
			f++;
			if (*f == '0')
			{
				pad = '0';
			}
			while (*f >= '0' && *f <= '9')
			{
				width = width * 10 + *f++ - '0';
			}

			if (*f == '.')
			{
				/*
				 ** We have got a precision
				 */
				f++;
				while (*f >= '0' && *f <= '9')
				{
					prec = prec * 10 + *f++ - '0';
				}
			}
			else if (*f == 'f')
			{
				/*
				 ** No precision given. Use default precision for float...
				 */
				prec = 6;
			}

			switch (*f)
			{
			case '%':
				*s++ = '%';
				len++;
				break;

			case 'd':
			{
				int iVal = va_arg(args, int);

				SCONV(iVal, 10, width, pad, digitsLC, iVal < 0);
				break;
			}

			case 'u':
			{
				unsigned int uVal = (unsigned int) va_arg(args, int);

				UCONV(uVal, 10, width, pad, digitsLC);
				break;
			}

			case 'x':
			{
				unsigned int uVal = (unsigned int) va_arg(args, int);

				UCONV(uVal, 16, width, pad, digitsLC);
				break;
			}

			case 'X':
			{
				unsigned int uVal = (unsigned int) va_arg(args, int);

				UCONV(uVal, 16, width, pad, digitsUC);
				break;
			}

			case 'f':
			{
				double dVal = va_arg(args, double);
				double fPart;
				double iPart;
				int iVal;
				int fVal;

				if (prec == 0)
				{
					/*
					 ** No precision specified. Add, or remove, 0.5
					 ** to get more accurate round off...
					 */
					if (dVal < 0.0)
					{
						iVal = (int)(dVal - 0.5);
					}
					else
					{
						iVal = (int)(dVal + 0.5);
					}

					SCONV(iVal, 10, width, ' ', digitsLC, dVal < 0.0);
				}
				else if (prec > 0)
				{
					width -= prec + 1;

					/*
					 ** Split into a fractional part and an integer part.
					 */
					fPart = modf(dVal, &iPart);
					iVal = (int)iPart;

					if (prec >= MAXPREC)
					{
						prec = MAXPREC - 1;
					}
					fVal = (int)(fPart * floatShift[prec] + 0.5);

					if (fVal >= intShift[prec])
					{
						/*point
						 ** The fractional part has been rounded up and has become
						 ** larger than the specified precision. Increase, or decrease,
						 ** the integer part, and reset the fractional part.
						 */
						if (dVal < 0.0)
						{
							iVal--;
						}
						else
						{
							iVal++;
						}
						fVal = 0;
					}

					SCONV(iVal, 10, width, ' ', digitsLC, dVal < 0.0);

					*s++ = '.';
					len++;

					UCONV(fVal, 10, prec, '0', digitsLC);
				}
				break;
			}

			case 's':
			{
				char *sVal = va_arg(args, char *);
				int strLen;

				if (! sVal)
				{
					sVal = "(null)";
				}
				strLen = (int)strlen(sVal);
				while (len < maxLen && width > strLen)
				{
					width--;
					*s++ = ' ';
					len++;
				}
				while (len < maxLen && *sVal)
				{
					*s++ = *sVal++;
					len++;
				}
				break;
			}

			case 'c':
			{
				while (len < maxLen && width > 1)
				{
					width--;
					*s++ = ' ';
					len++;
				}
				*s++ = (char)va_arg(args, int);
				len++;
				break;
			}

			default:
				*s++ = '%';
				len++;
				break;
			}
		}
	}

	*s = '\0';
	return len;
}

char *tpt_format_str2(const char *format, ...)
{
	static __thread char internal_trace_buf[TPT_MAX_TRACE_LEN] = {0};
	va_list args;  /* Argument list */

	va_start(args, format);

	(void)_fmt_str(
			internal_trace_buf,
			sizeof(internal_trace_buf),
			format,
			args);

	va_end(args);

	return internal_trace_buf;
}

const char *tpt_short_fname(const char *fname)
{
        const char *fileName = strrchr(fname,'/');
        return fileName ? fileName + 1 : fname;
}
