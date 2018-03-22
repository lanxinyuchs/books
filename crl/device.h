/* COPYRIGHT-ENEA-SRC-R2 *
 **************************************************************************
 * Copyright (C) 2004-2006 by Enea Software AB.
 * All rights reserved.
 *
 * This Software is furnished under a software license agreement and
 * may be used only in accordance with the terms of such agreement.
 * Any other use or reproduction is prohibited. No title to and
 * ownership of the Software is hereby transferred.
 *
 * PROPRIETARY NOTICE
 * This Software consists of confidential information.
 * Trade secret law and copyright law protect this Software.
 * The above notice of copyright on this Software does not indicate
 * any actual or intended publication of such Software.
 **************************************************************************
 * COPYRIGHT-END */
/**
 * @toc Device_Drivers:
 *
 * @file ose_spi/device.h
 *
 * @brief Device Driver and Device Client Interface Description.
 *
 */
#ifndef _DEVICE_H
#define _DEVICE_H
#include "osetypes.h"
#include "sys/uio.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @macro DEVICE_VERSION
 *
 * @brief The version number of the current device driver interface.
 *
 * @long Version 3 includes ddWritev/ddStop functions and some new
 * ddDevInfo/ddUnitInfo/ddGetConf/ddSetConf tags.
 *
 * @long Version 4 includes hardware acceleration features.
 */
#ifndef DEVICE_VERSION
#define DEVICE_VERSION  4
#endif

/**
 * @macro DEVICE_CLASS
 *
 * @brief A unique number that will be returned by device drivers when called
 * with the DEVICE_FCDEVCLASS function code.
 */
#define DEVICE_CLASS    0x2A86F264

/*
 *===========================================================================
 *                         DEVICE FUNCTION CODES
 *===========================================================================
 * Function codes to be used in biosCall().
 */

#define DEVICE_FCBASE            0x72957209

#define DEVICE_FCDEVCLASS        (DEVICE_FCBASE + 0)

#define DEVICE_FCINIT            (DEVICE_FCBASE + 1)
#define DEVICE_FCFUNCTIONS       (DEVICE_FCBASE + 2)

#define DEVICE_FCDEVINFO         (DEVICE_FCBASE + 3)
#define DEVICE_FCUNITINFO        (DEVICE_FCBASE + 4)

#define DEVICE_FCOPEN            (DEVICE_FCBASE + 5)
#define DEVICE_FCSTART           (DEVICE_FCBASE + 6)
#define DEVICE_FCCLOSE           (DEVICE_FCBASE + 7)

#define DEVICE_FCGETCONF         (DEVICE_FCBASE + 8)
#define DEVICE_FCSETCONF         (DEVICE_FCBASE + 9)

#define DEVICE_FCWRITE           (DEVICE_FCBASE + 10)
#define DEVICE_FCREAD            (DEVICE_FCBASE + 11)
#define DEVICE_FCSIMINT          (DEVICE_FCBASE + 12)

#define DEVICE_FCABORTWRITE      (DEVICE_FCBASE + 13)
#define DEVICE_FCABORTREAD       (DEVICE_FCBASE + 14)

#define DEVICE_FCWRITEV          (DEVICE_FCBASE + 15)
#define DEVICE_FCSTOP            (DEVICE_FCBASE + 16)
#define DEVICE_FCOPENSHARED      (DEVICE_FCBASE + 17)

/* Function codes with a value of 0x80000000 and above are reserved for
 * customer additions.
 */
#define DEVICE_FCUSER          	 0x80000000
    
/**
 * @macro DEVICE_EFATAL_MASK
 * @brief If this bit is set in dcHandleEX() 'errorCode' the driver must be
 * closed. NEW VERSION 2.
 */
#define DEVICE_EFATAL_MASK       0x80000000

/*
 * ErrorCodes
 */

/**
 * @macro DEVICE_EUNKNOWN
 *
 * @brief Request was unknown, can be returned by either BIOS (unknown
 * handle) or device driver (unknown function code).
 */
#define DEVICE_EUNKNOWN               -1

/**
 * @macro DEVICE_SUCCESS
 * @brief Operation was performed successfully.
 */
#define DEVICE_SUCCESS                 0

/**
 * @macro DEVICE_EDEVICE_NOT_FOUND
 * @brief Device hardware not found, device is unusable.
 */
#define DEVICE_EDEVICE_NOT_FOUND       1

/**
 * @macro DEVICE_EILLEGAL_UNIT
 * @brief Operation specified illegal (non existing) unit.
 */
#define DEVICE_EILLEGAL_UNIT           2

/**
 * @macro DEVICE_EILLEGAL_DEVTYPE
 * @brief Device does not support (implement) specified device type.
 */
#define DEVICE_EILLEGAL_DEVTYPE        3

/**
 * @macro DEVICE_EALREADY_OPENED
 * @brief Device is already opened (and must be closed before opened again).
 */
#define DEVICE_EALREADY_OPENED         4

/**
 * @macro DEVICE_ENOT_OPENED
 * @brief Device is not opened (and cannot be used until opened).
 */
#define DEVICE_ENOT_OPENED             5

/**
 * @macro DEVICE_EALREADY_STARTED
 * @brief Device is already started.
 */
#define DEVICE_EALREADY_STARTED        6

/**
 * @macro DEVICE_ENOT_STARTED
 * @brief Device is not started (and cannot be used until started).
 */
#define DEVICE_ENOT_STARTED            7

/**
 * @macro DEVICE_EHARDWARE_ERROR
 * @brief Device hardware did not perform as expected.
 */
#define DEVICE_EHARDWARE_ERROR         8

/**
 * @macro DEVICE_EFUNCTION_NOT_AVAILABLE
 * @brief Function not available (not implemented).
 */
#define DEVICE_EFUNCTION_NOT_AVAILABLE 9

/**
 * @macro DEVICE_EINVALID_TAG
 * @brief An unknown tag was found in a ddSetConf/ddGetConf tag list.
 */
#define DEVICE_EINVALID_TAG           10

/**
 * @macro DEVICE_EINVALID_VALUE
 * @brief A tag (to ddGetConf/ddSetConf) had an invalid value.
 */
#define DEVICE_EINVALID_VALUE         11

/**
 * @macro DEVICE_EQUEUE_FULL
 * @brief Output queue full, failed to write.
 */
#define DEVICE_EQUEUE_FULL            12

/**
 * @macro DEVICE_ENO_BUFFERS
 * @brief Failed to allocate buffer, failed to read.
 */
#define DEVICE_ENO_BUFFERS            13

/**
 * @macro DEVICE_EINVALID_LENGTH
 * @brief Invalid length in read or write request.
 */
#define DEVICE_EINVALID_LENGTH        15

/**
 * @macro DEVICE_EOVERFLOW_RX
 * @brief Receive buffer overflowed.
 */
#define DEVICE_EOVERFLOW_RX           16

/**
 * @macro DEVICE_EINIT
 * @brief Bad initialization of the driver.
 */
#define DEVICE_EINIT                  17

/**
 * @macro DEVICE_ELINK_DOWN
 * @brief Physical link can not be established on this media
 */
#define DEVICE_ELINK_DOWN             18

/**
 * @macro DEVICE_EHWADDR_NOT_SET
 * @brief Ethernet address not set
 */
#define DEVICE_EHWADDR_NOT_SET        19

/*
 * GenericTags
 */

/**
 * @macro DEVICE_TAGEND
 * @brief This tag signifies the end of a tag list.
 */
#define DEVICE_TAGEND       -1L

/**
 * @macro DEVICE_TAGERRPTR
 *
 * @brief This tag's parameter is a pointer to a DevTag variable that will
 * contain a pointer to the tag that was unknown or had an invalid value. If no
 * error is detected in the tag list the variable will be set to NULL
 */
#define DEVICE_TAGERRPTR    -2L

/*
 * DeviceTags
 */

/**
 * @macro DEVICE_TAGD_UNITLO
 * @brief Return the lowest numbered unit.
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_UNITLO			0x100

/**
 * @macro DEVICE_TAGD_UNITHI
 * @brief Return the highest numbered unit.
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_UNITHI			0x101

/**
 * @macro DEVICE_TAGD_OPENALL
 * @brief Must client open all units at once? Needed by some old hardware where
 * the units cannot be operated separately.
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_OPENALL			0x102

/**
 * @macro DEVICE_TAGD_NAME
 * @brief Return the device name in the specified buffer. Buffer must be
 * minimum 32 characters large.
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_NAME			0x103

/**
 * @macro DEVICE_TAGD_VERSION
 * @brief Return the driver version.
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_VERSION			0x104

/**
 * @macro DEVICE_TAGD_FEATURES
 *
 * @brief This tag is used to query the device for supported features. The
 * value of the tag returned from the driver will be a bit-field indicating
 * which features are supported.
 *
 * @seealso ddDevInfo_t
 */
#define DEVICE_TAGD_FEATURES                    0x105

/*
 * Features (feature word to DEVICE_TAGD_FEATURES).
 */

/**
 * @macro DEVICE_FEATURE_DDREAD
 * @brief Set if the driver implements software flow control using ddRead
 * (dcAlloc may return NULL, ddRead must be called to supply new RX buffers).
 * @seealso DEVICE_TAGD_FEATURES
 */
#define DEVICE_FEATURE_DDREAD          0x00000001

/**
 * @macro DEVICE_FEATURE_DDSTOP
 * @brief Set if the driver implements ddStop().
 * @seealso DEVICE_TAGD_FEATURES
 */
#define DEVICE_FEATURE_DDSTOP          0x00000002

/**
 * @macro DEVICE_FEATURE_DDWRITEV
 * @brief Set if the driver implements gather write using the call ddWritev.
 * @seealso DEVICE_TAGD_FEATURES
 */
#define DEVICE_FEATURE_DDWRITEV        0x00000004

/**
 * @macro DEVICE_FEATURE_DDOPENSHARED
 * @brief Set if the driver implements opening it in shared mode (for use by
 * multiple clients) using the call ddOpenShared.
 * @seealso DEVICE_TAGD_FEATURES
 */
#define DEVICE_FEATURE_DDOPENSHARED    0x00000008

/*
 * UnitTags
 */

/**
 * @macro DEVICE_TAGU_DEVTYPES
 * @brief Return a 32 bit device type value for each supported device type.
 * CHANGED VERSION 2.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_DEVTYPES                    0x200

/**
 * @macro DEVICE_TAGU_INUSE
 * @brief Return non zero value if unit is open.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_INUSE			0x201

/**
 * @macro DEVICE_TAGU_MAX_MTU
 * @brief Return the hardware/driver limitation on MTU.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_MAX_MTU			0x202

/**
 * @macro DEVICE_TAGU_MAX_MRU
 * @brief Return the hardware/driver limitation on MRU.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_MAX_MRU			0x203

/**
 * @macro DEVICE_TAGU_NUMDEVTYPES
 * @brief Return the number of supported device types. NEW VERSION 2.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_NUMDEVTYPES                 0x204

/**
 * @macro DEVICE_TAGU_NUMRX_FRAMES
 * @brief Return the number of RX frames for the specified framesize. VERSION 3.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_NUMRX_FRAMES                0x205

/**
 * @macro DEVICE_TAGU_NUMTX_FRAMES
 * @brief Return the number of TX frames for the specified framesize. VERSION 3.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_NUMTX_FRAMES                0x206

/**
 * @macro DEVICE_TAGU_RXBUF_ALIGNMENT
 * @brief Return the alignment requirement on RX buffers. VERSION 3.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_RXBUF_ALIGNMENT             0x207

/**
 * @macro DEVICE_TAGU_TXBUF_ALIGNMENT
 * @brief Return the alignment requirement on TX buffers. VERSION 3.
 * @seealso ddUnitInfo_t
 */
#define DEVICE_TAGU_TXBUF_ALIGNMENT             0x208

/*
 * ConfigurationTags
 */

/**
 * @macro DEVICE_TAGC_INTERRUPTS
 * @brief Enable (1) or disable (0) hardware interrupts for this unit.
 */
#define DEVICE_TAGC_INTERRUPTS                  0x300

/**
 * @macro DEVICE_TAGC_MIN_RU_ONE
 * @brief Enable (1) or disable (0) minimum receive unit one feature.
 */
#define DEVICE_TAGC_MIN_RU_ONE                  0x302


#ifndef _DEVICE_TYPES
#define _DEVICE_TYPES
typedef S32 DevTag;
typedef U32 DevErr;
typedef U32 DevBool;
#endif

/*
 * Flags to dcHandleTX
 */

/**
 * @macro DEVTX_FAILED_WRITES
 * @brief Bitmask for number of failed writes (because queue full). VERSION 3.
 * @seealso dcHandleTX_t
 */
#define DEVTX_FAILED_WRITES       0x0000000f

/**
 * @macro DEVTX_TRANSMIT_ERROR
 * @brief If this bit is set in dcHandleTX() 'failed_writes', the driver failed
 * to transmit the frame. New in version 3.
 * @seealso dcHandleTX_t
 */
#define DEVTX_TRANSMIT_ERROR       0x80000000

/**
 * @type ISRInfo
 *
 * @brief ISRInfo contains information about the Interrupt Service Routines
 * (ISR's) needed by this driver.
 *
 * @field numISRs Number of elements valid in the ISR array below.
 *
 * @field ISR Array of ISR information used by the device client when creating
 * the interrupt process.
 *
 * @field ISR.entrypoint Entry point for the Interrupt Service Routine.
 *
 * @field ISR.priority Priority this interrupt operates on. This value should
 * be from 0 (highest) to 31 (lowest) priority.
 *
 * @field ISR.stacksize The stack size (in bytes) that this ISR needs.
 *
 * @field ISR.vector The vector number the device hardware generates on interrupt.
 */
struct ISRInfo
{
    U32 numISRs;

    struct
    {
	void (*entrypoint) (void);
	U32 priority;
	U32 stacksize;
	S32 vector;
    } ISR[4];
};

/**
 * @type dcAlloc_t
 *
 * @brief Callback function to the device client. Used by the device driver to
 * request receive buffers.
 *
 * @input dcObject Opaque object specified in ddOpen.
 * @input buflen Requested length of receive buffer.
 */
typedef void *(*dcAlloc_t)(void *dcObject, U32 buflen);

/**
 * @type dcFree_t
 *
 * @brief Callback function to the device client. Used by the device to free an
 * receive buffer previously allocated with dcAlloc().
 *
 * @input dcObject Opaque object specified in ddOpen.
 * @input buf Pointer to receive buffer.
 */
typedef void (*dcFree_t)(void *dcObject, void *buf);

/**
 * @type dcHandleEX_t
 *
 * @brief Callback function to the device client. Used by the device to
 * asynchronously report an error condition.
 *
 * @input dcObject Opaque object specified in ddOpen.
 * @input errorCode Code indicating the type of error.
 */
typedef void (*dcHandleEX_t)(void *dcObject, DevErr errorCode);

/**
 * @type dcHandleRX_t
 *
 * @brief Callback function to the device client. Used by the device driver to
 * deliver a received buffer. The buffer has previously been allocated by a
 * call to dcAlloc().
 *
 * @input dcObject Opaque object specified in ddOpen.
 * @input buf Pointer to receive buffer.
 * @input buflen Number of (valid) bytes in buffer.
 * @input flags Bitfield with status information about this frame.
 */
typedef void (*dcHandleRX_t)(void *dcObject, void *buf, U32 buflen, U32 flags);

/**
 * @type dcHandleTX_t
 * 
 * @brief Callback function to the device client. Used by the device driver to
 * return a transmitted buffer (previously passed to ddWrite()) when it has
 * been transmitted.
 *
 * @input dcObject Opaque object specified in ddOpen.
 * @input buf Pointer to transmit buffer.
 * @input buflen Number of bytes transmitted.
 * @input flags Bitfield with status information about this frame.
 */
typedef void (*dcHandleTX_t)(void *dcObject, void *buf, U32 buflen, U32 flags);

/**
 * @type DcFuncs
 *
 * @brief Device client call-back functions.
 *
 * @field dcAlloc    Function pointer to dcAlloc() callback.
 * @field dcFree     Function pointer to dcFree() callback.
 * @field dcHandleEX Function pointer to dcHandleEX() callback.
 * @field dcHandleRX Function pointer to dcHandleRX() callback.
 * @field dcHandleTX Function pointer to dcHandleTX() callback.
 */
struct DcFuncs
{
    dcAlloc_t    dcAlloc;
    dcFree_t     dcFree;
    dcHandleEX_t dcHandleEX;
    dcHandleRX_t dcHandleRX;
    dcHandleTX_t dcHandleTX;
};


/**
 * @type ddDevInfo_t
 *
 * @brief Returns information about the device or driver.
 *
 * @input tagdList A pointer to a tag list, terminated by the DEVICE_TAGEND tag.
 *
 * @return Status of the operation.
 */
typedef DevErr (*ddDevInfo_t)(DevTag *tagdList);

/**
 * @type ddUnitInfo_t
 *
 * @brief Returns information about the specified unit.
 *
 * @input unit Device unit specifier.
 *
 * @input taguList A pointer to a tag list, terminated by the DEVICE_TAGEND tag.
 *
 * @return Status of the operation.
 */
typedef DevErr (*ddUnitInfo_t)(U32 unit, DevTag *taguList);

/**
 * @type ddOpen_t
 *
 * @brief Open the specified unit with the specified type.
 *
 * @input unit Device unit specifier.
 *
 * @input deviceType Requested type of device.
 *
 * @input dcObjects The client opaque object to be used in the call-backs.
 *
 * @output dcFuncs Pointer to a structure to be filled with pointers to the
 * drivers static functions.
 *
 * @output isrInfo Pointer to a structure to be filled with information about
 * the ISR this unit needs.  Specify NULL for polled mode.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS Operation completed successfully.
 * @error DEVICE_EILLEGAL_UNIT An illegal unit was specified.
 * @error DEVICE_EILLEGAL_DEVTYPE The specified device type is not supported. 
 * @error DEVICE_EALREADY_OPENED
 * @error DEVICE_EFUNCTION_NOT_AVAILABLE Polled mode not implemented.
 */
typedef DevErr (*ddOpen_t)(U32 unit, U32 deviceType, void *dcObjects,
                           const struct DcFuncs *dcFuncs,
                           struct ISRInfo *isrInfo);

/**
 * @type ddOpenShared_t
 *
 * @brief Open the specified unit in multiple client mode with the specified type.
 *
 * @input unit Device unit specifier.
 *
 * @input deviceType Requested type of device.
 *
 * @input dcObjects The client opaque object to be used in the call-backs.
 *
 * @output dcFuncs Pointer to a structure to be filled with pointers to the
 * drivers static functions.
 *
 * @output isrInfo Pointer to a structure to be filled with information about
 * the ISR this unit needs.  Specify NULL for polled mode.
 *
 * @output virtUnit Pointer to an integer to be written with the allocated
 * virtual unit number to be used in forthcoming dd-calls.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS Operation completed successfully.
 * @error DEVICE_EILLEGAL_UNIT An illegal unit was specified.
 * @error DEVICE_EILLEGAL_DEVTYPE The specified device type is not supported. 
 * @error DEVICE_EALREADY_OPENED
 * @error DEVICE_EFUNCTION_NOT_AVAILABLE Polled mode not implemented.
 */
typedef DevErr (*ddOpenShared_t)(U32 unit, U32 deviceType, void *dcObjects,
                                 const struct DcFuncs *dcFuncs,
                                 struct ISRInfo *isrInfo, U32 *virtUnit);

/**
 * @type ddStart_t
 *
 * @brief Start the specified unit, enabling transmission and reception.
 *
 * @input unit Device unit specifier.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS
 * @error DEVICE_EILLEGAL_UNIT
 * @error DEVICE_ENOT_OPENED
 * @error DEVICE_EALREADY_STARTED
 */
typedef DevErr (*ddStart_t)(U32 unit);

/**
 * @type ddClose_t
 *
 * @brief Close the specified unit, thus returning allocated buffers and
 * disabling the interrupts for the hardware etc.
 *
 * @input unit Device unit specifier.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS
 * @error DEVICE_EILLEGAL_UNIT
 * @error DEVICE_ENOT_OPENED
 */
typedef DevErr (*ddClose_t)(U32 unit);

/**
 * @type ddGetConf_t
 *
 * @brief Obtain the configuration for the specified unit.
 *
 * @input unit Device unit specifier.
 *
 * @input tagcList A pointer to a tag list specifying the configurations items
 * whose values should be obtained.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS
 * @error DEVICE_EILLEGAL_UNIT
 * @error DEVICE_EINVALID_TAG
 */
typedef DevErr (*ddGetConf_t)(U32 unit, DevTag *tagcList);

/**
 * @type ddSetConf_t
 *
 * @brief Change the configuration for the specified unit.
 *
 * @input unit Device unit specifier.
 *
 * @input tagcList A pointer to a tag list specifying the configuration item to
 * change and their new values.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS
 * @error DEVICE_EILLEGAL_UNIT
 * @error DEVICE_EINVALID_VALUE
 * @error DEVICE_EINVALID_TAG
 */
typedef DevErr (*ddSetConf_t)(U32 unit, DevTag *tagcList);

/**
 * @type ddWrite_t
 *
 * @brief Submit a buffer for transmittion.
 *        The buffer will be returned in a later dcHandleTX call.
 *
 * @input unit Unit to transmit on.
 *
 * @input buf Pointer to buffer to transmit.
 *
 * @input buflen Length of buffer to transmit.
 *
 * @return Status of the operation.
 *
 * @error DEVICE_SUCCESS
 * @error DEVICE_EILLEGAL_UNIT
 * @error DEVICE_ENOT_OPENED
 * @error DEVICE_ENOT_STARTED
 * @error DEVICE_EINVALID_LENGTH buflen zero or larger than MTU.
 * @error DEVICE_EQUEUE_FULL	internal (hardware) queue is full
 * @error DEVICE_EFUNCTION_NOT_AVAILABLE function not available (not implemented)
 */
typedef DevErr (*ddWrite_t)(U32 unit, void *buf, U32 *buflen);

/**
 * @type ddRead_t
 *
 * @brief Submit an RX buffer (as returned from dcAlloc()) for reception.
 *        The buffer will be returned in a later dcHandleRX call.
 *        ddRead() provides a method for an application to control the flow
 *        of incoming frames.
 *
 * @input unit Unit to receive on.
 * @input buf Pointer to RX buffer.
 * @input buflen Length of RX buffer, most be at least the MRU of the device.
 */
typedef DevErr (*ddRead_t)(U32 unit, void *buf, U32 *buflen);

/**
 * @type ddSimInt_t
 *
 * @brief Simulate an interrupt on the device (i.e. execute the driver ISR's).
 *
 * @input unit Unit to simulate interrupt on.
 */
typedef DevErr (*ddSimInt_t)(U32 unit);

/**
 * @type ddAbortWrite_t
 *
 * @brief Deprecated.
 *
 * @input unit
 */
typedef DevErr (*ddAbortWrite_t)(U32 unit);

/**
 * @type ddAbortRead_t
 *
 * @brief Deprecated.
 *
 * @input unit
 *
 */
typedef DevErr (*ddAbortRead_t)(U32 unit);

/**
 * @type ddWritev_t
 *
 * @brief Submit an array of buffers for transmission as one frame.
 *        The cookie will be returned in a later dcHandleTX call.
 *
 * @input unit Unit to transmit on.
 * @input vector Pointer to array of buffers and buffer lengths.
 * @input count Number of elements in vector.
 * @input cookie Value to return in dcHandleTX.
 * @input flags Device type specific flags.
 * @input wparam Device type specific parameters.
 */
typedef DevErr (*ddWritev_t)(U32 unit, const struct iovec *vector, U32 count,
                             void *cookie, U32 flags, void *wparam);

/**
 * @type ddStop_t
 *
 * @brief Stop a device unit. It can be configured and started again.
 *
 * @input unit Unit to stop.
 */
typedef DevErr (*ddStop_t)(U32 unit);

/**
 * @type DdFuncs
 *
 * @brief Device driver public programming interface.
 *
 * @long A client executing in supervisor mode might want to bypass the trap
 * interface for performance reasons. These functions can be obtained with the
 * DEVICE_FCFUNCTIONS BIOS call and can be called direct from the client.
 *
 * @field ddDevInfo Function pointer to ddDevInfo() function.
 * @field ddUnitInfo Function pointer to ddUnitInfo() function.
 * @field ddOpen Function pointer to ddOpen() function.
 * @field ddStart Function pointer to ddStart() function.
 * @field ddClose Function pointer to ddClose() function.
 * @field ddGetConf Function pointer to ddGetConf() function.
 * @field ddSetConf Function pointer to ddSetConf() function.
 * @field ddWrite Function pointer to ddWrite() function.
 * @field ddRead Function pointer to ddRead() function.
 * @field ddSimInt Function pointer to ddSimInt() function.
 * @field ddAbortWrite Function pointer to ddAbortWrite() function.
 * @field ddAbortRead Function pointer to ddAbortRead() function.
 * @field ddWritev Function pointer to ddWritev() function.
 * @field ddStop Function pointer to ddStop() function.
 * @field ddOpenShared Function pointer to ddOpenShared() function.
 */
struct DdFuncs
{
   ddDevInfo_t    ddDevInfo;
   ddUnitInfo_t   ddUnitInfo;
   ddOpen_t       ddOpen;
   ddStart_t      ddStart;
   ddClose_t      ddClose;
   ddGetConf_t    ddGetConf;
   ddSetConf_t    ddSetConf;
   ddWrite_t      ddWrite;
   ddRead_t       ddRead;
   ddSimInt_t     ddSimInt;
   ddAbortWrite_t ddAbortWrite;
   ddAbortRead_t  ddAbortRead;
   ddWritev_t     ddWritev;
   ddStop_t       ddStop;
   ddOpenShared_t ddOpenShared;
};


#ifdef DEVICE_DRIVER

static U32    ddDevClass(void);
static DevErr ddDevInfo(DevTag *);
static DevErr ddUnitInfo(U32, DevTag *);
static DevErr ddOpen(U32, U32, void *, const struct DcFuncs *, struct ISRInfo *);
static DevErr ddClose(U32);
static DevErr ddSetConf(U32, DevTag *);
static DevErr ddGetConf(U32, DevTag *);
static DevErr ddStart(U32);
static DevErr ddWrite(U32, void *, U32 *);
static DevErr ddRead(U32, void *, U32 *);
static DevErr ddSimInt(U32);
static DevErr ddAbortWrite(U32);
static DevErr ddAbortRead(U32);

#endif /* DEVICE_DRIVER */

#ifdef __cplusplus
}
#endif

#endif /* _DEVICE_H */
