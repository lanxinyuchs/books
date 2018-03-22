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

/**
 *
 * @file     atfmi.h
 *
 * @brief    Auxiliary Transport Function Manager Interface
 *
 *           Interface for initiating and shutting down ATF related services
 *
 */

#ifndef ATFMI_H_
#define ATFMI_H_

/* > Includes *****************************************************************/

/* > Defines ******************************************************************/

#define ATFMI_MAX_PROFILE_NAME_SIZE  64

/**
 * @ingroup atfmi_result_t
 * Returned when the operation completed successfully.
 */
#define ATFMI_SUCCESS                            0

/**
 * @ingroup atfmi_result_t
 * Returned internal NVPI error occurs fails or
 * parameter for some action is invalid e.g. daemon starting
 */
#define ATFMI_INVALID_PARAMETER                  1

/**
 * @ingroup atfmi_result_t
 * Returned when server is in wrong state, usually when it dies
 */
#define ATFMI_WRONG_STATE                        2

/**
 * @ingroup atfmi_result_t
 * Returned when unsuported signal is received
 */
#define ATFMI_UNSUPPORTED                        3

/**
 * @ingroup atfmi_result_t
 * Returned for other miscellaneous error
 */
#define ATFMI_OTHER_ERROR                        4

/* > Type Declarations ********************************************************/

/**
 * @defgroup atfmi_result_t atfmi_result_t
 * @brief Return codes.
 */
/**
 * @ingroup atfmi_result_t
 * @brief Return codes.
 */
typedef uint32_t atfmi_result_t;

/**
 * @brief
 *
 * This function will search for the specified profile and start ATF services
 * according to the found profile.
 *
 * The specified profile must be located in the board parameters as a node.
 *
 * @param profile  The profile
 *
 * @return ATFMI_SUCCESS on success, other value on failure.
 *
 * @b Example:
 @code

 #include <atfmi.h>

 void example()
 {
        // Start ATF Services described in some profile.
        if (atfmi_init("/sys/atf/some-profile") != ATFMI_SUCCESS) {
                 // Error handling here.
        }
 }

 @endcode
 *
 */

atfmi_result_t atfmi_init(const char *profile);

/**
 * @brief
 *
 * This function will shutdown ATF services eariler initiated with atfmi_init().
 *
 * @param profile  The profile
 *
 * @return ATFMI_SUCCESS on success, other value on failure.
 *
 */

atfmi_result_t atfmi_shutdown(const char *profile);

#endif
