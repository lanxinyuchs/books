/**
 *
 * @brief
 *
 *      T is a translator between LINX and GLH Link Handler protocols.
 *
 *
 *      LINX Protocol SW                  GLH Protocol SW
 *                         ===========
 *                         |         |
 *      --- t_loc_tx() --> |         | --- t_rem_tx() -->
 *                         |    T    |
 *      <-- t_loc_rx() --- |         | <-- t_rem_rx() ---
 *                         |         |
 *                         ===========
 *
 *      In the picture above T is the protocol translator. The local LINX
 *      protocol SW resides on the left side and the remote GLH protocol SW
 *      resides on the right side. T is linked with the local LINX Connection
 *      Manager.
 *
 *      t_loc_tx() and t_rem_rx() are functions in T which provide input
 *      messages from LINX and GLH. t_loc_tx() is called from the CM when LINX
 *      transmits a message. t_rem_rx() is called from the CM when a complete
 *      message from GLH has been received.
 *
 *      t_loc_rx() and t_rem_tx() are callback functions in the CM. When input
 *      messages are processed in T output messages are provided to the CM by
 *      backcalls to t_loc_rx() and t_rem_tx(). A backcall to t_loc_rx() results
 *      in a message being delivered to local LINX. A backcall to t_rem_tx()
 *      results in a message being enqueued for transmission to remote GLH.
 *
 *      Each link has its own context stored separatly in memory. A new context
 *      is allocated whenever a link is connected. The CM calls T function
 *      t_open() before it indicates to LINX that the link is connected.
 *      t_open() returns a handle to the context which the CM uses with
 *      t_loc_tx() and t_rem_rx(). The CM may associate a context with an
 *      object pointer which can be used for efficient code in the two
 *      callback functions.
 *
 *      Whenever a link enters disconnected state the context is freed. The CM
 *      calls the function t_close() after the CM indicates to LINX that the
 *      link is disconnected.
 *
 *      If t_loc_tx() or t_rem_rx() returns something not zero then an error
 *      has occured and the link should then be reset by the CM.
 *
 *      OSE User numbers are either ignored or set to zero.
 *
 *      T supports only one thread per link context.
 *
 *      Parameters here are in host byte endian if not othwerwise stated.
 *
 */

#include <stdint.h>


/**
 * @brief
 *
 * Callback function in the CM which delivers a message to LINX on local side
 *
 * @param obj   Object pointer
 * @param src   LINX source address
 * @param dst   LINX destination address
 * @param size  LINX data size
 * @param data  LINX data pointer
 *
 * @return 0 on success
 *
 * @warning
 *
 * If the LINX implementation has an allocation function then the CM has to copy
 * the data to allocated memory before actually delivering it.
 *
 */

typedef int (*t_loc_rx)(void *obj, uint32_t src, uint32_t dst,
			uint32_t size, void *data);


/**
 * @brief
 *
 * Callback function in the CM which transmits a message to GLH on remote side
 *
 * @param obj   Object pointer
 * @param size  GLH data size
 * @param data  GLH data pointer
 *
 * @return 0 on success
 *
 */

typedef int (*t_rem_tx)(void *obj, uint32_t size, void *data);


/**
 * @brief
 *
 * Translates a LINX protocol message
 *
 * @param handle  Object pointer provided in t_open
 * @param src     LINX source address
 * @param dst     LINX destination address
 * @param size    LINX data size
 * @param dst     LINX data poiner
 *
 * @return 0 on success
 *
 * @warning
 *
 * Depending on the CPU architecture the data pointer might have to be
 * 4-byte aligned.
 *
 */

int t_loc_tx(void *handle, uint32_t src, uint32_t dst, uint32_t size,
	     void *data);


/**
 * @brief
 *
 * Translates a GLH protocol message
 *
 * @param handle  Object pointer provided in t_open
 * @param size    GLH data size
 * @param dst     GLH data poiner
 *
 * @return 0 on success
 *
 * @warning
 *
 * Depending on the CPU architecture the data pointer might have to be
 * 4-byte aligned.
 *
 */

int t_rem_rx(void *handle, uint32_t size, void *data);


/**
 * @brief
 *
 * Open the translator. This function should be called before it's indicated
 * to LINX that the link is connected.
 *
 * @param handle             Pointer to where handle pointer is stored
 * @param obj                Object pointer passed to callback functions
 * @param rx                 Callback function for receive on local side
 * @param tx                 Callback function for transmit to remote side
 * @param max_num_endpoints  Maximum number of endpoints to be handled
 *
 * @return 0 on success
 *
 */

int t_open(void **handle, void *obj, t_loc_rx rx, t_rem_tx tx,
	   uint32_t max_num_endpoints);


/**
 * @brief
 *
 * Close the translator. This function should be called after it's indicated
 * to LINX that the link is disconnected.
 *
 * @param handle  Pointer to where handle pointer is stored
 *
 * @return 0 on success
 *
 */

int t_close(void **handle);
