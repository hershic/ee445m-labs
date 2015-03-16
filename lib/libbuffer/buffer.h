/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson 2015-03-04 */
/* Revision history: Look in Git FGT */
#ifndef __BUFFER__
#define __BUFFER__

/*! \addtogroup Buffer Buffer management macros
 * @{
 */

/*! A buffer consists of storage and metadata. Specifically,
 * - the storage of a buffer is an array of any type; and
 * - the metadata of a buffer is
 *   - uint32_t of the same name suffixed with '_SIZE'
 *   - global maximum buffer length, set in this file
 */

#define BUFFER_MAX_LENGTH                      32
#define BUFFER_DEFAULT_DECREMENT_AMOUNT         1

/*! Initialize buffer and its metadata */
#define buffer_init(buf)			\
    buffer_len(buf) = 0;

/*! Add to buffer a single element.
 * \bug No warning on overflow failure
 */
#define buffer_add(buf, elt)				\
    do {						\
        if(buffer_len(buf) + 1 <= BUFFER_MAX_LENGTH) {  \
	    buf[buffer_len(buf)] = elt;			\
	    ++buffer_len(buf);                          \
        }                                               \
    } while (false)


/*! Decrement the number of characters in buffer by the standard
 *  amount. */
#define buffer_dec(buf)				\
    buffer_dec_(buf, BUFFER_DEFAULT_DECREMENT_AMOUNT)

/*! Decrement the number of characters in buffer by amount.
 * \note This macro needs to be called on its own line.
 * \bug No warning on underflow failure
 * \bug Non-atomic
 */
#define buffer_dec_(buf, amount)				\
    do {							\
        if(buffer_len(buf) - amount >= 0) {			\
	    buffer_len(buf) = buffer_len(buf) - amount;		\
        }							\
    } while (false)

/*! Access the last element in a buffer */
#define buffer_last(buf)			\
    buf[buffer_len(buf)-1]

/*! True if a buffer is full */
#define buffer_full(buf)			\
    (buf##_SIZE == BUFFER_MAX_LENGTH-1)

/*! True if a buffer is empty */
#define buffer_empty(buf)			\
    (buf##_SIZE == 0)

#define buffer_clear(buf)			\
    buffer_len(buf) = 0

/*! Terminate a buffer with a null character. */
#define buffer_null_terminate(buf)		\
    buf[buffer_len(buf)] = 0

/*! Evaluate to the length of a buffer. */
#define buffer_len(buf)				\
    buf##_SIZE

#endif

/* End Doxygen group
 * @}
 */
