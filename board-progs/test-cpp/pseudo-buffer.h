#ifndef __pseudobufferpp__
#define __pseudobufferpp__

#define BUFFER_MAX_LENGTH 16

/*! True if a buffer is empty */
#define buffer_empty(buf)			\
    (buf##_SIZE == 0)

/*! Decrement the number of characters in buffer by the standard
 *  amount. */
#define buffer_dec(buf)				\
    buffer_dec_(buf, 1)

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

/*! Evaluate to the length of a buffer. */
#define buffer_len(buf)				\
    buf##_SIZE

/*! True if a buffer is full */
#define buffer_full(buf)			\
    (buf##_SIZE == BUFFER_MAX_LENGTH-1)

/*! True if a buffer is empty */
#define buffer_empty(buf)			\
    (buf##_SIZE == 0)

#define buffer_clear(buf)			\
    buffer_len(buf) = 0

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

#endif
