/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __bufferpp__
#define __bufferpp__

#include <stdint.h>

/*! \addtogroup buffer
 * @{
 */

#define DEFAULT_BUFFER_LENGTH 64

class buffer {
private:
    /* Points ahead of last valid char */
    uint32_t pos;
    uint32_t len;

public:
    char buf[DEFAULT_BUFFER_LENGTH];

    buffer();

    /*! Add a char. */
    void add(const char ch);

    /*! Peek at a char. */
    char peek(void);

    /*! Remove a char. */
    char get(void);

    /*! Clear the buffer. */
    void clear(void);

    /*! True if buffer is full */
    bool full(void);

    /*! True if buffer is empty */
    bool empty(void);

    /*! Length of buffer contents. */
    uint32_t length(void);
};

#endif

/*! End doxygen group
 * @}
 */
