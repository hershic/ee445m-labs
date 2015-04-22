/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __bufferpp__
#define __bufferpp__

#include <stdint.h>
#include <stdbool.h>

#include "semaphorepp.hpp"

/*! \addtogroup buffer
 * @{
 */

#define DEFAULT_BUFFER_LENGTH 64

class buffer {
private:
    /* Points ahead of last valid data. */
    uint32_t pos;
    /* Total length of bufer. */
    uint32_t len;
    /* Optional notification system */
    semaphore sem;
    /* Number of overflows */
    uint32_t error_overflow;
    /* Number of underflows */
    uint32_t error_underflow;

    /*! Internal init protocol */
    void init(void);

public:
    int8_t buf[DEFAULT_BUFFER_LENGTH];

    buffer();
    buffer(semaphore sem);

    /* Not possible without malloc */
    /* buffer(int32_t length); */
    /* buffer(int32_t length, semaphore sem); */

    /*! Add data. */
    bool add(const int8_t data);

    /*! Add data and notify via semaphore. */
    void notify(const int8_t data);

    /*! Peek at an int8_t. */
    int8_t peek(void);

    /*! Remove an int8_t. */
    int8_t get(void);

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
