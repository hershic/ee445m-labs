/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __bufferpp__
#define __bufferpp__

#include <stdint.h>

#include "semaphorepp.hpp"

/*! \addtogroup buffer
 * @{
 */

#define DEFAULT_BUFFER_LENGTH 64

template <typename T, uint32_t N>
class buffer {
private:

public:
    buffer() {

        init();
    }

    buffer(semaphore* sem) {

        this->sem = sem;
        init();
    }

    void clear() {

        while(pos>0) {
            buf[--pos] = 0;
        }
    }

    void init() {
        error_overflow = 0;
        error_underflow = 0;
        pos = 0;
        len = N;
        clear();
    }

    void notify(const T data) {

        if (add(data)) {
            sem->post();
        }
    }

    /*! warning: drops Ts if buffer is full */
    bool add(const T data) {

        if (full()) {
            ++error_overflow;
            return false;
        }
        buf[pos++] = (T) data;
        return true;
    }

    T peek() {

        return buf[pos];
    }

    /*! warning: returns 0 if no more Ts in buffer */
    T get() {

        /* base case */
        if (pos <= 0) {
            ++error_underflow;
            return 0;
        }

        /* normal operation */
        T ret = buf[--pos];

        /*buf is always null-terminated*/
        buf[pos] = 0;

        return ret;
    }

    bool full() {

        return pos == len;
    }

    bool empty() {

        return pos == 0;
    }

    uint32_t length() {

        return pos;
    }

    /* Points ahead of last valid T */
    uint32_t pos;
    uint32_t len;
    semaphore* sem;
    T buf[N];

    uint32_t error_overflow;
    uint32_t error_underflow;
};

#endif

/*! End doxygen group
 * @}
 */
