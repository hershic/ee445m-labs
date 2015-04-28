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

    /*! Sem is only incremented on successful add */
    buffer(semaphore* sem) {

        this->sem = sem;
        *(this->sem) = semaphore();
        init();
    }

    virtual void clear() {

        while(pos>0) {
            buf[--pos] = 0;
        }
    }

    virtual void init() {
        error_overflow = 0;
        error_underflow = 0;
        pos = 0;
        len = N;
        clear();
    }

    virtual void notify(const T data) {

        if (add(data)) {
            sem->post();
        }
    }

    /*! warning: drops Ts if buffer is full */
    virtual bool add(const T data) {

        if (full()) {
            ++error_overflow;
            return false;
        }
        buf[pos++] = (T) data;
        return true;
    }

    virtual T peek() {

        if (pos == 0) {
            return buf[len-1];
        } else {
            return buf[pos-1];
        }
    }

    /*! warning: returns 0 if no more Ts in buffer */
    virtual T get(bool &ok) {

        /* base case */
        if (pos <= 0) {
            ++error_underflow;
            ok = false;
            return 0;
        }

        /* normal operation */
        ok = true;

        T ret = buf[--pos];

        /*buf is always null-terminated*/
        buf[pos] = 0;

        return ret;
    }

    virtual bool full() {

        return pos == len;
    }

    virtual bool empty() {

        return pos == 0;
    }

    virtual uint32_t length() {

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
