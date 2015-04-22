/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __bufferpp__
#define __bufferpp__

#include <stdint.h>

/*! \addtogroup buffer
 * @{
 */

#define DEFAULT_BUFFER_LENGTH 64

template <typename T, uint32_t N>
class buffer {
private:
    /* Points ahead of last valid T */
    uint32_t pos;
    uint32_t len;

public:
    T buf[N];

    buffer() {

        pos = 0;
        len = N;
        clear();
    }

    void clear() {

        while(pos>0) {
            buf[--pos] = 0;
        }
    }

    /*! warning: drops Ts if buffer is full */
    void add(const T ch) {

        if (pos >= len) {
            return;
        }
        buf[pos++] = (T) ch;
    }

    T peek() {

        return buf[pos];
    }

    /*! warning: returns 0 if no more Ts in buffer */
    T get() {

        if (pos <= 0) {
            return 0;
        }
        T ret = buf[--pos];
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
};

#endif

/*! End doxygen group
 * @}
 */
