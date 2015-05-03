/* -*- mode: c++; c-basic-offset: 4; -*- */
#ifndef __circularbuffer__
#define __circularbuffer__

#include <stdint.h>
#include "bufferpp.hpp"

/*! \addtogroup buffer
 * @{
 */

template <typename T, uint32_t N>
class circularbuffer : public buffer<T, N> {
private:

public:
    circularbuffer() : buffer<T, N>() { }

    void increment_ptr(uint32_t* ptr, uint32_t increment,
                       uint32_t wrap_len) {
        *ptr = (*ptr + increment) % wrap_len;
    }

    void add(const T ch) {
        this->buf[this->pos] = ch;
        increment_ptr(&(this->pos), 1, this->len);
    }

    T peek() {
        return this->buf(this->pos);
    }

    T get(int32_t offset) {

        int32_t position_to_get = ((this->pos+offset) % this->len);

        if (position_to_get < 0) {
            return this->buf[position_to_get + this->len];
        }
        return this->buf[position_to_get];
    }
};

#endif  /* __circularbuffer__ */

/*! End doxygen group
 * @}
 */
