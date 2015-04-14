#include "bufferpp.hpp"

buffer::buffer() {

    len = DEFAULT_BUFFER_LENGTH;
    init();
}

buffer::buffer(semaphore sem) {

    this->sem = sem;
    len = DEFAULT_BUFFER_LENGTH;
    init();
}

/* buffer::buffer(int32_t length) { */

/*     len = length; */
/*     init(); */
/* } */

/* buffer::buffer(int32_t length, semaphore sem) { */

/*     this->sem = sem; */
/*     len = length; */
/*     init(); */
/* } */

/* To reduce duplicated code in ooverloaded constructors */
void buffer::init() {

    pos = 0;
    error_overflow = 0;
    error_underflow = 0;
    clear();
}

void buffer::clear() {

    while(pos>0) {
        buf[--pos] = 0;
    }
}

void buffer::notify(const int8_t data) {

    if (add(data)) {
        sem.post();
    }
}

/*! warning: drops data if buffer is full */
bool buffer::add(const int8_t data) {

    if (full()) {
        ++error_overflow;
        return false;
    }
    buf[pos++] = (int8_t) data;
    return true;
}

int8_t buffer::peek() {

    return buf[pos];
}

/*! warning: returns 0 if no more data resides in buffer */
int8_t buffer::get() {

    /* base case */
    if (pos <= 0) {
        ++error_underflow;
        return 0;
    }
    /* normal operation */
    int8_t ret = buf[--pos];
    buf[pos] = 0;               /*buf is always null-terminated*/
    return ret;
}

bool buffer::full() {

    return pos == len;
}

bool buffer::empty() {

    return pos == 0;
}

uint32_t buffer::length() {

    return pos;
}
