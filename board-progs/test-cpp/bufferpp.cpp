#include "bufferpp.hpp"

buffer::buffer() {

    pos = 0;
    len = DEFAULT_BUFFER_LENGTH;
    clear();
}

void buffer::clear() {

    while(pos>0) {
        buf[--pos] = 0;
    }
}

/*! warning: drops chars if buffer is full */
void buffer::add(const char ch) {

    if (pos >= len) {
        return;
    }
    buf[pos++] = (char) ch;
}

char buffer::peek() {

    return buf[pos];
}

/*! warning: returns 0 if no more chars in buffer */
char buffer::get() {

    if (pos <= 0) {
        return 0;
    }
    char ret = buf[--pos];
    buf[pos] = 0;
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
