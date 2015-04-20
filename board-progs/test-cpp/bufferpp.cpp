#include "bufferpp.hpp"

buffer::buffer() {

    pos = 0;
    clear();
}

void buffer::clear() {

    uint32_t i;
    for(i=0; i<DEFAULT_BUFFER_LENGTH; ++i) {
        buf[i] = 0;
    }
}

/*! warning: drops chars if buffer is full */
void buffer::add(const char ch) {

    if (pos >= DEFAULT_BUFFER_LENGTH) {
        return;
    }
    buf[pos++] = (char) ch;
}

char buffer::peek(void) {

    return buf[pos];
}

/*! warning: returns 0 if no more chars in buffer */
char buffer::get(void) {

    if (pos <= 0) {
        return 0;
    }
    return buf[--pos];
}
