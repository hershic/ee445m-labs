/* -*- mode: c++; c-basic-offset: 4; -*- */

#include "math.hpp"

int32_t clamp(int32_t value, int32_t min, int32_t max) {
    if (value > max) {
        return max;
    } else if (value < min) {
        return min;
    } else {
        return value;
    }
}

int32_t floor(int32_t value, int32_t scaling) {
    return value/scaling*scaling;
}

int32_t ceil(int32_t value, int32_t scaling) {
    return (value + scaling)/scaling*scaling;
}

int32_t max(int32_t v1, int32_t v2) {
    if (v1 > v2) return v1;
    return v2;
}
