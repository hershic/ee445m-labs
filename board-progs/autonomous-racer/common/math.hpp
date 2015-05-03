int32_t clamp(int32_t value, int32_t min, int32_t max) {
    if (value > max) {
        return max;
    } else if (value < min) {
        return min;
    } else {
        return value;
    }
}

inline int32_t floor(int32_t value, int32_t scaling) {
    return value/scaling*scaling;
}

inline int32_t ceil(int32_t value, int32_t scaling) {
    return (value + scaling)/scaling*scaling;
}
