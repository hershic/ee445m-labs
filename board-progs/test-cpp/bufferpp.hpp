/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __hardwarepp__
#define __hardwarepp__

#endif

/*! \addtogroup buffer
 * @{
 */

#define DEFAULT_BUFFER_LENGTH 64

class buffer {
private:
    char buf[DEFAULT_BUFFER_LENGTH];
    /* Points ahead of last valid char */
    uint32_t pos;

public:
    buffer();

    /*! Add a char. */
    void add(const char ch);

    /*! Peek at a char. */
    char peek(void);

    /*! Remove a char. */
    char get(void);

    /*! Clear the buffer. */
    void clear(void);
};

/*! End doxygen group
 * @}
 */
