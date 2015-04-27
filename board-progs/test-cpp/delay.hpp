/* -*- mode: c++; c-basic-offset: 4; */
#ifndef __delaypp__
#define __delaypp__

#include <stdint.h>

class Delay {
public:
    Delay() {}
    Delay(uint32_t cycles) {
        uint32_t counter = 0;
        while(counter < cycles){counter++;}
    }

    static void static_delay(uint32_t cycles) {
        uint32_t counter = 0;
        while(counter < cycles){counter++;}
    }
};

#endif  /* __delaypp__ */

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
