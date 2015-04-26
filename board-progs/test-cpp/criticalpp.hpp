#ifndef __criticalpp__
#define __criticalpp__

#include <stdint.h>
#include <stdbool.h>

class critical {
private:
    static bool valid_primask;
    static uint32_t primask;
public:
    uint32_t StartCritical(void) {
        asm("MRS    R0, PRIMASK  ;// save old status\n");
        asm("CPSID  I            ;// mask all (except faults)\n");
    }

    void EndCritical(uint32_t primask) {
        asm("MSR    PRIMASK, R0\n");
    }

    static uint32_t static_StartCritical(void) {
        asm("MRS    R0, PRIMASK  ;// save old status\n");
        asm("CPSID  I            ;// mask all (except faults)\n");
    }

    static void static_EndCritical(uint32_t primask) {
        asm("MSR    PRIMASK, R0\n");
    }

    /*! Save primask internally. */
    static void save_primask(void) {

        critical::valid_primask = true;
        critical::primask = static_StartCritical();
    }

    /*! Restore saved primask. */
    static void restore_primask(void) {

        if(!critical::valid_primask) {
            while(1) {}         /* fix yo code, this should never be called */
        }
        critical::valid_primask = false;
        static_EndCritical(critical::primask);
    }

    /*! Check if primask has been saved by not restored */
    static bool primask_saved(void) {

        return valid_primask;
    }
};

#endif

/* Local Variables: */
/* firestarter: (compile "make -k -j32 -C ~/workspace/ee445m-labs/build/") */
/* End: */
