#ifndef __criticalpp__
#define __criticalpp__

class critical {
public:
    uint32_t StartCritical(void) {
        asm("MRS    R0, PRIMASK  ;// save old status\n");
        asm("CPSID  I            ;// mask all (except faults)\n");
    }

    void EndCritical(uint32_t primask) {
        asm("MSR    PRIMASK, R0\n");
    }
};

#endif
