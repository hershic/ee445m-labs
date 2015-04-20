/* -*- mode: c; c-basic-offset: 4; -*- */
#ifndef __NEXUS__
#define __NEXUS__

#include "defines.h"
#include <stdint.h>

/*! A keyword to signify that a value should never be reassigned. */
#define immutable

/*! A convenience alias to '__attribute__((always_inline))' to make
 *  function definitions read more naturally. */
#define always __attribute__((always_inline))

/*! Begin a critical section. */
#define atomic_start()                          \
    int32_t atom;                               \
    atom = StartCritical()

/*! End a critical section. */
#define atomic_end()                            \
    EndCritical(atom)

/*! Wrap a block of code and ensure it is executed without
 * interruption.
 * \warning Assumes that an int32t named atom exists in local
 * scope. */
#define atomic(x) {                             \
    int32_t atom;                               \
    atom = StartCritical();                     \
    x                                           \
    EndCritical(atom);                          \
}

/*! A macro to make it clear what we're doing with this while loop. */
#define postpone_death()                        \
    while(1)

/*! #Defined to nothing; results in code sugar allowing developers to
 * easily determine which parameter is frequency. */
#define Hz

/*! A reference to a memory location on the ARM Cortex M4. */
typedef uint32_t memory_address_t;

/*! A representation of a periodic frequency. */
typedef uint32_t frequency_t;

/*! Begin a critical section while saving the PRIMASK for future
 *  restoration.
 *  \returns current PRIMASK
 */
always static inline
int32_t StartCritical() {
    asm("MRS    R0, PRIMASK  ;// save old status\n");
    asm("CPSID  I            ;// mask all (except faults)\n");
}

/*! End a critical section by restoring a previously saved PRIMASK.
 * \param PRIMASK to restore
 */
always static inline
void EndCritical(int32_t primask) {
    /* asm("MSR    PRIMASK, R0\n"); */

    /*! bug: this line should be removed in favor of the above to
     *  avoid blindly enable interrupts, but instead enabling
     *  interrupts only if they were previously enabled before the
     *  last \StartCritical function call. */
    asm("CPSIE I");
}

#ifdef __cplusplus
extern "C"
{
#endif

/*! A duplicate of the c standard memset function. */
void* umemset(void*, int, int);

/*! A duplicate of the c standard memcpy function. */
void* umemcpy(void*, const void*, long);

/*! A duplicate of the c standard strcmp function. */
int ustrcmp(const char*, const char*);

/*! A duplicate of the c standard strncmp function. */
int ustrncmp(const char*, const char*, uint32_t);

/*! A duplicate of the c standard strcpy function.  */
void ustrcpy(char*, const char*);

/*! A duplicate of the c standard strlen function. */
uint32_t ustrlen(const char *s);

#ifdef __cplusplus
}
#endif

#endif
