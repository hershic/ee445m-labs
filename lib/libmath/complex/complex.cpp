/* -*- mode: c; c-basic-offset: 4; -*- */
/* Created by Eric Crosson on <2015-03-22 Sun> */
/* Revision History: Look in git FGT */

#include "complex.hpp"

void complex::convolve(uint32_t *x, uint32_t *h, uint32_t *y, uint32_t length) {
    uint32_t i, j;
    for (i=0; i<length; ++i) {
	j = 0;
	y[i] = 0;
	while(j <= i) {
	    y[i]+= h[j] * x[i-j];
	}
    }
}
