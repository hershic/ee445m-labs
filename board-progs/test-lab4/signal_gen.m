#!/usr/bin/octave

function [x] = signal_gen(frequencies, signal_length, sampling_rate)

  # Generate a signal by adding up sinusoids:
  x = zeros(1,signal_length);
  n = 0:(signal_length-1);
  for fk = frequencies;
    x = x + sin(2*pi*n*fk/sampling_rate);
  endfor

endfunction
