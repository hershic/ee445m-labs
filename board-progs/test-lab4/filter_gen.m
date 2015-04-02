#!/usr/bin/octave

function [h] = filter_gen(filter_length, cutoff_freq, sampling_rate)

  # Design the filter using the window method:
  hsupp = (-(filter_length-1)/2:(filter_length-1)/2);
  hideal = (2*cutoff_freq/sampling_rate)*sinc(2*cutoff_freq*hsupp/sampling_rate);

  # h is our filter
  h = hamming(filter_length)' .* hideal;

endfunction
