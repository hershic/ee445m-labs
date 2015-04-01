function [out] = convolve(x, h)

  filter_len = length(h);
  signal_len = length(x);

  for i=1:(signal_len + filter_len)
    y(i) = 0;

    if (i > signal_len)
      jmin = i - signal_len;
    end



    jmin =  ? i - (signal_len) : 1;
    jmax = (i <= filter_len) ? i : filter_len;

    for(j = jmin:jmax)
      y(i)+= (h(j) * x(i-j));
      y(i) /= 256;
    endfor

endfunction
