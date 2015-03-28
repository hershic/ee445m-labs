#!/usr/bin/octave

pkg load signal;
pkg load data-smoothing

raw_data = csvread("T0003CH2.CSV");
time = raw_data(:,1);
voltage = raw_data(:,2);

distance = (time + 2) * 5.75;

## [b_1,a_1]=butter(1,.25);
## voltage_filtered_1 = filter(b_1, a_1, voltage);
## subplot (4, 1, 1);
## plot(distance, voltage_filtered_1, "o-");

## [b_2,a_2]=butter(1,.125);
## voltage_filtered_2 = filter(b_2, a_2, voltage);
## subplot (4, 1, 2);
## plot(distance, voltage_filtered_2);

## [b_3,a_3]=butter(1,.35);
## voltage_filtered_3 = filter(b_3, a_3, voltage);
## subplot (4, 1, 3);
## plot(distance, voltage_filtered_3);

## [b_4,a_4]=butter(1,.55);
## voltage_filtered_4 = filter(b_4, a_4, voltage);
## subplot (4, 1, 4);
## plot(distance, voltage_filtered_4);

plot(distance, voltage, "o-");
