---
title: "CodeBook"
author: "Haili DENG"
date: "Thursday, January 22, 2015"
---
Data.txt CodeBook

Column 1:
subject 
  A series of number of subjects from 1 to 30.
  Type: Integer
  
Column 2:  
activity
  The six activities that the subjects performed.Listed as:
  WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING
  Type: character
  
Column 3 to 68:
variables
  These are the variables that obtain by the experiment.
  The breakdown of the notation of the variables is listed as follow.
  t: denote time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. 
  f: denote a Fast Fourier Transform (FFT) was applied to some of these signals(the 'f' to indicate
     frequency domain signals). 
  Body: denote a body acceleration
  Gravity: denote a gravity acceleration
  Acc: denote a signals obtained by accelerometer.
  Gyro: denote a signals obtained by Gyroscope.
  Jerk: denote a Jerk signals derived in time by body linear acceleration and angular velocity.
  Mag: denote the magnitude of the three dimensional signals calculated by Euclidean norm.
  mean: denote the mean value of the signal.
  std: denote the standard deviation of the signal.
  X/Y/Z: denote the 3-axis.
  
  for example "tBodyAccJerk mean X" is the mean of a time domain body linear acceleration and angular velocity of X axis.
  