# Getting and Cleaning Data - Run Analysis R Script Project
###### by Varun Bhandary
###### 24 December 2017


######      The run_analysis.R does the following.
1. Merges the training and the test sets to create one data set.
1. Extracts only the measurements on the mean and standard deviation for each measurement.
1. Uses descriptive activity names to name the activities in the data set.
1. Appropriately labels the data set with descriptive variable names.
1. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each  subject.


##     Codebook
The variables in the analyzed data set contain mean and standard deviation values for tri-axial measurements (X,Y,Z axes) of body and gravity acceleration components. 

Each value in the analyzed set comprises the average of those mean and standard deviation measurements across samples for each subject and activity. For example, tBodyAcc_mean_[XYZ]_avg and tBodyAcc_std_[XYZ]_avg.

Similarly, mean and standard deviation of time-derived acceleration signals, such as Jerk and angular velocity (Gyro) were summarized along each axis in, e.g. tBodyAccJerk_mean_[XYZ]_avg and tBodyAccJerk_std_[XYZ]_avg.

Euclidean norm computations were performed to determine the magnitude of the three-dimensional signals and their means and standard deviations appear in variables containing 'Mag', e.g. tBodyAccMag_mean_avg

Values with the 'f' prefix represent the average of FFT (Fast Fourier Transforms) of the original time-domain measurements.

The complete list of variables:
1. timeDomainBodyAccelerometerMeanX
1. timeDomainBodyAccelerometerMeanY
1. timeDomainBodyAccelerometerMeanZ
1. timeDomainBodyAccelerometerStandardDeviationX
1. timeDomainBodyAccelerometerStandardDeviationY
1. timeDomainBodyAccelerometerStandardDeviationZ
1. timeDomainGravityAccelerometerMeanX
1. timeDomainGravityAccelerometerMeanY
1. timeDomainGravityAccelerometerMeanZ
1. timeDomainGravityAccelerometerStandardDeviationX
1. timeDomainGravityAccelerometerStandardDeviationY
1. timeDomainGravityAccelerometerStandardDeviationZ
1. timeDomainBodyAccelerometerJerkMeanX
1. timeDomainBodyAccelerometerJerkMeanY
1. timeDomainBodyAccelerometerJerkMeanZ
1. timeDomainBodyAccelerometerJerkStandardDeviationX
1. timeDomainBodyAccelerometerJerkStandardDeviationY
1. timeDomainBodyAccelerometerJerkStandardDeviationZ
1. timeDomainBodyGyroscopeMeanX
1. timeDomainBodyGyroscopeMeanY
1. timeDomainBodyGyroscopeMeanZ
1. timeDomainBodyGyroscopeStandardDeviationX
1. timeDomainBodyGyroscopeStandardDeviationY
1. timeDomainBodyGyroscopeStandardDeviationZ
1. timeDomainBodyGyroscopeJerkMeanX
1. timeDomainBodyGyroscopeJerkMeanY
1. timeDomainBodyGyroscopeJerkMeanZ
1. timeDomainBodyGyroscopeJerkStandardDeviationX
1. timeDomainBodyGyroscopeJerkStandardDeviationY
1. timeDomainBodyGyroscopeJerkStandardDeviationZ
1. timeDomainBodyAccelerometerMagnitudeMean
1. timeDomainBodyAccelerometerMagnitudeStandardDeviation
1. timeDomainGravityAccelerometerMagnitudeMean
1. timeDomainGravityAccelerometerMagnitudeStandardDeviation
1. timeDomainBodyAccelerometerJerkMagnitudeMean
1. timeDomainBodyAccelerometerJerkMagnitudeStandardDeviation
1. timeDomainBodyGyroscopeMagnitudeMean
1. timeDomainBodyGyroscopeMagnitudeStandardDeviation
1. timeDomainBodyGyroscopeJerkMagnitudeMean
1. timeDomainBodyGyroscopeJerkMagnitudeStandardDeviation
1. frequencyDomainBodyAccelerometerMeanX
1. frequencyDomainBodyAccelerometerMeanY
1. frequencyDomainBodyAccelerometerMeanZ
1. frequencyDomainBodyAccelerometerStandardDeviationX
1. frequencyDomainBodyAccelerometerStandardDeviationY
1. frequencyDomainBodyAccelerometerStandardDeviationZ
1. frequencyDomainBodyAccelerometerMeanFrequencyX
1. frequencyDomainBodyAccelerometerMeanFrequencyY
1. frequencyDomainBodyAccelerometerMeanFrequencyZ
1. frequencyDomainBodyAccelerometerJerkMeanX
1. frequencyDomainBodyAccelerometerJerkMeanY
1. frequencyDomainBodyAccelerometerJerkMeanZ
1. frequencyDomainBodyAccelerometerJerkStandardDeviationX
1. frequencyDomainBodyAccelerometerJerkStandardDeviationY
1. frequencyDomainBodyAccelerometerJerkStandardDeviationZ
1. frequencyDomainBodyAccelerometerJerkMeanFrequencyX
1. frequencyDomainBodyAccelerometerJerkMeanFrequencyY
1. frequencyDomainBodyAccelerometerJerkMeanFrequencyZ
1. frequencyDomainBodyGyroscopeMeanX
1. frequencyDomainBodyGyroscopeMeanY
1. frequencyDomainBodyGyroscopeMeanZ
1. frequencyDomainBodyGyroscopeStandardDeviationX
1. frequencyDomainBodyGyroscopeStandardDeviationY
1. frequencyDomainBodyGyroscopeStandardDeviationZ
1. frequencyDomainBodyGyroscopeMeanFrequencyX
1. frequencyDomainBodyGyroscopeMeanFrequencyY
1. frequencyDomainBodyGyroscopeMeanFrequencyZ
1. frequencyDomainBodyAccelerometerMagnitudeMean
1. frequencyDomainBodyAccelerometerMagnitudeStandardDeviation
1. frequencyDomainBodyAccelerometerMagnitudeMeanFrequency
1. frequencyDomainBodyBodyAccelerometerJerkMagnitudeMean
1. frequencyDomainBodyBodyAccelerometerJerkMagnitudeStandardDeviation
1. frequencyDomainBodyBodyAccelerometerJerkMagnitudeMeanFrequency
1. frequencyDomainBodyBodyGyroscopeMagnitudeMean
1. frequencyDomainBodyBodyGyroscopeMagnitudeStandardDeviation
1. frequencyDomainBodyBodyGyroscopeMagnitudeMeanFrequency
1. frequencyDomainBodyBodyGyroscopeJerkMagnitudeMean
1. frequencyDomainBodyBodyGyroscopeJerkMagnitudeStandardDeviation
1. frequencyDomainBodyBodyGyroscopeJerkMagnitudeMeanFrequency


