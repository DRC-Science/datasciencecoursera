
# Getting and Cleaning Data Course Project

## Code Book 




### The Tidy Data - TidyData.csv

Each row represents data from a subject (person) and activity (e.g. WALKING, STANDING, etc.). 
Each variable is the Mean or Standard Deviation of the Mean or Standard Deviation of the Mean of Standard Deviation of measurements produced by the researchers.

Mean.Variable - The Mean of the variables produced by the researchers are labeled *"Mean.<insert measurement variable>"*
Std.Variable - The Standard Deviation of the variables produced by the researchers are labled *"Std.<insert measurement variable>"*

 

Measurement Variables - Measurements were taken on the X, Y, and Z axes.  The specific axis is noted on the end of the variable name. Time and Frequency measurements were taken.  If the measurement variable begins with "t", it refers to a time measurement.  If the measurement variable begins with an "f", it refers to a frequency measurement.



#### Mean and Standard Deviation of measurements below were provided:

- The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Same applies for the Y and Z axis. 
- The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
- The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second.


For additional detail on the measurements taken, see the Original Data Section of the Code Book.


#### To tidy the data, the following steps needed to be taken:


- Merged subject (subject_train.txt, subject_test.txt) and activity numbers (y_train.txt, y_test.txt) to the training (X_train.txt) and test (X_test.txt) datasets 
- Added labels to the activity numbers data sets by activity number by merging the activity labels (activity_lables.txt) to the updated training and test dataset.
- Added descriptive measurement names (features.txt) to the updated training and test datasets
- Merged the training (X_train.txt) and the test (X_test.txt) sets to create one data set. 
- Extracted only the measurements on the mean and standard deviation for each measurement variable. 
- Created an independent tidy data set with the average and standard deviation of each variable in the extracted dataset for each activity and each subject.

More detail is available in the comments of the code which is available in the README.md document.




### The Original Data:

Human Activity Recognition Using Smartphones Dataset
Version 1.0

Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - UniversitÓ degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws


The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

#### For each record it is provided:


- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

#### The dataset includes the following files:


- 'README.txt'

- 'features_info.txt': Shows information about the variables used on the feature vector.

- 'features.txt': List of all features.  Lists the names of the summary variables for each measurement.

- 'activity_labels.txt': Links the class labels with their activity name (WALKING, STANDING, etc.).

- 'train/X_train.txt': Training set. Contains the summary variables for the measurements taken.

- 'train/y_train.txt': Training labels. Labels range from 1 to 6.

- 'test/X_test.txt': Test set.  Contains the summary variables for the measurements taken.

- 'test/y_test.txt': Test labels.  Labels range from 1 to 6.

The following files are available for the train and test data. Their descriptions are equivalent. 

- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 

- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 

- 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 


#### Notes: 

- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License:

Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.