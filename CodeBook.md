## Abstract
Human Activity Recognition database built from the recordings of 30 subjects performing activities of daily living (ADL) while carrying a waist-mounted smartphone with embedded inertial sensors.

### Data Set Information:
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

## About the data
In summary  the data includes:
1. Train and Test Datasets - data from the experiment.

   X_test.txt - observation
   
   Y_test.txt - activity code
2. Features - column names for train and test dataset
3. Activity Labels - activity mapping

Please read ./data/README.txt for detailed information on the dataset


## Data Cleaning Steps
1. Join X_test & Y_test columns for each (train & test)
2. Join rows of train and test observations.
3. Assign columnnames for data on step#2 from features.txt:
   
   But features.txt has duplicate records. Records were renamed to include X, Y and Z each:
   
   fBodyAcc-bandsEnergy()-1,8    ------> 'X'
   
   fBodyAcc-bandsEnergy()-1,8    ------> 'Y'
   
   fBodyAcc-bandsEnergy()-1,8    ------> 'Z'
   
4. Filtered the columns that is a measurement on the mean and standard deviation
5. Export tidydata from previous steps. Please check tidydata1.csv 
6. Get average of each measurement in tidydata1 by activity. Please check tidydata2.csv


Thank you.
