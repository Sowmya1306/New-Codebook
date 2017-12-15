run_analysis.R should do the following: 
  2 #    
3 #   1)  Merges the training and the test sets to create one data set. 
4 #  
5 #   2)  Extracts only the measurements on the mean and standard deviation for each measurement.  
6 #  
7 #   3)  Uses descriptive activity names to name the activities in the data set 
8 #  
9 #   4)  Appropriately labels the data set with descriptive variable names.  
10 #    
11 #   5)  From the data set in step 4, creates a second, independent tidy data set with the average 
12 #       of each variable for each activity and each subject. 
13 

14 #     Please upload the tidy data set created in step 5 of the instructions.  
15 

16 #     Please upload your data set as a txt file created with write.table() using row.name=FALSE  
17 #     (do not cut and paste a dataset directly into the text box, as this may cause errors  
18 #     saving your submission). 
19 

20 # Load the needed packages 
21 packages <- c("data.table", "reshape2", "dplyr") 
22 sapply(packages, require, character.only=TRUE, quietly=TRUE) 
23 

24 # Assumes the Git repository : https://github.com/dholtz/GettingAndCleaningData 
25 # has been cloned to a users local machine, and the R, setwd(), has been used  
26 # to set the working directory to the root of this cloned repository. 
27 path <- getwd() 
28 

29 # Give warning to set the working directory if not able to find data files. 
30 projectDataPath <- file.path(path, "project_data") 
31 fileCount <- length(list.files(projectDataPath, recursive=TRUE)) 
32 if (fileCount != 28) { 
  33   stop("Please use setwd() to the root of the cloned repository.") 
  34 } 
35 

36 # Read in the 'Subject' data 
37 dtTrainingSubjects <- fread(file.path(projectDataPath, "train", "subject_train.txt")) 
38 dtTestSubjects  <- fread(file.path(projectDataPath, "test" , "subject_test.txt" )) 
39 

40 # Read in the 'Activity' data 
41 dtTrainingActivity <- fread(file.path(projectDataPath, "train", "Y_train.txt")) 
42 dtTestActivity  <- fread(file.path(projectDataPath, "test" , "Y_test.txt" )) 
43 

44 # Read in the 'Measurements' data 
45 # Switching to standard, read.table to avoid the following possible error: 
46 # https://github.com/Rdatatable/data.table/issues/487 
47 # No time to figure out where this, 'works again now' version is 
48 dtTrainingMeasures <- data.table(read.table(file.path(projectDataPath, "train", "X_train.txt"))) 
49 dtTestMeasures  <- data.table(read.table(file.path(projectDataPath, "test" , "X_test.txt"))) 
50 

51 # Row merge the Training and Test Subjects 
52 # http://www.statmethods.net/management/merging.html 
53 dtSubjects <- rbind(dtTrainingSubjects, dtTestSubjects) 
54 # Why setnames() ?? http://stackoverflow.com/questions/10655438/rename-one-named-column-in-r 
55 setnames(dtSubjects, "V1", "subject") 
56 

57 # Row merge the Training and Test Activities 
58 dtActivities <- rbind(dtTrainingActivity, dtTestActivity) 
59 setnames(dtActivities, "V1", "activityNumber") 
60 

61 # Merge the Training and Test 'Measurements' data 
62 dtMeasures <- rbind(dtTrainingMeasures, dtTestMeasures) 
63 

64 # Column merge the subjects to activities 
65 dtSubjectActivities <- cbind(dtSubjects, dtActivities) 
66 dtSubjectAtvitiesWithMeasures <- cbind(dtSubjectActivities, dtMeasures) 
67 

68 # Order all of the combined data by, subject and activity 
69 setkey(dtSubjectAtvitiesWithMeasures, subject, activityNumber) 
70 

71 ## Read in the 'features.txt'  
72 ## This file matches up to the columns in the data.table, dtSubjectActivitiesWithMeasures 
73 ## with the features/measures. 
74 dtAllFeatures <- fread(file.path(projectDataPath, "features.txt")) 
75 setnames(dtAllFeatures, c("V1", "V2"), c("measureNumber", "measureName")) 
76 

77 # Use grepl to just get features/measures related to mean and std 
78 dtMeanStdMeasures <- dtAllFeatures[grepl("(mean|std)\\(\\)", measureName)] 
79 # Create a column to 'index/cross reference' into the 'measure' headers 
80 # in dtSubjectActivitiesWithMeasures 
81 dtMeanStdMeasures$measureCode <- dtMeanStdMeasures[, paste0("V", measureNumber)] 
82 

83 # Build up the columns to select from the data.table, 
84 # dtSubjectActivitiesWithMeasures 
85 columnsToSelect <- c(key(dtSubjectAtvitiesWithMeasures), dtMeanStdMeasures$measureCode) 
86 # Just take the rows with the columns of interest ( std() and mean() ) 
87 dtSubjectActivitesWithMeasuresMeanStd <- subset(dtSubjectAtvitiesWithMeasures,  
                                                   88                                                 select = columnsToSelect) 
89 

90 # Read in the activity names and give them more meaningful names 
91 dtActivityNames <- fread(file.path(projectDataPath, "activity_labels.txt")) 
92 setnames(dtActivityNames, c("V1", "V2"), c("activityNumber", "activityName")) 
93 

94 # Merge the 'meaningful activity names' with the  
95 # dtSubjectActiitiesWithMeasuresMeanStd 
96 dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd,  
                                                  97                                                dtActivityNames, by = "activityNumber",  
                                                  98                                                all.x = TRUE) 
99 

100 # Sort the data.table, dtSubjectActivitesWithMeasuresMeanStd 
101 setkey(dtSubjectActivitesWithMeasuresMeanStd, subject, activityNumber, activityName) 
102 

103 # Convert from a wide to narrow data.table using the keys created earlier 
104 dtSubjectActivitesWithMeasuresMeanStd <- data.table(melt(dtSubjectActivitesWithMeasuresMeanStd,  
                                                             105                                                          id=c("subject", "activityName"),  
                                                             106                                                          measure.vars = c(3:68),  
                                                             107                                                          variable.name = "measureCode",  
                                                             108                                                          value.name="measureValue")) 
109 

110 # Merge measure codes 
111 dtSubjectActivitesWithMeasuresMeanStd <- merge(dtSubjectActivitesWithMeasuresMeanStd,  
                                                   112                                                dtMeanStdMeasures[, list(measureNumber, measureCode, measureName)],  
                                                   113                                                by="measureCode", all.x=TRUE) 
114 

115 # Convert activityName and measureName to factors 
116 dtSubjectActivitesWithMeasuresMeanStd$activityName <-  
  117   factor(dtSubjectActivitesWithMeasuresMeanStd$activityName) 
118 dtSubjectActivitesWithMeasuresMeanStd$measureName <-  
  119   factor(dtSubjectActivitesWithMeasuresMeanStd$measureName) 
120 

121 # Reshape the data to get the averages  
122 measureAvgerages <- dcast(dtSubjectActivitesWithMeasuresMeanStd,  
                              123                           subject + activityName ~ measureName,  
                              124                           mean,  
                              125                           value.var="measureValue") 
126 

127 # Write the tab delimited file 
128 write.table(measureAvgerages, file="tidyData.txt", row.name=FALSE, sep = "\t") 
