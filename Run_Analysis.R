##########################################################################################################
# Data Science Specialization - Module 3 - Getting and Cleaning Data
# Paulo Roberto S. de Azevedo
##########################################################################################################


# Step 1. Merge the training and the test sets to create one data set.

setwd('C:\\datascience\\Mod3CourseProject');

#Reading the training data
features     = read.table('./features.txt',header=FALSE); 
activityType = read.table('./activity_labels.txt',header=FALSE); 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); 
xTrain       = read.table('./train/x_train.txt',header=FALSE); 
yTrain       = read.table('./train/y_train.txt',header=FALSE);

# Defining the column names 
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId";

# Binding the training files into the final training data frame
trainDF = cbind(yTrain,subjectTrain,xTrain);

# Reading the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt

# Defining the column names
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId";


# Binding the test files into the final test data frame
TestDF = cbind(yTest,subjectTest,xTest);


# Merging Test and training data frames
MergedDF = rbind(trainDF,TestDF);

colNames  = colnames(MergedDF); 

# Step 2. Extract only the measurements on the mean and standard deviation for each measurement. 

#Getting only the desired columns 
DesiredColumns = (grepl("activity..",colNames) | grepl("subject..",colNames) | grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) & !grepl("mean..-",colNames) | grepl("-std..",colNames) & !grepl("-std()..-",colNames));
MergedDF = MergedDF[DesiredColumns==TRUE];

# Step 3. Use descriptive activity names to name the activities in the data set

# Including activity names
MergedDF = merge(MergedDF,activityType,by='activityId',all.x=TRUE);
colNames  = colnames(MergedDF); 

# Step 4. Appropriately label the data set with descriptive activity names. 

for (i in 1:length(colNames)) 
{
  colNames[i] = gsub("\\()","",colNames[i])
  colNames[i] = gsub("-std$","StdDev",colNames[i])
  colNames[i] = gsub("-mean","Mean",colNames[i])
  colNames[i] = gsub("^(t)","time",colNames[i])
  colNames[i] = gsub("^(f)","freq",colNames[i])
  colNames[i] = gsub("([Gg]ravity)","Gravity",colNames[i])
  colNames[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames[i])
  colNames[i] = gsub("[Gg]yro","Gyro",colNames[i])
  colNames[i] = gsub("AccMag","AccMagnitude",colNames[i])
  colNames[i] = gsub("([Bb]odyaccjerkmag)","BodyAccJerkMagnitude",colNames[i])
  colNames[i] = gsub("JerkMag","JerkMagnitude",colNames[i])
  colNames[i] = gsub("GyroMag","GyroMagnitude",colNames[i])
};
colnames(MergedDF) = colNames;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 


MergedDFNoActivityType  = MergedDF[,names(MergedDF) != 'activityType'];
tidyData    = aggregate(MergedDFNoActivityType[,names(MergedDFNoActivityType) != c('activityId','subjectId')],by=list(activityId=MergedDFNoActivityType$activityId,subjectId = MergedDFNoActivityType$subjectId),mean);
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

write.table(tidyData, './TidyData.txt',row.names=TRUE,sep='\t');