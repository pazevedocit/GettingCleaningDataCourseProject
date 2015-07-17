---
title: "Readme.MD"
author: "Paulo Roberto Azevedo"
date: "Friday, July 17, 2015"
output: html_document
---
**Getting and Cleaning Data Project**

Repo for the submission of the course project.

**Overview**

The purpose of this project is to demonstrate how to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis.

Here are the raw data used by script: 

<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

**Making Modifications to This Script**

Once you have obtained and unzipped the source files, you will need to make one modification to the R file before you can process the data in order to set the working directory properly. To do this configurariton edit the Run_Analysis.R file and change the line 9 - **setwd('your working directory here');**

**Project Summary**

The following is a summary description of the project instructions

You should create one R script called run_analysis.R that does the following. 1. Merges the training and the test sets to create one data set. 2. Extracts only the measurements on the mean and standard deviation for each measurement. 3. Uses descriptive activity names to name the activities in the data set 4. Appropriately labels the data set with descriptive activity names. 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

**Additional Information**

You can find additional information about the variables, data and transformations in the CodeBook.MD file.

