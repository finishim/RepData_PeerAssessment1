# Reproducible Research: Peer Assessment 1
Nazmi Anik  


## Loading and preprocessing the data  

1. Unzip the file first, if the unzipped file is not already in the working directory.  

```r
if(!file.exists('activity.csv')){
    unzip('activity.zip')
}
```

2. Load the data ( i.e. read.csv() )  

```r
#read the table
activity <- read.csv("activity.csv", stringsAsFactors=FALSE, na.strings="NA")
```

3. Process/transform the data (if necessary) into a format suitable for your analysis  

```r
#change the date column's class from string to Date
activity[,"date"] <- as.Date(activity[,"date"],format='%Y-%m-%d')
#display the first few rows
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## What is mean total number of steps taken per day?  

1. Calculate the total number of steps taken per day  

```r
#import dplyr library
library(dplyr)
```

```r
#group the activity table by day
activityDay <- group_by(activity, date)
#summarize this grouped table with the sum of steps for each day
stepDay <- summarize(activityDay, steps_by_day = sum(steps))
#For this part of the assignment, you can ignore the missing values in the dataset.
stepDay <- na.omit(stepDay)
#display the first few rows
head(stepDay)
```

```
## Source: local data frame [6 x 2]
## 
##         date steps_by_day
## 1 2012-10-02          126
## 2 2012-10-03        11352
## 3 2012-10-04        12116
## 4 2012-10-05        13294
## 5 2012-10-06        15420
## 6 2012-10-07        11015
```

2. Make a histogram of the total number of steps taken each day  

```r
hist(stepDay$steps_by_day, main="Number of Steps Taken Each Day", xlab="Number of Steps per Day", ylab="Frequency", col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

3. Calculate and report the mean and median of the total number of steps taken per day

```r
#take the mean and median from the summed column
stepDayMean <- mean(stepDay$steps_by_day)
stepDayMedian <- median(stepDay$steps_by_day)
```
**The mean total number of steps taken per day**: 10766.1886792453  
**The median total number of steps taken per day**: 10765   

## What is the average daily activity pattern?  

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  

```r
#average number of steps taken (mean) within each interval
stepInterval <- tapply(activity$steps, activity$interval, mean, na.rm = TRUE)
#plot
plot(stepInterval, type="l", xlab="5-min Interval", ylab="Av Num of Steps", main = "Average Number of Steps Taken", col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


## Imputing missing values  



## Are there differences in activity patterns between weekdays and weekends?  