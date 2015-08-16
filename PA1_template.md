---
output: word_document
---
## **Data Analysis Report for Activity Monitoring Device Data**
#### **------Reproducible Research Project 1**
#### *Miao Li*
#### *August 10, 2015*

### **1 Introduction**

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This report makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

### **2 Loading and Preprocessing the Data**
#### **2.1 Preparation of Working Environment**
THe first step is to set up the working environment for R used in this analysis. There are several additional libraries used that are not in the standard R installation.

```r
setwd("D:/R Document/Coursera/RepResearch/Project1")
library(data.table)
library(date)
library(ggplot2)
library(dplyr)
```

#### **2.2 Load the Data**
The next step is to load the data for the study. The data file was originally in compressed form, and was downloaded and decompressed to the local drive as activity.csv.

```r
if (!file.exists("activity.csv")) {
    url="http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(url, "activity.zip", quiet=T)
    unzip("activity.zip")
    unlink("activity.zip")
    }
df.activity <- fread("activity.csv", data.table=F)
str(df.activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(df.activity)
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
df.activity[,"date"] <- as.Date(df.activity[,"date"])
rollUpSteps <- summarise(group_by(df.activity, date), steps=sum(steps))
summary.rollUpSteps <- summary(rollUpSteps)
```

### **3 What is Mean Total Number of Steps Taken per Day?**

```r
totalSteps <- aggregate(steps ~ date, data = df.activity, sum, na.rm = TRUE)
```
#### **3.1 Histogram of Total Number of Steps**


```r
totalStepPlot <- ggplot(totalSteps, aes(steps)) 
totalStepPlot <- totalStepPlot + labs(title="Histogram of Daily Steps",
                        x="Total Number of Steps per Day",
                        y="Frequency")
totalStepPlot <- totalStepPlot + geom_histogram(binwidth=1952, colour='black', fill='blue')
totalStepPlot
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

#### **3.2 The Mean and Median for Total Number of Steps per Day**


```r
mean_steps <- mean(totalSteps$steps)
median_steps <- median(totalSteps$steps)
mean_steps
```

```
## [1] 10766.19
```

```r
median_steps
```

```
## [1] 10765
```
The mean for total number of steps per day is 10766 steps

The median for total number of steps per day is 10765 steps

### **4 What is the average daily activity pattern?**

```r
rollUpInterval <- summarise(group_by(df.activity, interval), msteps=mean(steps, na.rm=T))
```
#### **4.1 Time Series Plot for the Average Daily Activity Pattern**

```r
TimeSeriesPlot <- ggplot(data=rollUpInterval, aes(interval, msteps)) + geom_line(colour="blue")
TimeSeriesPlot <- TimeSeriesPlot + labs(title="Average Number of Steps by Intervals",x="Intervals",y="Number of Steps")
TimeSeriesPlot
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

#### **4.2 Intervels Containing the Maximum Number of Steps**

```r
MaxSteps <- max(rollUpInterval[,'msteps'])
MaxStepInterval <- rollUpInterval[rollUpInterval$msteps==MaxSteps, 'interval']
MaxStepInterval
```

```
## Source: local data frame [1 x 1]
## 
##   interval
## 1      835
```
So 835 intervals contain the maximum number of steps

### **5 Imputing Missing Values**
#### **5.1 Total Number of Missing Values**

```r
MissingValues <- sum(!complete.cases(df.activity))
MissingValues
```

```
## [1] 2304
```
The number of missing values is 2304

#### **5.2 Dataset Creation of Imputing Missing Values**

A strategy for filling in all of the missing values in the dataset is described below. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
steps_interval <- aggregate(steps ~ interval, data = df.activity, mean, na.rm = TRUE)
## This function returns the mean steps for a given interval
MeanStepsPerInterval <- function(interval){
    steps_interval[steps_interval$interval==interval,"steps"]
    }
```
Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
complete.activity <- df.activity
## Filling the missing values with the mean for that 5-minute interval
flag = 0
for (i in 1:nrow(complete.activity)) {
    if (is.na(complete.activity[i,"steps"])) {
        complete.activity[i,"steps"] <- MeanStepsPerInterval(complete.activity[i,"interval"])
        flag = flag + 1
        }
}
ImputedMissingValues <- aggregate(steps ~ date, data = complete.activity, sum)
```

#### **5.3 Histogram of Total Number of Steps**

```r
ImputingPlot <- ggplot(ImputedMissingValues, aes(x=steps)) 
ImputingPlot <- ImputingPlot + labs(title="Histogram Using Imputed Data",
                        x="Total Steps per Day",
                        y="Frequency")
ImputingPlot <- ImputingPlot + geom_histogram(binwidth=1952, colour='black', fill='green')
ImputingPlot
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 

#### **5.4 The Mean and Median for Total Number of Steps per Day using Imputing Data**

```r
ImputingMean <- mean(ImputedMissingValues$steps)
ImputingMedian <- median(ImputedMissingValues$steps)
ImputingMean
```

```
## [1] 10766.19
```

```r
ImputingMedian
```

```
## [1] 10766.19
```
Mean for total number of steps taken per day is 10766

Median total number of steps taken per day is 10766

The mean value is the same, but the median value changed.

### **6 Are there differences in activity patterns between weekdays and weekends?**
#### **6.1 Dataset with Weekday and Weekend Factors**

```r
complete.activity$day <- ifelse(as.POSIXlt(as.Date(complete.activity$date))$wday%%6 == 
                                    0, "weekend", "weekday")
complete.activity$day <- factor(complete.activity$day, levels = c("weekday", "weekend"))
```
#### **6.2 Panel Plot of Weekday VS Weekend Activity**
A panel plot is made containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.

```r
steps.interval= aggregate(steps ~ interval + day, complete.activity, mean)
library(lattice)
xyplot(steps ~ interval | factor(day), cex=10,data = steps.interval, aspect = 1/2, type = "l")
```

![plot of chunk unnamed-chunk-15](figure/unnamed-chunk-15-1.png) 
