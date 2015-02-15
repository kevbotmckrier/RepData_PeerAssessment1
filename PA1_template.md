# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
stepDataRaw <- read.csv("activity.csv")
```
## What is mean total number of steps taken per day?

The total number of steps each day are:

```r
stepsPerDay <- tapply(stepDataRaw$steps,stepDataRaw$date,sum,na.rm=TRUE)
stepsPerDay
```

```
## 2012-10-01 2012-10-02 2012-10-03 2012-10-04 2012-10-05 2012-10-06 
##          0        126      11352      12116      13294      15420 
## 2012-10-07 2012-10-08 2012-10-09 2012-10-10 2012-10-11 2012-10-12 
##      11015          0      12811       9900      10304      17382 
## 2012-10-13 2012-10-14 2012-10-15 2012-10-16 2012-10-17 2012-10-18 
##      12426      15098      10139      15084      13452      10056 
## 2012-10-19 2012-10-20 2012-10-21 2012-10-22 2012-10-23 2012-10-24 
##      11829      10395       8821      13460       8918       8355 
## 2012-10-25 2012-10-26 2012-10-27 2012-10-28 2012-10-29 2012-10-30 
##       2492       6778      10119      11458       5018       9819 
## 2012-10-31 2012-11-01 2012-11-02 2012-11-03 2012-11-04 2012-11-05 
##      15414          0      10600      10571          0      10439 
## 2012-11-06 2012-11-07 2012-11-08 2012-11-09 2012-11-10 2012-11-11 
##       8334      12883       3219          0          0      12608 
## 2012-11-12 2012-11-13 2012-11-14 2012-11-15 2012-11-16 2012-11-17 
##      10765       7336          0         41       5441      14339 
## 2012-11-18 2012-11-19 2012-11-20 2012-11-21 2012-11-22 2012-11-23 
##      15110       8841       4472      12787      20427      21194 
## 2012-11-24 2012-11-25 2012-11-26 2012-11-27 2012-11-28 2012-11-29 
##      14478      11834      11162      13646      10183       7047 
## 2012-11-30 
##          0
```

Here is a histogram of the steps per day:

```r
hist(stepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Calculating mean and median steps per day:

```r
meanStepsPerDay <- mean(stepsPerDay)
medianStepsPerDay <- median(stepsPerDay)
meanStepsPerDay
```

```
## [1] 9354.23
```

```r
medianStepsPerDay
```

```
## [1] 10395
```
The mean steps per day are 9354.2295082. The median steps per day are 10395.

## What is the average daily activity pattern?

Here is a time series plot of the average number of steps for each time interval:

```r
stepsPerTimeInterval <- tapply(stepDataRaw$steps,stepDataRaw$interval,mean,na.rm=TRUE)
plot(names(stepsPerTimeInterval),stepsPerTimeInterval,type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Calculating interval with highest average steps:

```r
maxAvgStepInterval <- which.max(stepsPerTimeInterval)
names(maxAvgStepInterval)
```

```
## [1] "835"
```

835 is the time interval with the highest average step count.


## Imputing missing values

```r
sum(is.na(stepDataRaw$steps))
```

```
## [1] 2304
```

The total number of rows with missing values is 2304.

To fill in the missing values, I'm going to input the average step data for that time interval across all days.


```r
stepDataNARemoved <- stepDataRaw

stepDataNARemoved[is.na(stepDataNARemoved$steps),"steps"] <- stepsPerTimeInterval[as.character(stepDataNARemoved[is.na(stepDataNARemoved$steps),"interval"])]
```

Here is a histogram of the data with the NAs removed:

```r
NAsRemovedStepsPerDay <- tapply(stepDataNARemoved$steps,stepDataNARemoved$date,sum,na.rm=TRUE)

hist(NAsRemovedStepsPerDay)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

```r
mean(NAsRemovedStepsPerDay)
```

```
## [1] 10766.19
```

```r
median(NAsRemovedStepsPerDay)
```

```
## [1] 10766.19
```

The histogram is more skewed towards the middle after replacing NAs with the average for that time interval. The mean and median are both slightly increased by inputting data for the missing values.

## Are there differences in activity patterns between weekdays and weekends?


```r
stepDataNARemoved$dayOfWeek <- weekdays(as.Date(stepDataNARemoved$date))
stepDataNARemoved$weekend <- "Weekday"
stepDataNARemoved[stepDataNARemoved$dayOfWeek%in%c("Sauturday","Sunday"),"weekend"] <- "Weekend"

averageStepsIntervalWeekend <- tapply(stepDataNARemoved[stepDataNARemoved$weekend=="Weekend","steps"],stepDataNARemoved[stepDataNARemoved$weekend=="Weekend","interval"],mean,na.rm=TRUE)                                                                                                        
averageStepsIntervalWeekday <- tapply(stepDataNARemoved[stepDataNARemoved$weekend=="Weekday","steps"],stepDataNARemoved[stepDataNARemoved$weekend=="Weekday","interval"],mean,na.rm=TRUE)  

par(mfrow=c(2,1))
plot(names(averageStepsIntervalWeekend),averageStepsIntervalWeekend,type='l')
plot(names(averageStepsIntervalWeekday),averageStepsIntervalWeekday,type='l')
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

