---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

This is my assignment 1 in Reproducible Research course.

First of all it was need load the data:


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
##Function to Read txt files
readData <- function(path, fileName) {
  
  file <- paste(path,"\\", fileName, sep = "")
  dat <- read.csv(file)
  dat
  
}

rootPath <- "D:\\Arquivos de Projetos\\[R] Codes\\reproducible_research"
flName <- "activity.csv"
dataSet <- readData(rootPath, flName)
```


## What is mean total number of steps taken per day?

The code above remove the missing value as asked in exercise:


```r
subDataSet <- subset(dataSet, !is.na(dataSet$steps))
```

The First question was "Calculate the total number of steps taken per day". For this, the following code solve:


```r
result <-
      subDataSet %>%
      select(date, steps) %>%
      group_by(date) %>%
      summarize(
            sum(steps)
      )
colNames <- c("Dates","sum.Of.Steps") 
names(result) <- colNames

sumOfSteps <- sum(result$sum.Of.Steps)
sumOfSteps
```

```
## [1] 570608
```

The Second question was "If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day"


```r
hist(result$sum.Of.Steps)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

And finally, the third question was: "Calculate and report the mean and median of the total number of steps taken per day"

```r
meanOfSteps <- mean(result$sum.Of.Steps)
medianOfSteps <- median(result$sum.Of.Steps)
meanOfSteps
```

```
## [1] 10766.19
```

```r
medianOfSteps
```

```
## [1] 10765
```

## What is the average daily activity pattern?

Next group of question was using the same data set without missing values 

*"Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)" 


```r
resultPart1_2 <-
      subDataSet %>%
      select(interval, steps) %>%
      group_by(interval) %>%
      summarize(
            mean(steps)
      )
colNames <- c("Interval","mean.Of.Steps") 
names(resultPart1_2) <- colNames
```


```r
plot(resultPart1_2$Interval, 
     resultPart1_2$mean.Of.Steps, 
     type="l",
     ylab = "Mean Of Steps",
     xlab = "5-Minute Interval")
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png) 

*"Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?"


```r
maximo<-max(resultPart1_2$mean.Of.Steps)
interval <-
      resultPart1_2 %>%
      select(Interval, mean.Of.Steps)%>%
      filter(mean.Of.Steps == maximo)
interval
```

```
## Source: local data frame [1 x 2]
## 
##   Interval mean.Of.Steps
## 1      835      206.1698
```


## Imputing missing values
*"Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)"

```r
missingValue <- subset(dataSet$steps, is.na(dataSet$steps)) 
length(missingValue)##2304
```

```
## [1] 2304
```

*"Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc."/"Create a new dataset that is equal to the original dataset but with the missing data filled in."


```r
x <- length(dataSet$steps)
newSteps = 1:x

for(i in seq_along(newSteps)){
      if(is.na(dataSet$steps[i])){
            newSteps[i] = medianOfSteps
      }else{
            newSteps[i] = dataSet$steps[i]
      }
} 

newDataSet <- cbind(dataSet, newSteps)

resultPart2 <-
      newDataSet %>%
      select(date, newSteps) %>%
      group_by(date) %>%
      summarize(
            sum(newSteps)
      )
colNames <- c("Dates","sum.Of.newSteps") 
names(resultPart2) <- colNames

sumOfSteps <- sum(resultPart2$sum.Of.newSteps)
meanOfSteps <- mean(resultPart2$sum.Of.newSteps)
medianOfSteps <- median(resultPart2$sum.Of.newSteps)

sumOfSteps
```

```
## [1] 25373168
```

```r
meanOfSteps
```

```
## [1] 415953.6
```

```r
medianOfSteps
```

```
## [1] 11458
```

*"Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?" 
the answer is Yes. Improve total of days in the first group in histogram.


```r
hist(resultPart2$sum.Of.newSteps)
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 


## Are there differences in activity patterns between weekdays and weekends?

*"Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day."


```r
WeekDay <- weekdays(as.Date(newDataSet$date), abbreviate = FALSE)
week <- WeekDay

for(i in seq_along(WeekDay)){
      if(WeekDay[i] == "sábado" || WeekDay[i] == "domingo"){
            week[i] = "weekend"
      }else{
            week[i] = "weekday"
      }
} 
weekDataSet <- cbind(newDataSet, week)

resultPart2_2 <-
      weekDataSet %>%
      select(week, interval, newSteps) %>%
      group_by(week, interval) %>%
      summarize(
            mean(newSteps)
      )
colNames <- c("week", "interval","Mean.Of.newSteps") 
names(resultPart2_2) <- colNames
```

*"Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data."


```r
qplot(interval, 
      Mean.Of.newSteps, 
      data = resultPart2_2,
      geom = "line",
      facets  =      .      ~	week,
      ylab = "Mean Of Steps",
      xlab = "5-Minute Interval")
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png) 
