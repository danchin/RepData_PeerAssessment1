# Peer Graded Assignment - Course Project 1



# Loading and preprocessing the data

```r
## Set working directory
setwd("~/14 Training/3 Data Science MOOC")

## Load required libraries
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(lattice)
## Read in required dataset
activity <- read.csv("activity.csv", header=TRUE, na.strings = "")

## Convert steps from factor format into numeric format
activity$steps  <- as.numeric(as.character(activity$steps))
```

```
## Warning: NAs introduced by coercion
```

#What is the mean total number of steps taken per day?

```r
## Calculate the total number of steps taken per day
activitysum <- activity %>%
        group_by(date) %>%
        summarise(totalsteps = sum(steps, na.rm=TRUE))

##Plot histogram of steps taken per day
hist(activitysum$totalsteps,
     main="Histogram of Steps Taken Per Day",
     xlab="Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
## Calculate and report mean and median of the total number of steps taken per day
meansteps <- mean(activitysum$totalsteps, na.rm=TRUE)
mediansteps <- median(activitysum$totalsteps, na.rm=TRUE)
```
The mean number of steps taken per day is 9354.2295082  
The median number of steps taken per day is 1.0395\times 10^{4}

#What is the average daily activity pattern?

```r
pattern<-activity %>%
        group_by(interval) %>%
        summarise(avgsteps = mean(steps, na.rm=TRUE))

## Plot time series chart of the 5-minute interval on x-axis vs average number of steps taken on y-axis
plot(pattern$interval, pattern$avgsteps, 
        type="l", 
        main="Average number of steps taken for each 5 minute interval", 
        ylab="Average steps taken", 
        xlab="Time")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
## Calculate which 5 minute interval contains the maximum number of steps
sortedpattern <- arrange(pattern, desc(avgsteps))
maxinterval <- sortedpattern$interval[1]
```
The 5-minute interval with the maximum number of steps starts at 835 am. This is probably the time when the subject was walking to work.

#Inputing missing values


```r
## Calculate and report total number of missing values in the dataset
missingvalues <-sum(is.na(activity$steps))

## Fill all missing values with the mean for that 5 minute interval
activitymeanfilled <- activity %>%
        group_by(interval) %>%
        mutate(
                steps = replace(steps, is.na(steps), mean(steps, na.rm=TRUE))
        )

## Plot new histogram of steps taken per day
activityfilledsum <- activitymeanfilled %>%
        group_by(date) %>%
        summarise(totalsteps = sum(steps, na.rm=TRUE))
hist(activityfilledsum$totalsteps,
     main="Histogram of Steps Taken Per Day",
     xlab="Steps taken per day")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
## Calculate and report mean and median of the total number of steps taken per day
meanstepsnew <- mean(activityfilledsum$totalsteps, na.rm=TRUE)
medianstepsnew <- median(activityfilledsum$totalsteps, na.rm=TRUE)
```
The number of missing values in the dataset is 2304 These missing values were treated by imputing the average number of steps taken for that 5 minute interval across all days.  
The new mean number of steps per day after imputing missing data is 1.0766189\times 10^{4} This is higher than the original mean number of steps ignoring missing data of 9354.2295082 Clearly the impact of imputing missing data with the mean of that 5 minute interval is to raise the average number of steps per day  
The new median number of steps per day after imputing missing data is 1.0766189\times 10^{4}. This is also higher than the original median number of steps ignoring missing data of 1.0395\times 10^{4}

# Are there differences in the activity pattern between weekdays and weekends

```r
## Convert date column format from factor to date
activitymeanfilled$date <- as.Date(activitymeanfilled$date, format = "%Y-%m-%d")



## create a list of weekdays
weekdayslist <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
## create new variable called wday with two lables, weekend and weekday.
activitymeanfilled$wday <- factor((weekdays(activitymeanfilled$date) %in% weekdayslist),
                                  levels=c(FALSE, TRUE), labels = c('weekend', 'weekday'))

## Create dataset with average number of steps for each 5 minute interval grouped by weekend vs weekdays
patternfilled <-activitymeanfilled %>%
        group_by(wday, interval) %>%
        summarise(avgsteps = mean(steps, na.rm=TRUE))
## Create panel plot of the 5 minute interval on the x-axis and the average number of steps taken, averaged across all weekdays and weekends
xyplot(avgsteps~interval | wday, data = patternfilled,
       xlab="interval", ylab="number of steps",
       type="l",
       layout=(c(1,2)))
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

                 
