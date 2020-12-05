---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Load the data

```r
if (!file.exists('activity.csv')) {
  unzip('activity.zip',overwrite=TRUE)
}
activity <- read.csv("activity.csv", header = T, sep = ",")
```

There is no need to process/transform the data. It already is in a format suitable for the analysis


## What is mean total number of steps taken per day?

Calculate the total number of steps taken per day

```r
su <- tapply(activity$steps, activity$date, sum, na.rm=T)
```

Histogram of the total number of steps taken each day

```r
hist(su, xlab = "Steps sum", main = "Steps per day")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

Calculate and report the mean and median of the total number of steps taken per day

```r
mean_su <- round(mean(su))
median_su <- round(median(su))

print(c("Mean",mean_su))
```

```
## [1] "Mean" "9354"
```

```r
print(c("Median",median_su))
```

```
## [1] "Median" "10395"
```

## What is the average daily activity pattern?

Time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
mn_int <- tapply(activity$steps, activity$interval, mean, na.rm=T)
plot(mn_int ~ unique(activity$interval), type="l", xlab = "5-min interval")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

5-minute interval, on average across all the days in the dataset, which contains the maximum number of steps

```r
mn_int[which.max(mn_int)]
```

```
##      835 
## 206.1698
```

## Inputing missing values

Total number of missing values in the dataset

```r
table(is.na(activity) == TRUE)
```

```
## 
## FALSE  TRUE 
## 50400  2304
```

Strategy for filling in all of the missing values in the dataset: replace any NA with the mean of the corresponding interval

New dataset that is equal to the original dataset but with the missing data filled in

```r
activity2 <- activity
for (i in 1:nrow(activity)){
    if(is.na(activity$steps[i])){
        activity2$steps[i]<- mn_int[[as.character(activity[i, "interval"])]]
    }
}
```

Histogram of the total number of steps taken each day

```r
su2 <- tapply(activity2$steps, activity2$date, sum, na.rm=T)
hist(su2, xlab = "Steps sum", main = "Steps per day")
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

New mean and median total number of steps taken per day

```r
mean_su2 <- round(mean(su2))
median_su2 <- round(median(su2))

print(c("New Mean",mean_su2))
```

```
## [1] "New Mean" "10766"
```

```r
print(c("New Median",median_su2))
```

```
## [1] "New Median" "10766"
```

Impact of imputing missing data on the estimates of the total daily number of steps:
1. We set about 14% of new values, all of them with the same value of the all mean, so the result is that we drive both the mean and median to values closer to the old mean
2. Surprisingly, this drove both the median and the mean to the same value.

## Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
Sys.setenv(LANGUAGE = "en")
Sys.setlocale("LC_TIME", "English")
```

```
## Warning in Sys.setlocale("LC_TIME", "English"): OS reports request to set
## locale to "English" cannot be honored
```

```
## [1] ""
```

```r
activity2$weekday <- c("weekday")
activity2[weekdays(as.Date(activity2[, 2])) %in% c("Saturday", "Sunday", "saturday", "sunday"), ][4] <- c("weekend")
activity2$weekday <- factor(activity2$weekday)
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis)

```r
activity2_weekend <- subset(activity2, activity2$weekday == "weekend")
activity2_weekday <- subset(activity2, activity2$weekday == "weekday")
mean_activity2_weekday <- tapply(activity2_weekday$steps, activity2_weekday$interval, mean)
mean_activity2_weekend <- tapply(activity2_weekend$steps, activity2_weekend$interval, mean)

library(lattice)
df_weekday <- data.frame(interval = unique(activity2_weekday$interval), avg = as.numeric(mean_activity2_weekday), day = rep("weekday", length(mean_activity2_weekday)))
df_weekend <- data.frame(interval = unique(activity2_weekend$interval), avg = as.numeric(mean_activity2_weekend), day = rep("weekend", length(mean_activity2_weekend)))
df_final <- rbind(df_weekday, df_weekend)

xyplot(avg ~ interval | day, data = df_final, layout = c(1, 2), type = "l", ylab = "Average Number of Steps")
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png) 
