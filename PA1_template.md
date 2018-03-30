---
title: "Assignment PA1"
output: 
  html_document: 
    keep_md: yes
---



## Loading and preprocessing the data



```r
activity <- read.csv("activity.csv")
activity2 <- activity[which(activity$steps != "NA"),]
```

## What is mean total number of steps taken per day?

Building a data.frame containing the total steps per day, and without "NA´s", and also 
building an histogram, and calculating mean and median:  


```r
library(plyr)
tabla <- ddply(activity2, .(date), summarize, steps = sum(steps))
hist(tabla$steps)
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

Total steps: 

```r
total_steps <- sum(tabla$steps)
total_steps
```

```
## [1] 570608
```
Mean: 

```r
mean_steps <- mean(tabla$steps)
mean_steps
```

```
## [1] 10766.19
```
Median: 

```r
median_steps <- median(tabla$steps)
median_steps
```

```
## [1] 10765
```

## What is the average daily activity pattern? 

Time series plot of the 5-minute interval and the average number of steps taken, averaged across all days: 

```r
mean_intervals <- ddply(activity2, .(interval), summarize, mean = mean(steps))
library(ggplot2)
ggplot(mean_intervals, aes(mean_intervals$interval, mean_intervals$mean))+geom_line()+geom_point(color="steelblue", size=0.2, alpha=1/2)+ggtitle("Averaged pattern per interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

The 5-minute interval which contains the maximum number of steps: 

```r
activity2$interval[which(activity2$steps == max(activity2$steps))]
```

```
## [1] 615
```
## Imputing missing values

Total number of missing values in the dataset:

```r
nrow(activity)-nrow(activity2)
```

```
## [1] 2304
```
Strategy for filling in all of the missing values in the dataset, based on completing with date´s daily mean, and completing days with no data with total daily mean.

```r
tabla <- ddply(activity, .(date), summarize, steps = sum(steps))
m <- mean(tabla$steps[which(!is.na(tabla$steps))])
for (j in (1:nrow(tabla))){
  if(is.na(tabla$steps[j])){tabla$steps[j] <- m}
}
tabla <- cbind(tabla, mean = tabla$steps/(12*24))
activity3 <- activity
for (i in (1:nrow(activity3))){
  if (is.na(activity3$steps[i])){
    fecha <- as.character(as.Date(activity3$date[i]))
    activity3$steps[i] <-  tabla[tabla$date == fecha,]$mean
  }
}
```
For completed dataset, histogram: 

```r
hist(tabla$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

% Diference in Total steps: 

```r
(sum(tabla$steps) - total_steps)*100 / total_steps
```

```
## [1] 15.09434
```
% Diference in Mean: 

```r
(mean(tabla$steps) - mean_steps)*100 / mean_steps
```

```
## [1] 0
```
% Diference in Median: 

```r
(median(tabla$steps) - median_steps)*100 / median_steps
```

```
## [1] 0.01104207
```

## Are there differences in activity patterns between weekdays and weekends?

Creating a new factor variable in the dataset with two levels – “weekday” and 
“weekend” indicating whether a given date is a weekday or weekend day.

```r
x <- factor(weekdays(as.Date(as.character(activity3$date))))
x <- as.character(x)
for (i in 1:length(x)){
  if (x[i]=="sábado"){x[i] <- "weekend"}
  if (x[i]=="domingo"){x[i] <- "weekend"}
  else {x[i] <- "weekday"}
}
activity3 <- cbind(activity3, week = x)
```

Panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") o
f the 5-minute interval (x-axis) and the average number of steps taken, 
averaged across all weekday days or weekend days (y-axis). 

```r
mean_intervals <- ddply(activity3, .(interval, week), summarize, 
                        mean = mean(steps))
ggplot(mean_intervals, aes(interval, mean)) +
geom_line() + facet_grid(week ~ .) + ggtitle("Averaged pattern per interval for weekend and weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
