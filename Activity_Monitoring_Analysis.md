---
title: "Activity Monitoring Analysis"
subtitle: "Reproducible Research Course Project 1"
author: "Vaibhav Vivek Sharma"
date: "September 7, 2021"
output: 
  html_document: 
    fig_caption: yes
    fig_height: 7
    fig_width: 9
    keep_md: yes
---

## Introduction

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the "quantified self" movement -- a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.



## Loading And Processing the Data

To Check if the zip file for the data set already exists in the working directory if not then download the zip file.


```r
file <- "activity.zip"
if(!file.exists(file)) {
  fileUrl <- "https://github.com/Anikesh-Aich/RepData_PeerAssessment1/blob/master/activity.zip"
  download.file(fileUrl, file)
}
```

To Check if the csv file for the data set exists in the working directory if not then unzip the zip file.


```r
if(!file.exists("activity.csv")){
  unzip(file)
}
```

1. Loading the Activity Monitoring Data


```r
activity <- read.csv("activity.csv")
```

Checking the Structure of the dataset


```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
tail(activity,10)
```

```
##       steps       date interval
## 17559    NA 2012-11-30     2310
## 17560    NA 2012-11-30     2315
## 17561    NA 2012-11-30     2320
## 17562    NA 2012-11-30     2325
## 17563    NA 2012-11-30     2330
## 17564    NA 2012-11-30     2335
## 17565    NA 2012-11-30     2340
## 17566    NA 2012-11-30     2345
## 17567    NA 2012-11-30     2350
## 17568    NA 2012-11-30     2355
```

2. Processing/Transforming the data set such that there is no missing value.


```r
library(dplyr)

Transformed_activity <- activity %>% 
                        select(date, everything()) %>% 
                        filter(!is.na(steps))
```

## What is Mean Total Number of Steps Taken Per Day?

1. Calculating the Total Number of Steps Taken Per Day


```r
Total_steps_perday <- Transformed_activity %>% 
                      group_by(date) %>%
                      summarize(steps = sum(steps))
```

2. Histogram of the Total Number of Steps Taken Each Day


```r
hist(Total_steps_perday$steps, xlab = "Total Number of Steps Taken Per Day",
     main = "Histogram of Total Number of Steps Taken Each Day", col = "grey", 
     ylim = c(0,20), breaks = seq(0,25000, by = 1000))
```

![](Activity_Monitoring_Analysis_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

3.1 Calculating the Mean of Total Number of Steps Taken Per Day


```r
mean(Total_steps_perday$steps)
```

```
## [1] 10766.19
```

3.2 Calculating the Median of Total Number of Steps Taken Per Day


```r
median(Total_steps_perday$steps)
```

```
## [1] 10765
```

## What is the Average Daily Activity Pattern?

1. Making a Time Series Plot of the 5 Minutes Interval and the Average Number of Steps Taken.


```r
Average_steps_taken_allday <- Transformed_activity %>%
                              group_by(interval)%>%
                              summarize(Average_steps = mean(steps))

plot(Average_steps_taken_allday$interval, 
     Average_steps_taken_allday$Average_steps, type = "l", xlab = "Interval", 
     main = "Average Number Steps Taken per 5 Minutes Interval", 
     ylab = "Average Number of Steps", col = "blue", lwd = 0.5)
```

![](Activity_Monitoring_Analysis_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

2. Which 5 Minutes Interval on Average Across All the Days in the Dataset contains the Maximum Number of Steps


```r
Average_steps_taken_allday[which.max(Average_steps_taken_allday$Average_steps), ]$interval
```

```
## [1] 835
```

## Imputing Missing Values

1. Calculating the total number of missing values in the dataset.


```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```

2. The rounded value of the Average Number of Steps Taken per 5 Minutes Interval is used to replace the NA values.

3. A New Dataset is Created which is equal to the Original Dataset but with the Missing Values filled in.


```r
Imputed_missingValues_activity_data <- activity %>%
                    select(date, everything()) %>%
                    mutate(steps = replace(steps, is.na(steps), 
                    round(Average_steps_taken_allday$Average_steps)))
```

4. Histogram of the total number of steps taken each day.


```r
Total_steps_perday_after_imputation <- Imputed_missingValues_activity_data %>% 
                                       group_by(date) %>%
                                       summarize(steps = sum(steps))

hist(Total_steps_perday_after_imputation$steps, col = "grey", ylim = c(0,20), 
     xlab = "Total Number of Steps Taken Per Day", breaks = seq(0,25000, by = 1000), 
     main = "Histogram of Total Number of Steps Taken Each Day")
```

![](Activity_Monitoring_Analysis_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

Calculating the mean total number of steps taken per day.


```r
mean(Total_steps_perday_after_imputation$steps)
```

```
## [1] 10765.64
```

Calculating the median total number of steps taken per day.


```r
median(Total_steps_perday_after_imputation$steps)
```

```
## [1] 10762
```

These values differ from the estimates from the first part of the assignment. After imputing the missing data it is observed that although the total daily number of steps in the range of 10000-11000 have increased the mean and median values of total number of steps taken each day have decreased.

## Are There Differnces in Actvity patterns between Weekdays and Weekends?

1. Creating a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
library(lubridate)

Final_Data <- Imputed_missingValues_activity_data %>% 
              mutate(Weekdays = wday(Imputed_missingValues_activity_data$date, 
                                     label = TRUE), 
                     Weekdays_Weekends = if_else(Weekdays == "Sat" | 
                                                   Weekdays == "Sun", 
                                                 "Weekend", "Weekday"))
```

2. Making a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
Average_steps_across_Weekday_weekends <- Final_Data %>% 
  group_by(Weekdays_Weekends, interval) %>%
  summarize(steps = mean(steps))

library(ggplot2)

ggplot(Average_steps_across_Weekday_weekends, 
       aes(x = interval, y = steps, color = Weekdays_Weekends)) + 
  geom_line() + 
  labs(title = "Average Daily Steps Averaged Across Weekdays or Weekends", 
       x = "Interval", y = "Average Number of Steps") + 
  facet_wrap(~Weekdays_Weekends, ncol = 1, nrow = 2)
```

![](Activity_Monitoring_Analysis_files/figure-html/unnamed-chunk-18-1.png)<!-- -->

