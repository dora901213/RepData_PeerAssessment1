Reproducible Research: Peer Assessment 1
========================================

Introduction
------------

It is now possible to collect a large amount of data about personal movement using activity monitoring devices such as a Fitbit, Nike Fuelband, or Jawbone Up. These type of devices are part of the “quantified self” movement – a group of enthusiasts who take measurements about themselves regularly to improve their health, to find patterns in their behavior, or because they are tech geeks. But these data remain under-utilized both because the raw data are hard to obtain and there is a lack of statistical methods and software for processing and interpreting the data.

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

steps: Number of steps taking in a 5-minute interval (missing values are coded as NA. date: The date on which the measurement was taken in YYYY-MM-DD format interval: Identifier for the 5-minute interval in which measurement was taken The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

Step 1: Loading and preprocessing the data
------------------------------------------

``` r
unzip(zipfile="repdata%2Fdata%2Factivity.zip")
data <- read.csv("activity.csv")
```

Step 2: What is mean total number of steps taken per day?
---------------------------------------------------------

``` r
library(ggplot2)
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)
qplot(total.steps, binwidth=1000, xlab="Total Number of Steps Taken per Day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-1-1.png)

``` r
mean(total.steps, na.rm=TRUE)
```

    ## [1] 9354.23

``` r
median(total.steps, na.rm=TRUE)
```

    ## [1] 10395

According to the above computation, the mean of the total number of steps taken per day is 9354.23, the median of the total number of steps taken per day is 10395.

Step 3: What is the average daily activity pattern?
---------------------------------------------------

``` r
library(ggplot2)
averages <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averages, aes(x=interval, y=steps)) +
    geom_line() +
    xlab("Interval") +
    ylab("Average number of steps taken") +
    ggtitle("Average number of steps per intervals")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-2-1.png)

### Q2: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

``` r
averages[which.max(averages$steps), 1]
```

    ## [1] 835

The 835 interval is the the 5-minute interval which had the maximum number of steps.

Step 4: Imputing missing values
-------------------------------

There are many days/intervals where there are missing values (coded as `NA`). The presence of missing days may introduce bias into some calculations or summaries of the data.

### Q1: Calculate and report the total number of missing values in the dataset

``` r
missing <- is.na(data$steps)
# How many missing
table(missing)
```

    ## missing
    ## FALSE  TRUE 
    ## 15264  2304

Ans Q1: The total number of missing values is 2304.

### Q2: Devise a strategy for filling in all of the missing values in the dataset.

I will convert all of the missing values to the mean value for that 5-minute interval.

``` r
# Replace each missing value with the mean value of its 5-minute interval
fill.value <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps))
        filled <- c(steps)
    else
        filled <- (averages[averages$interval==interval, "steps"])
    return(filled)
}
```

### Q3: Create a new dataset that is equal to the original dataset but with the missing data filled in.

``` r
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)
```

### Q4: Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

``` r
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=1000, xlab="total number of steps taken each day")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-6-1.png)

``` r
mean(total.steps)
```

    ## [1] 10766.19

``` r
median(total.steps)
```

    ## [1] 10766.19

Ans Q4: Mean and median values are higher after imputing missing data with the mean value for that 5-minute interval.

First of all, imputing missing data increases the number of observations that would be taken account when calculating the mean and median of the sample. Secondly, due to the density of missing values in the range of 10,000 to 15,000 steps are relative higher than other intervals, the imputing of missing values in that range further increase the mean and median of the sample.

Step 5: Are there differences in activity patterns between weekdays and weekends?
---------------------------------------------------------------------------------

### Q1: Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

``` r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
        return("weekday")
    else if (day %in% c("Saturday", "Sunday"))
        return("weekend")
    else
        stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)
```

### Q2: Make a panel plot containing a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

``` r
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
    xlab("5-minute interval") + ylab("Number of steps")
```

![](PA1_template_files/figure-markdown_github/unnamed-chunk-8-1.png)
