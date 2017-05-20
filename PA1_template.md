Reproducible Research: Peer Assessment 1
================

Loading and preprocessing the data
==================================

1. Load the data (i.e. read.csv())
----------------------------------

Check whether the the **destfile** is present, if not download from the **fileURL** link. Analysing the data using head and str functions.

``` r
destfile <- "repdata%2Fdata%2Factivity.zip" 

fileURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip" 

if (!file.exists(destfile)) 
  {download.file(fileURL ,destfile,method="auto") }

Activity <- read.csv(unz(destfile, "activity.csv"), header = TRUE)

head(Activity,6)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

``` r
str(Activity)
```

    ## 'data.frame':    17568 obs. of  3 variables:
    ##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
    ##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...

2. Process/transform the data (if necessary) into a format suitable for your analysis
-------------------------------------------------------------------------------------

Converting date column in date format.

``` r
Activity$date <- as.Date(Activity$date)

head(Activity,6)
```

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

What is mean total number of steps taken per day?
=================================================

1.Calculate the total number of steps taken per day
---------------------------------------------------

The total number of steps per day is calculated using the aggregate function having argugment of sum.

``` r
TotalStepsPerDay <- aggregate(Activity$steps ~ Activity$date,FUN = sum )

names(TotalStepsPerDay) <- c("Date", "Steps")

head(TotalStepsPerDay,6)
```

    ##         Date Steps
    ## 1 2012-10-02   126
    ## 2 2012-10-03 11352
    ## 3 2012-10-04 12116
    ## 4 2012-10-05 13294
    ## 5 2012-10-06 15420
    ## 6 2012-10-07 11015

2.If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Creating histogram for total steps taken each day.

``` r
hist(x = TotalStepsPerDay$Steps , xlab = "Step" ,main = "Histogram plot of total steps per day")
```

![](PA1_template_files/figure-markdown_github/hist1-1.png)

3.Calculate and report the mean and median of the total number of steps taken per day
-------------------------------------------------------------------------------------

``` r
Mean <- mean(Activity$steps, na.rm = TRUE)
```

``` r
Median <- median(Activity$steps, na.rm = TRUE)
```

The mean of the total number steps taken per day is **37.3825996** and median is **0**.

What is the average daily activity pattern?
===========================================

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
------------------------------------------------------------------------------------------------------------------------------------------------------

The total number of steps per day is calculated using the aggregate function having argugment of sum. Adding names to created variable and the plotting it with type = "l" plot

``` r
Av.StepsbyInterval <- aggregate(Activity$steps ~ Activity$interval,FUN = mean )

names(Av.StepsbyInterval) <- c("Interval", "Average.Steps")

plot( x = Av.StepsbyInterval$Interval, y = Av.StepsbyInterval$Average.Steps , 
     type = "l" , main = "Average number of steps per day for 5 mins interval", xlab = "Interval",
     ylab = "Average steps")
```

![](PA1_template_files/figure-markdown_github/Average%20steps-1.png)

``` r
head(Av.StepsbyInterval,6)
```

    ##   Interval Average.Steps
    ## 1        0     1.7169811
    ## 2        5     0.3396226
    ## 3       10     0.1320755
    ## 4       15     0.1509434
    ## 5       20     0.0754717
    ## 6       25     2.0943396

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
---------------------------------------------------------------------------------------------------------------

``` r
Max.Interval <- 
Av.StepsbyInterval$Interval[which.max(Av.StepsbyInterval$Average.Steps)]
```

The maximum 5-minute interval, on average across all the days in the dataset is **835**

Imputing missing values
=======================

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1.Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
-----------------------------------------------------------------------------------------------------------------

``` r
Missing.Values <- sum(is.na(Activity$steps))
```

The total number of missing values is **2304**.

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Combining next question solutions with 2.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
-------------------------------------------------------------------------------------------------

Creating a Activity.NoNa variable which is initally equal to Activity fata frame.

``` r
Activity.NoNA <- Activity
```

Filling the missing data of the interval with average value of interval. By running the while loop and missing the filling data. Checking if there are any missing values.

``` r
while( sum(is.na(Activity.NoNA$steps)) > 1)
{
  xyz <-  head(Activity.NoNA$interval[is.na(Activity.NoNA$steps)],1) ##Get the first interval value
  abc <-  head(which(is.na(Activity.NoNA$steps)), 1) ##Get the first row
  Activity.NoNA$steps[abc] <-Av.StepsbyInterval$Average.Steps [Av.StepsbyInterval$Interval == xyz]
  rm(xyz)
  rm(abc)
}

sum(is.na(Activity.NoNA$steps))
```

    ## [1] 1

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

``` r
TotalStepsPerDay.NoNa <- aggregate(Activity.NoNA$steps ~ Activity.NoNA$date,FUN = sum )

names(TotalStepsPerDay.NoNa) <- c("Date", "Steps")

par(mfrow = c(1,2))
hist(x = TotalStepsPerDay.NoNa$Steps, xlab = "Step" ,main = "Total steps per day for NoNa")
hist(x = TotalStepsPerDay$Steps, xlab = "Step" ,main = "Total steps per day")
```

![](PA1_template_files/figure-markdown_github/hist2-1.png)

``` r
Mean2 <- mean(Activity.NoNA$steps, na.rm = TRUE)

Median2 <- median(Activity.NoNA$steps, na.rm = TRUE)
```

The new mean after removing the NA values is **37.3846664** which is slightly higher than original mean **37.3825996**. However, the median remains the same with new median value **0** and original being **0**.

Are there differences in activity patterns between weekdays and weekends?
=========================================================================

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
----------------------------------------------------------------------------------------------------------------------------------------------------

Creating a new factor variable with two levels.

``` r
day <- weekdays(Activity.NoNA$date)

Activity.NoNA$day <-   ifelse(day %in% c("Monday","Tuesday","Wednesday","Thursday","Friday") , "Weekday" ,"Weekend")

Activity.NoNA$day <- as.factor(Activity.NoNA$day)
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

Loading the lattice plot library and plotting the data.

``` r
library(lattice)

Av.StepsbyInterval.NoNa <- aggregate(Activity.NoNA$steps ~ Activity.NoNA$interval + Activity.NoNA$day,FUN = mean )

names(Av.StepsbyInterval.NoNa) <- c("Interval", "Day" ,"Steps")

xyplot(Av.StepsbyInterval.NoNa$Steps ~ Av.StepsbyInterval.NoNa$Interval | Av.StepsbyInterval.NoNa$Day
       , layout = c(1,2), type ="l" , xlab = "Interval" , ylab = "Number of steps")
```

![](PA1_template_files/figure-markdown_github/plot-1.png)

Thank you for your time in reviewing the data and codes.

RG
