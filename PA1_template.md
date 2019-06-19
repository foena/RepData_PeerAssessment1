This is an R Markdown document to show the solution as well as the
results of the assignment from week 2.

### Do some cleaning and package loading stuff and load the necessary data set

    rm(list=ls())
    library("dplyr")
    library("ggplot2")
    library("Hmisc")
    setwd("C:/User/Reproducible_Research/week2")
    activity <- read.csv2("activity.csv",sep=",")
    head(activity)

    ##   steps       date interval
    ## 1    NA 2012-10-01        0
    ## 2    NA 2012-10-01        5
    ## 3    NA 2012-10-01       10
    ## 4    NA 2012-10-01       15
    ## 5    NA 2012-10-01       20
    ## 6    NA 2012-10-01       25

### Date transformation

    activity$date <- as.Date(activity$date)

Question 1: What is mean total number of steps taken per day?
-------------------------------------------------------------

Missing values (NANÂ´s) will be ignored. The task is divided into 3
different steps:

### Calculate the total number of steps taken per day

    total_steps_per_day <- activity %>% select(steps, date, interval) %>% group_by(date) %>% 
                           summarise(total_steps = sum(steps))
    head(total_steps_per_day)

    ## # A tibble: 6 x 2
    ##   date       total_steps
    ##   <date>           <int>
    ## 1 2012-10-01          NA
    ## 2 2012-10-02         126
    ## 3 2012-10-03       11352
    ## 4 2012-10-04       12116
    ## 5 2012-10-05       13294
    ## 6 2012-10-06       15420

### Make a histogram of the total number of steps taken each day

    qplot(total_steps_per_day$total_steps, geom="histogram", binwidth = 1060, xlab = "Total steps per day", 
          ylab="Frequency", fill=I("blue"), col=I("black"), alpha=I(.2))

![](assignment_1_files/figure-markdown_strict/unnamed-chunk-4-1.png)

### Calculate and report the mean and median of the total number of steps per day

    mean(na.omit(total_steps_per_day$total_steps))

    ## [1] 10766.19

    median(na.omit(total_steps_per_day$total_steps))

    ## [1] 10765

Question 2: What is the average daily activity pattern?
-------------------------------------------------------

The task is divided into 2 different steps:

### Time series plot of the interval and average number of steps, averaged across days

    average_steps_per_interval <- (na.omit(activity)) %>% select(steps, date, interval) %>% group_by(interval) %>%
                                   summarise(mean_steps = mean(steps))
    head(average_steps_per_interval)

    ## # A tibble: 6 x 2
    ##   interval mean_steps
    ##      <int>      <dbl>
    ## 1        0     1.72  
    ## 2        5     0.340 
    ## 3       10     0.132 
    ## 4       15     0.151 
    ## 5       20     0.0755
    ## 6       25     2.09

    ggplot(data=average_steps_per_interval, aes(x=interval, y=mean_steps)) + geom_line( col="blue") +
        xlab("5-minute interval") + ylab("average number of steps") 

![](assignment_1_files/figure-markdown_strict/unnamed-chunk-8-1.png)

### Which 5-minute interval contains the maximum number of steps?

    maximum <- average_steps_per_interval[which(average_steps_per_interval$mean_steps == max(average_steps_per_interval$mean_steps)), ]
    maximum_interval <- maximum$interval
    print(maximum_interval)

    ## [1] 835

    maximum_steps <- maximum$mean_steps
    print(maximum_steps)

    ## [1] 206.1698

Task 3: Imputing missing values
-------------------------------

The task is divided into 7 different steps:

### Calculate and report the total number of missing values in the dataset

    missing_values_steps <- sum(is.na(as.character(activity$steps)))
    print(missing_values_steps)

    ## [1] 2304

    missing_values_date <- sum(is.na(as.character(activity$date)))
    print(missing_values_date)

    ## [1] 0

    missing_values_interval <- sum(is.na(as.character(activity$interval)))
    print(missing_values_interval)

    ## [1] 0

    missing_values_sum <- missing_values_steps + missing_values_date + missing_values_interval
    print(missing_values_sum)

    ## [1] 2304

### Devise a strategy for filling in all of the missing values in the dataset

Since there is no further information, the missing values of "steps""
are replaced by the the mean value of all existing step data. This can
be done relatively easily with the function "impute".

### Create a new dataset equal to the original one but with the missing data filled in.

    activity_without_nan <- activity
    activity_without_nan$steps <- impute(activity_without_nan$steps,fun=mean)
    head(activity_without_nan)

    ##     steps       date interval
    ## 1 37.3826 2012-10-01        0
    ## 2 37.3826 2012-10-01        5
    ## 3 37.3826 2012-10-01       10
    ## 4 37.3826 2012-10-01       15
    ## 5 37.3826 2012-10-01       20
    ## 6 37.3826 2012-10-01       25

### Make a histogram of the total number of steps taken each day

    total_steps_per_day_without_nan <- activity_without_nan %>% select(steps, date, interval) %>% group_by(date) %>%  summarise(total_steps = sum(steps))

    qplot(total_steps_per_day_without_nan$total_steps, geom="histogram", binwidth = 1060, xlab = "Total steps per day",  ylab="Frequency", fill=I("red"), col=I("black"), alpha=I(.2))

![](assignment_1_files/figure-markdown_strict/unnamed-chunk-13-1.png)

### Calculate and report the mean and median total number of steps taken per day.

    mean_total_steps_without_na <- mean(total_steps_per_day_without_nan$total_steps)
    print(mean_total_steps_without_na)

    ## [1] 10766.19

    median_total_steps_without_na <- median(total_steps_per_day_without_nan$total_steps)
    print(median_total_steps_without_na)

    ## [1] 10766.19

### Do these values differ from the estimates from the first part of the assignment?

    mean_before_imputing <-  mean(na.omit(total_steps_per_day$total_steps))
    print(mean_before_imputing)

    ## [1] 10766.19

    mean_after_imputing <-  mean(total_steps_per_day_without_nan$total_steps)
    print(mean_after_imputing)

    ## [1] 10766.19

    median_before_imputing <-  median(na.omit(total_steps_per_day$total_steps))
    print(median_before_imputing)

    ## [1] 10765

    median_after_imputing <-  median(total_steps_per_day_without_nan$total_steps)
    print(median_after_imputing)

    ## [1] 10766.19

### What is the impact of imputing missing data on the estimates of the total daily number of steps?

The mean value is completly identical. The median value shows slight
differences.

Question 4: Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------------------

The task is divided into 2 different steps:

### Create a new factor variable in the dataset with two levels â âweekdayâ and âweekendâ

    activity_without_nan$days <- weekdays(activity_without_nan$date)
    activity_without_nan$week <- ""
    activity_without_nan[activity_without_nan$days == "Samstag" | activity_without_nan$days == "Sonntag", ]$week <- "weekend"
    activity_without_nan[!(activity_without_nan$days == "Samstag" | activity_without_nan$days == "Sonntag"), ]$week <- "weekday"
    activity_without_nan <- (activity_without_nan) %>% select(steps, date, interval,days,week) %>% group_by(week, interval) %>% summarise(mean_steps = mean(steps))
    print(activity_without_nan)

    ## # A tibble: 576 x 3
    ## # Groups:   week [2]
    ##    week    interval mean_steps
    ##    <chr>      <int>      <dbl>
    ##  1 weekday        0       7.01
    ##  2 weekday        5       5.38
    ##  3 weekday       10       5.14
    ##  4 weekday       15       5.16
    ##  5 weekday       20       5.07
    ##  6 weekday       25       6.30
    ##  7 weekday       30       5.61
    ##  8 weekday       35       6.01
    ##  9 weekday       40       4.98
    ## 10 weekday       45       6.58
    ## # ... with 566 more rows

### Make a panel plot containing a time series plot

    qplot(interval,mean_steps,data=activity_without_nan) + geom_line() + facet_wrap(.~week,  ncol=1) +
    xlab("5-minute interval") +
    ylab("Average number of steps")

![](assignment_1_files/figure-markdown_strict/unnamed-chunk-17-1.png)
