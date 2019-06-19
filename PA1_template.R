
rm(list=ls())
library("dplyr")
library("ggplot2")
library("Hmisc")
setwd("C:/User/Reproducible_Research/week2")
activity <- read.csv2("activity.csv",sep=",")
head(activity)

###Date transformation

activity$date <- as.Date(activity$date)

##Question 1: What is mean total number of steps taken per day?
        
### Calculate the total number of steps taken per day
        

total_steps_per_day <- activity %>% select(steps, date, interval) %>% group_by(date) %>% 
        summarise(total_steps = sum(steps))

### Make a histogram of the total number of steps taken each day


qplot(total_steps_per_day$total_steps, geom="histogram", binwidth = 1060, xlab = "Total steps per day", 
      ylab="Frequency", fill=I("blue"), col=I("black"), alpha=I(.2))
dev.copy(png,"plot1.png", width=960, height=480)
dev.off()

### Calculate and report the mean and median of the total number of steps per day

mean(na.omit(total_steps_per_day$total_steps))

median(na.omit(total_steps_per_day$total_steps))

##Question 2: What is the average daily activity pattern?
        
### Time series plot of the interval and average number of steps, averaged across days
        
average_steps_per_interval <- (na.omit(activity)) %>% select(steps, date, interval) %>% group_by(interval) %>%
        summarise(mean_steps = mean(steps))
head(average_steps_per_interval)


ggplot(data=average_steps_per_interval, aes(x=interval, y=mean_steps)) + geom_line( col="blue") +
        xlab("5-minute interval") + ylab("average number of steps") 
dev.copy(png,"plot2.png", width=960, height=480)
dev.off()


### Which 5-minute interval contains the maximum number of steps?


maximum <- average_steps_per_interval[which(average_steps_per_interval$mean_steps == max(average_steps_per_interval$mean_steps)), ]
maximum_interval <- maximum$interval
print(maximum_interval)
maximum_steps <- maximum$mean_steps
print(maximum_steps)

##Task 3: Imputing missing values

        ### Calculate and report the total number of missing values in the dataset

missing_values_steps <- sum(is.na(as.character(activity$steps)))
print(missing_values_steps)
missing_values_date <- sum(is.na(as.character(activity$date)))
print(missing_values_date)
missing_values_interval <- sum(is.na(as.character(activity$interval)))
print(missing_values_interval)
missing_values_sum <- missing_values_steps + missing_values_date + missing_values_interval
print(missing_values_sum)

### Devise a strategy for filling in all of the missing values in the dataset

activity_without_nan <- activity
activity_without_nan$steps <- impute(activity_without_nan$steps,fun=mean)
head(activity_without_nan)

### Make a histogram of the total number of steps taken each day 


total_steps_per_day_without_nan <- activity_without_nan %>% select(steps, date, interval) %>% group_by(date) %>%  summarise(total_steps = sum(steps))

qplot(total_steps_per_day_without_nan$total_steps, geom="histogram", binwidth = 1060, xlab = "Total steps per day",  ylab="Frequency", fill=I("red"), col=I("black"), alpha=I(.2))
dev.copy(png,"plot3.png", width=960, height=480)
dev.off()

### Calculate and report the mean and median total number of steps taken per day. 

mean_total_steps_without_na <- mean(total_steps_per_day_without_nan$total_steps)
print(mean_total_steps_without_na)
median_total_steps_without_na <- median(total_steps_per_day_without_nan$total_steps)
print(median_total_steps_without_na)

### Do these values differ from the estimates from the first part of the assignment?

mean_before_imputing <-  mean(na.omit(total_steps_per_day$total_steps))
print(mean_before_imputing)
mean_after_imputing <-  mean(total_steps_per_day_without_nan$total_steps)
print(mean_after_imputing)
median_before_imputing <-  median(na.omit(total_steps_per_day$total_steps))
print(median_before_imputing)
median_after_imputing <-  median(total_steps_per_day_without_nan$total_steps)
print(median_after_imputing)


## Question 4: Are there differences in activity patterns between weekdays and weekends?

### Create a new factor variable in the dataset with two levels â âweekdayâ and âweekendâ 

activity_without_nan$days <- weekdays(activity_without_nan$date)
activity_without_nan$week <- ""
activity_without_nan[activity_without_nan$days == "Samstag" | activity_without_nan$days == "Sonntag", ]$week <- "weekend"
activity_without_nan[!(activity_without_nan$days == "Samstag" | activity_without_nan$days == "Sonntag"), ]$week <- "weekday"
activity_without_nan <- (activity_without_nan) %>% select(steps, date, interval,days,week) %>% group_by(week, interval) %>% summarise(mean_steps = mean(steps))
print(activity_without_nan)

### Make a panel plot containing a time series plot 

qplot(interval,mean_steps,data=activity_without_nan) + geom_line() + facet_wrap(.~week,  ncol=1) +
xlab("5-minute interval") +
ylab("Average number of steps")
dev.copy(png,"plot4.png", width=960, height=480)
dev.off()

