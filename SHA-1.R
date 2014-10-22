### Libraries

library(lubridate)
library(dplyr)
library(tibble)
library(lattice)

### Reading data
activity <- read.csv("activity.csv")
str(activity)

### What is mean total number of steps taken per day?

### 1. Calculate the total number of steps taken per day

totalbyday <- aggregate(steps~date, activity, sum)
head(totalbyday)

mean_day <- aggregate(steps~date, activity, mean)
head(mean_day)

### 2. Make a histogram of the total number of steps taken each day

hist(totalbyday$steps,
     col = "turquoise",
     main = "Total Number of Steps Taken Each Day", 
     xlab =  "Steps",
     ylab = "Number of Days")  

### 3. Calculate and report the mean and median of the total number of steps taken per day

### 3.1 Calculate mean

mean_steps <- mean(totalbyday$steps)
print(mean_steps)

### 3.2 Calulate median

median_steps <- median(totalbyday$steps)
print(median_steps)

### What is the average daily activity pattern?

### 1. Make a time series plot (type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

### 1.1 The average number of steps taken:

average_steps <- aggregate(steps~interval, activity, mean)
head(average_steps)

### 1.2 Time series plot

with(average_steps,
     plot(interval,
          steps,
          type = "l",
          col= "red",
          main = "Time Series Plot"))

### 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

interval_max <- average_steps[which.max(average_steps$steps),1]
print(interval_max)

### Inputing missing values
### 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

missing_values <- sum(is.na(activity$steps))
print(missing_values)

### Answer 1: The total number of rows with NAs is 2304)

### 2. Replacing the missing values with mean for that 5-minute interval 

### 2.1 Averaging: the mean number of steps per Interval

mean_values <- mean(average_steps$steps)
print(mean_values)

### 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

### 3.1 Create a new dataset

new_df <- activity

### 3.2 Fill the new_df with mean values (37.3826)

new_df[is.na(new_df)] <- 37.3826
head(new_df)

### 4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

totalstepsnew_df <- aggregate(steps ~ date, new_df, sum)
hist(totalstepsnew_df$steps,
     main = "Total Steps Taken Each day -  Replacing NA",
     ylab = "Number of Days",
     xlab = "Steps",
     col = "pink")

### 4.1 Calculate mean

mean_steps_new_df <- mean(totalstepsnew_df$steps)
head(mean_steps_new_df)

### 4.2 Calulate median

median_steps_new_df <- median(totalstepsnew_df$steps)
print(median_steps_new_df)

### Do these values differ from # the estimates from the first part of the assignment? 
### There is insignificant difference.
### When we replacing the NA values we got a increase about number of days but insignificant difference between previous values of mean and median

### What is the impact of inputing missing data on the estimates of the total daily number of steps?
### The number of days increased.

### Are there differences in activity patterns between weekdays and weekends?
### For this part the weekdays() function may be of some help here.
### Use the dataset with the filled-in missing values for this part.

### 1. Create a new factor variable in the dataset with two levels "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

new_df$date <- as.Date(new_df$date)

df <- new_df %>%
    mutate(dayofweek = ifelse(weekdays(new_df$date) == "Saturday" | weekdays(new_df$date) == "Sunday",
                              "weekend","weekday"))

### 2. Make a panel plot containing a time series plot (i.e.type="l") of the 5-minute interval (x-axis) and the average number of steps taken, 
### averaged across all weekday days or weekend days (y-axis)

df2<-df %>%
    group_by(dayofweek, interval) %>%
    summarize(sumsteps=sum(steps))

head(df2)

with(df2,
     xyplot(sumsteps ~ interval | dayofweek, 
            type = "l",      
            main = "Total Number of Steps within Intervals by dayofweek",
            xlab = "Daily Intervals",
            ylab = "Average Number of Steps"))
