dataset <- read.csv("activity.csv")
dataset$date <- as.Date(dataset$date,"%Y-%m-%d")

library(dplyr)
x <- group_by(dataset,dataset$date)
total_steps_perday <- summarize(x,sum(steps,na.rm=TRUE))
names(total_steps_perday) <- c("date","total steps per day")
total_steps_perday

library(ggplot2)
ggplot(data = dataset, aes(date,steps)) + geom_histogram(stat = "identity",na.rm = TRUE) + xlab("Days") + ggtitle("Total number of steps taken each day")

mean_of_totalsteps <- ceiling(mean(total_steps_perday$`total steps per day`))
print("MEAN OF TOTAL STEPS TAKEN PER DAY:")
mean_of_totalsteps
median_of_totalsteps <- ceiling(median(total_steps_perday$`total steps per day`))
print("MEDIAN OF TOTAL STEPS TAKEN PER DAY:")
median_of_totalsteps

y <- group_by(dataset,dataset$interval)
average_steps <- summarize(y,mean(steps,na.rm=TRUE))
names(average_steps)<- c("interval","average steps")
plot(average_steps$interval,average_steps$`average steps`,type="l",main="Time series plot of the average number of steps taken", xlab="5-minute interval",ylab="Average number of steps")

interval_max_steps <- average_steps[which.max(average_steps$`average steps`),]$interval
print("THE 5-MINUTE INTERVAL THAT CONTAINS MAXIMUM NO. OF STEPS:")
interval_max_steps

total_missing_values <- sum(is.na(dataset$steps))
print("TOTAL NO. OF ROWS HAVING MISSING DATA")
total_missing_values

new_dataset <- dataset
new_dataset$steps <- ifelse(is.na(new_dataset$steps), 
                            ave(new_dataset$steps,new_dataset$interval, FUN = function(x) median(x,na.rm=TRUE)), 
                            new_dataset$steps)

ggplot(data = new_dataset,aes(date,steps)) + geom_histogram(stat = "identity",na.rm=TRUE) + ggtitle("Total number of steps taken each day after imputing missing values")

x1 <- group_by(new_dataset,new_dataset$date)
total_steps_perday_new <- summarize(x1,sum(steps,na.rm=TRUE))
names(total_steps_perday_new) <- c("date","total steps per day")
total_steps_perday_new

new_mean_of_totalsteps <- ceiling(mean(total_steps_perday_new$`total steps per day`))
print("MEAN OF TOTAL STEPS TAKEN PER DAY AFTER IMPUTING:")
new_mean_of_totalsteps
new_median_of_totalsteps <- ceiling(median(total_steps_perday_new$`total steps per day`))
print("MEDIAN OF TOTAL STEPS TAKEN PER DAY AFTER IMPUTING:")
new_median_of_totalsteps

print("MEAN OF TOTAL STEPS TAKEN PER DAY BEFORE IMPUTING:")
mean_of_totalsteps
print("MEDIAN OF TOTAL STEPS TAKEN PER DAY BEFORE IMPUTING:")
median_of_totalsteps

new_dataset <- mutate(new_dataset, day = ifelse(weekdays(new_dataset$date)=="Sunday" | weekdays(new_dataset$date)=="Saturday","weekend","weekday"))
y1 <- group_by(new_dataset,new_dataset$day,new_dataset$interval)
average_steps_new <- summarize(y1,mean(steps,na.rm=TRUE))
names(average_steps_new)<- c("day","interval","average steps")
ggplot(data = average_steps_new,aes(interval,`average steps`)) + geom_line() + facet_grid(day~.) + ggtitle("Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends")


