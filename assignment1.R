library(dplyr) #reloading here to make group_by work in Knitr
library(ggplot2) #reloading here to make group_by work in Knitr

# Load the data (i.e. read.csv())
#
## Read in the activity data from previously downloaded CSV dataset
unzip("activity.zip", c("activity.csv"))
activity <- read.csv("activity.csv")

## Check to make sure the data is what we expected

head(activity)

# What is mean total number of steps taken per day?
#
# For this part of the assignment, we ignore the missing values in the
# dataset.
#
# Calculate the total number of steps taken per day
#
# If you do not understand the difference between a histogram and a barplot,
# research the difference between them. Make a histogram of the total number of
# steps taken each day
#
#
# Calculate and report the mean and median of the total number of steps taken
# per day
#
#
#
## Sum the total steps per day and aggreate these totals into a new dataframe

totalsteps <- aggregate(steps ~ date, activity, sum)

## Check to make sure the data is what we expected

head(totalsteps)

## Create a histogram of the aggregated steps by day data


hist(totalsteps$steps, breaks=10,
     main = paste("Histogram of Total Steps per Day"), xlab = "Total Steps",
     ylab ="Frequency of Days")
dev.copy(png,'aggsteps.png')
dev.off()
## Calculate the median and mean of the aggregated steps by day data

summary(totalsteps)

## Or calculate the median and mean individually
mean(totalsteps$steps)
median(totalsteps$steps)

## Find the mean of steps per interval and aggreate these into a new dataframe

stepinterval <- aggregate(steps ~ interval, activity, mean)


head(stepinterval)
# What is the average daily activity pattern?
#
# Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
# and the average number of steps taken, averaged across all days (y-axis)
#
# Which 5-minute interval, on average across all the days in the dataset,
# contains the maximum number of steps?



## Plot the data frame to show mean # of steps per 5 minute interval across all
## days of the dataset
plot.new()
plot(stepinterval, type = "l", xlab ="Time of Day",
     ylab = "Mean number of steps",
     main = "Mean steps measured at 5 minute intervals", axes = FALSE)

axis(side=1, at = seq(0, 2400, by = 100))
axis(side=2, at = seq(0, 220, by = 10))
dev.copy(png,'intervals.png')
dev.off()

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
## In the dataframe that's already been grouped by interval, find the row with
## the max number of steps, then display the row data.

stepinterval[which.max(stepinterval$steps), ]


# Imputing missing values
#
# Note that there are a number of days/intervals where there are missing values
# (coded as NA). The presence of missing days may introduce bias into some
# calculations or summaries of the data.
#
# Calculate and report the total number of missing values in the dataset (i.e.
# the total number of rows with NAs)

## Summary reports the number of NAs per variable

summary(activity)

## Or count them individually
sum(is.na(activity$steps))
sum(is.na(activity$interval))
sum(is.na(activity$date))

## Or create a dataframe with the count from individual columns
totalNAs <- sapply(activity, function(x) sum(is.na(x)))
totalNAs



#
# Devise a strategy for filling in all of the missing values in the dataset. The
# strategy does not need to be sophisticated. For example, you could use the
# mean/median for that day, or the mean for that 5-minute interval, etc.
#
# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
#
# Make a histogram of the total number of steps taken each day and Calculate and
# report the mean and median total number of steps taken per day. Do these
# values differ from the estimates from the first part of the assignment? What
# is the impact of imputing missing data on the estimates of the total daily
# number of steps?






##Devise a strategy for filling in all of the missing values in the dataset.
##The strategy does not need to be sophisticated. For example, you could use the
##mean/median for that day, or the mean for that 5-minute interval, etc.

# Create a new dataset that is equal to the original dataset but with the
# missing data filled in.
#
## Duplicate original data to preserve it
activityNoNAs <- activity

## Add the means steps per interval across days data to the dataframe - since
## the intervals in activityNoNAs repeat on the same 288 interval cycle, simply
## adding the cycle to the records matches correctly.  This is a bit of a
## shortcut.

activityNoNAs$meansteps <- stepinterval$steps

## For each time the original steps data is NA, copy the meansteps data in to
## replace it.

head(activityNoNAs)

for (i in 1:nrow(activityNoNAs)){
if (is.na(activityNoNAs$steps[i])) {
  activityNoNAs$steps[i] <- activityNoNAs$meansteps[i]
}
}

# Make a histogram of the total number of steps taken each day and Calculate and
# report the mean and median total number of steps taken per day. Do these
# values differ from the estimates from the first part of the assignment? What
# is the impact of imputing missing data on the estimates of the total daily
# number of steps?

head(activity)
head(activityNoNAs)

totalsteps <- aggregate(steps ~ date, activity, sum)
totalNoNAsteps <- aggregate(steps ~ date, activityNoNAs, sum)


head(totalsteps)
head(totalNoNAsteps)

## Create a histogram of the aggregated steps by day data
par(mfrow=c(2,1))
hist(totalsteps$steps, breaks=10,
     main = paste("Histogram of Total Steps per Day"), xlab = "Total Steps",
     ylab ="Frequency of Days")

hist(totalNoNAsteps$steps, breaks=10,
     main = paste("Histogram of Total Steps per Day (NO NAs)"), xlab = "Total Steps",
     ylab ="Frequency of Days")
dev.copy(png,'noNAhist.png')
dev.off()


## Calculate the median and mean of the aggregated steps by day data

summary(totalsteps)
summary(totalNoNAsteps)

## Or calculate the median and mean individually
mean(totalsteps$steps)
median(totalsteps$steps)
mean(totalNoNAsteps$steps)
median(totalNoNAsteps$steps)

## Find the mean of steps per interval and aggreate these into a new dataframe

stepinterval <- aggregate(steps ~ interval, activity, mean)
stepNoNAsinterval <- aggregate(steps ~ interval, activityNoNAs, mean)


# Are there differences in activity patterns between weekdays and weekends?
#
# For this part the weekdays() function may be of some help here. Use the
# dataset with the filled-in missing values for this part.
#
# Create a new factor variable in the dataset with two levels – “weekday” and
# “weekend” indicating whether a given date is a weekday or weekend day.

# convert date to a date() class variable
totalNoNAsteps$date <- as.Date(strptime(totalNoNAsteps$date, format="%Y-%m-%d"))


totalNoNAsteps$day <- weekdays(totalNoNAsteps$date)                              # build a 'day' factor to hold weekday / weekend
head(totalNoNAsteps)

for (i in 1:nrow(totalNoNAsteps)) {                                       # for each day
  if (totalNoNAsteps[i,]$day %in% c("Saturday","Sunday")) {             # if Saturday or Sunday,
    totalNoNAsteps[i,]$day<-"weekend"                                 #   then 'weekend'
  }
  else{
    totalNoNAsteps[i,]$day<-"weekday"                                 #    else 'weekday'
  }
}


  head(totalNoNAsteps)

#
# Make a panel plot containing a time series plot (i.e. type = "l") of the
# 5-minute interval (x-axis) and the average number of steps taken, averaged
# across all weekday days or weekend days (y-axis). See the README file in the
# GitHub repository to see an example of what this plot should look like using
# simulated data.

head(stepinterval)
head(totalNoNAsteps)


# Are there differences in activity patterns between weekdays and weekends?
#
# For this part the weekdays() function may be of some help here. Use the
# dataset with the filled-in missing values for this part.
#
# Create a new factor variable in the dataset with two levels – “weekday” and
# “weekend” indicating whether a given date is a weekday or weekend day.

# convert date to a date() class variable
activityNoNAs$date <- as.Date(strptime(activityNoNAs$date, format="%Y-%m-%d"))


activityNoNAs$day <- weekdays(activityNoNAs$date)                              # build a 'day' factor to hold weekday / weekend
head(activityNoNAs)

for (i in 1:nrow(activityNoNAs)) {                                       # for each day
  if (activityNoNAs[i,]$day %in% c("Saturday","Sunday")) {             # if Saturday or Sunday,
    activityNoNAs[i,]$day<-"weekend"                                 #   then 'weekend'
  }
  else{
    activityNoNAs[i,]$day<-"weekday"                                 #    else 'weekday'
  }
}

activityNoNAsbyInt <- group_by(activityNoNAs,interval, day) 
activityNoNAsbyInt <- summarise(activityNoNAsbyInt, meansteps = mean(steps))

plot <- ggplot(activityNoNAsbyInt, aes(x = interval, y = meansteps)) + 
  geom_line() + facet_grid(day ~ .) + scale_x_continuous("Time of Day", breaks = seq(0, 2400, by = 100)) +
  scale_y_continuous("Mean number of steps", seq(0, 220, by = 30)) + ggtitle("Mean steps measured at 5 minute intervals")
plot
dev.copy(png,'wkdaywkend.png')
dev.off()

