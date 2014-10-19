library(ggplot2) # use in plot
library(lattice) # use in histogram
library(dplyr)   # use in preprocessing

# Loading and preprocessing the data --------------------------------------

# Get csv dataset from the zip file
data = read.csv(unz("activity.zip", "activity.csv"), header=TRUE, colClasses = c("numeric","Date","numeric"))


# What is mean total number of steps taken per day? -----------------------
# Ignoring the missing values in the dataset.
steps_by_day = data %>% group_by(date) %>% summarize(Steps = sum(steps, na.rm=TRUE))

# Histogram of the total number of steps taken each day
histogram(steps_by_day$Steps, breaks = 50, 
          xlab = "Total number of steps per day",
          ylab = "Frecuency",
          col = "blue", type = "count")

# Mean and median of the total number of steps taken per day
mean_original = mean(steps_by_day$Steps)
median_original = median(steps_by_day$Steps)


# What is the average daily activity pattern? -----------------------------
# Calculate average steps for each of 5-minute interval during a 24-hour period
avg_daily_activity = data %>% group_by(interval) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

qplot(x=interval, y=meanSteps, data = avg_daily_activity,  geom = "line",
      xlab="Day interval",
      ylab="Average Number of Steps",
      main="Average Number of Steps Taken Averaged Across All Days"
)


# Imputing missing values -------------------------------------------------
# Missing values imputed with mean
aux = data %>% left_join(avg_daily_activity, by = "interval")
aux$fillSteps = ifelse(is.na(aux$steps), aux$meanSteps, aux$steps)
aux$steps = NULL; aux$meanSteps = NULL

colnames(aux) = c("interval", "date", "steps")
#colnames(aux) = c("date", "interval", "steps")
#aux = aux[,c(3,1,2)]

# Steps by day without missing values
steps_by_day_2 = aux %>% group_by(date) %>% summarize(Steps = sum(steps))

histogram(steps_by_day_2$Steps, breaks = 50, 
          xlab = "Total number of steps per day",
          ylab = "Frecuency",
          col = "blue", type = "count")


# Are there differences in activity patterns between weekdays and  weekends? --------
Sys.setlocale("LC_TIME", "English") # To ensure english weekdays
aux$weekdayType = ifelse( weekdays(aux$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

data_intervales = aux %>% group_by(interval, weekdayType) %>% summarize(meanSteps = mean(steps, na.rm = TRUE))

qplot(x=interval, y=meanSteps, data = data_intervales,  colour = weekdayType, geom = "line",
      xlab="Day interval",
      ylab="Average Number of Steps",
      main="Average Number of Steps Taken Averaged Across All Days"
)
