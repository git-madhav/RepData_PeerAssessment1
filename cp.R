workDir <- "D:\\Personal\\Madhav\\Learning\\DataScience\\05-ReproducibleResearch\\cp"
setwd(workDir)

urlFile <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

zipFileName <- "repdata_data_activity.zip"
download.file(urlFile, zipfilename)

zipFileName <- "activity.zip"
unzip(zipFileName)

csvFile <- "activity.csv"
activity.df <- read.csv(csvFile, na.strings=c("NA","#DIV/0!",""))

# Mean total number of steps taken per day
byDate <- group_by(activity.df, date)
sumDate <- summarise(byDate, totalSteps = sum(steps))
sumDate2 <- filter(sumDate, !is.na(totalSteps))
summarise(sumDate2, meanStepsPerDay = mean(totalSteps), medianStepsPerDay = median(totalSteps))
library(ggplot2)
ggplot(sumDate2, aes(x = totalSteps)) + 
  geom_histogram(fill = "red", binwidth = 1000) +
  labs(title = "Hist", x = "Steps per day", y = "frequency")

hist(sumDate2$totalSteps, breaks = 20, xlab = "Total steps per day", main = "Total Steps Taken Per Day", col = "lightgreen", xlim = c(0, 25000))
abline(v = mean(sumDate2$totalSteps), col = "royalblue", lwd = 2)
abline(v = median(sumDate2$totalSteps), col = "darkred", lwd = 2)
legend(x = "topright", c("Mean", "Median"), col = c("royalblue", "darkred"), lwd = c(2, 2))
meanSteps <- mean(sumDate2$totalSteps)
medianSteps <- median(sumDate2$totalSteps)
meanSteps
# Mean number of steps per day : 10766.19
medianSteps
# Median value of Total number of steps per day : 10765


#What is the average daily activity pattern?
activity.df2 <- filter(activity.df, !is.na(steps))
byInterval <- group_by(activity.df2, interval)
meanStepsInterval = summarise(byInterval, meanSteps = mean(steps))

# plot the time series plot
ggplot(meanStepsInterval, aes(x = interval, y = meanSteps)) + geom_line(color = "royalblue") + labs(title = "Time Series Plot: Avg. Daily Steps", x = "Interval", y = "Average Steps")

plot(x = meanStepsInterval$interval, y = meanStepsInterval$meanSteps, type = "l", xlab = "Interval", ylab = "Average Steps", main = "Time Series: Avg. Daily Activity", col = "Blue")

# find max average Steps per interval across all days
maxIntervalSteps <- meanStepsInterval[meanStepsInterval$meanSteps == max(meanStepsInterval$meanSteps),]

maxIntervalSteps <- meanStepsInterval[which.max(meanStepsInterval$meanSteps),]

maxIntervalSteps

#Interval 835 contains maximum number of average steps per interval across all days.

# Imputing missing values

totalMissingValues <- sum(is.na(activity.df$steps))
# total number of missing values : 2304

# Strategy : After examing, entire first day doesn't have any values, taking an average daily value doesn't make sense. Let us take average interval value across all days as value to fill missing values.

# implement the strategy
activity.df.full <- activity.df
na.values <- is.na(activity.df.full$steps)
mSteps <- meanStepsInterval$meanSteps
names(mSteps) = as.character(meanStepsInterval$interval)
activity.df.full$steps[na.values] <- mSteps[as.character(activity.df.full$interval[na.values])]
missingValues <- sum(is.na(activity.df.full$steps))
missingValues 
# now missing values are zero

#calculate mean and median
byDate.full <- group_by(activity.df.full, date)
sumDate.full <- summarise(byDate.full, totalSteps = sum(steps))
meanSteps.full <- mean(sumDate.full$totalSteps)
medianSteps.full <- median(sumDate.full$totalSteps)
meanSteps.full
# Mean value of Total number of Steps per day : 10766.19
medianSteps.full
# Median value of Total number of steps per day : 10766.19

# Compared to original data set, median is slightly shifted due to imputing missing values by average number of steps per 5 minute interval. Mean and Median values of new data set is identical. 

hist(sumDate.full$totalSteps, breaks = 20, xlab = "Total steps per day", main = "Total Steps Taken Per Day, with imputed missing values", col = "lightgreen", xlim = c(0, 25000))
abline(v = mean(sumDate2$totalSteps), col = "royalblue", lwd = 2)
abline(v = median(sumDate2$totalSteps), col = "darkred", lwd = 2)
legend(x = "topright", c("Mean", "Median"), col = c("royalblue", "darkred"), lwd = c(2, 2))

#Are there differences in activity patterns between weekdays and weekends?
fullData.df <- activity.df.full
fullData.df <- mutate(fullData.df, day = weekdays(as.Date(date)))
fullData.df <- mutate(fullData.df, dayType = ifelse(day == "Saturday" | day == "Sunday", "weekend", "weekday"))
fullData.df$dayType <- as.factor(fullData.df$dayType)


byInterval.full <- group_by(fullData.df, interval, dayType)
meanStepsInterval.full <- summarise(byInterval.full, meanSteps = mean(steps))

tsPlot <- ggplot(meanStepsInterval.full, aes(x=interval, y=meanSteps)) +
          geom_line(color = "royalblue") + 
          labs(title = "Time Series Plot : Avg. Daily Steps", 
               x = "Interval", 
               y = "Average Steps") + 
          facet_wrap(~dayType, ncol = 1, nrow = 2)

print(tsPlot)

library(lattice)
xyplot(meanSteps ~ interval | dayType, data = meanStepsInterval.full, type = "l", main = "Time Series: Avg. Daily Activity", ylab = "Average Steps", xlab = "Interval", layout = c(1, 2), col = "royalblue")


