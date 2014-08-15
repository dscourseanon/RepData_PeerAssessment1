# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data

This is the submission to the first peer assesment. Setting global echo parameter to TRUE


```r
library(knitr)
opts_chunk$set(echo=TRUE)
```

Loading data in memory for analysis and converting date column into a date class. Assuming data is available in the working directory.


```r
activitydata <- read.csv("activity.csv")
activitydata$date <- as.Date(activitydata$date)
```

## What is mean total number of steps taken per day?
Prep the data by removing the NA values for calculations and summarizing the data. Print histogram of steps by day. Also print median and mean of daily steps.


```r
#Remove rows with NA and aggregate data
activitydata_noNA <- activitydata[complete.cases(activitydata), ]
activitydata_sumday <- aggregate(list(aggsteps = activitydata_noNA$steps), by = list(date = activitydata_noNA$date), FUN = sum)

#Make histogram
hist(activitydata_sumday$aggsteps, main = "Histogram of total steps taken per day", breaks = 10, xlab ="Number of steps")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
#Compute and print summary data
summary1 <- c(mean(activitydata_sumday$aggsteps), median(activitydata_sumday$aggsteps))
names(summary1) <- c("Mean", "Median")
summary1
```

```
##   Mean Median 
##  10766  10765
```

## What is the average daily activity pattern?
Aggregate all the steps by 5 min intervals across days first and then use it to plot a line chart. Also find the interval with the maximum number of steps.


```r
#Plot activity by interval
activitydata_avgtime <- aggregate(list(meansteps = activitydata_noNA$steps), by = list(interval = activitydata_noNA$interval), FUN = mean)
plot(activitydata_avgtime$interval, activitydata_avgtime$meansteps, type = "l", xlab = "Minute interval", ylab ="Avg steps taken", main ="Average steps taken by interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
#Calculate interval with max steps and print it
maxinterval <- which.max(activitydata_avgtime$meansteps)
cat("Max interval:", activitydata_avgtime$interval[maxinterval])
```

```
## Max interval: 835
```

## Imputing missing values
First calculate the total number of rows with NA. Now missing values will be imputed based on the minute interval they were missing from i.e. the mean of that minute interval across all days will be used to substitute the value. After imputing the missing values, the historgram and mean & median information that was printed before is recreated. As we can see below the plot and the values differ because of >10% missing values.


```r
#Identify missing value count
missingvalues <- (nrow(activitydata) - nrow(activitydata_noNA))
cat("Missing values:", missingvalues)
```

```
## Missing values: 2304
```

```r
#Impute missing values
activitydata_impute <- activitydata
for (i in 1:nrow(activitydata_impute)){
  if(is.na(activitydata_impute$steps[i])){
    activitydata_impute$steps[i] <- activitydata_avgtime$meansteps[activitydata_avgtime$interval == activitydata_impute$interval[i]]
  }
} 

# Recreate histogram
activitydata_sumday_impute <- aggregate(list(aggsteps = activitydata_impute$steps), by = list(date = activitydata_impute$date), FUN = sum)
hist(activitydata_sumday_impute$aggsteps, main = "Histogram of total steps taken per day", breaks = 10, xlab ="Number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

```r
# Calculate mean and median and print them
summary2 <- c(mean(activitydata_sumday_impute$aggsteps), median(activitydata_sumday_impute$aggsteps))
names(summary2) <- c("Mean", "Median")
summary2
```

```
##   Mean Median 
##  10766  10766
```

## Are there differences in activity patterns between weekdays and weekends?
First create a factor variable for weekdays and weekend and add that column to the imputed dataset. Create average values for the weekend and weekdays. Use it to plot the weekend and weekday plots.


```r
#Add weekday/weekend flag
for (i in 1:nrow(activitydata_impute)){
  activitydata_impute$weekday[i] <- if(weekdays(activitydata_impute$date[i]) == "Sunday" | weekdays(activitydata_impute$date[i]) == "Saturday") "Weekend" else "Weekday"
}

#Get average by interval for weekdays and weekends
activitydata_weekday <- subset(activitydata_impute, activitydata_impute$weekday == "Weekday")
activitydata_weekend <- subset(activitydata_impute, activitydata_impute$weekday == "Weekend")

activitydata_weekday_avg <- aggregate(list(meansteps = activitydata_weekday$steps), by = list(interval = activitydata_weekday$interval), FUN = mean)

activitydata_weekend_avg <- aggregate(list(meansteps = activitydata_weekend$steps), by = list(interval = activitydata_weekend$interval), FUN = mean)

#Plot the two charts
par(mfrow = c(2,1))
plot(activitydata_weekend_avg$interval, activitydata_weekend_avg$meansteps, type = "l", ylab ="Avg steps taken", xlab ="Interval", main ="Average steps taken by interval weekend")
plot(activitydata_weekday_avg$interval, activitydata_weekday_avg$meansteps, type = "l", xlab ="Interval", ylab ="Avg steps taken", main ="Average steps taken by interval weekday")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 
