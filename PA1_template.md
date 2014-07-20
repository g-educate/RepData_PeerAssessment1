#Reproducible Research: Peer Assessment 1

##Loading and preprocessing the data

```r
library(sqldf)
```

```
Read and build base datasets
```{r, echo= TRUE}
activity_original <- read.csv("activity.csv", header=TRUE)
activity_exclude_na <- subset(activity_original, !is.na(steps))
activity_only_na <- subset(activity_original, is.na(steps))
```

##Plot the histogram
```{r, echo= TRUE}
total_steps_by_date <- aggregate(steps ~ date, activity_exclude_na, sum)
par(mfrow = c(1, 2))
hist(
  total_steps_by_date$steps,
  main = "Distribution of Total Number of Steps Taken per Day",
  xlab = "Total Number of Steps",
  ylab = "Frequency"
)
plot(
  total_steps_by_date$steps,
  type = "h",
  main = "Total Number of Steps Taken per Day",
  xlab = "Day",
  ylab = "Total Number of Steps"
  )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


## Mean and Median of total number of steps taken
```{r, echo= TRUE}
mean_total_number_of_steps <- mean(total_steps_by_date$steps)
mean_total_number_of_steps
median_total_number_of_steps <- median(total_steps_by_date$steps)
median_total_number_of_steps
```

## What is the average daily activity pattern?


```r
average_steps_by_interval <- aggregate(steps ~ interval, activity_exclude_na, mean)
plot(
    average_steps_by_interval$interval,
    average_steps_by_interval$steps,
    type = "l"
  )
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
interval_with_most_steps_on_average <- subset(average_steps_by_interval, steps==max(steps))
```

## Imputing missing values


```r
number_of_rows_with_na <- sum(is.na(activity_original))
activity_include_na_with_na_replaced <-
  rbind(
    activity_exclude_na,
    sqldf("
      SELECT
        ROUND(abi.steps, 0) AS steps,
        na.date,
        na.interval
      FROM
        activity_only_na na
      INNER JOIN
        average_steps_by_interval abi
          ON abi.interval = na.interval
      ")
    )
```

```
## Loading required package: tcltk
```

```r
total_steps_by_date_2 <- aggregate(steps ~ date, activity_include_na_with_na_replaced, sum)
par(mfrow = c(2, 2))
hist(
  total_steps_by_date$steps,
  main = "Distribution of Total Number of Steps Taken per Day",
  xlab = "Total Number of Steps",
  ylab = "Frequency"
)
plot(
  total_steps_by_date$steps,
  type = "h",
  main = "Total Number of Steps Taken per Day",
  xlab = "Day",
  ylab = "Total Number of Steps"
)
hist(
  total_steps_by_date_2$steps,
  main = "Distribution of Total Number of Steps Taken per Day",
  xlab = "Total Number of Steps",
  ylab = "Frequency"
)
plot(
  total_steps_by_date_2$steps,
  type = "h",
  main = "Total Number of Steps Taken per Day",
  xlab = "Day",
  ylab = "Total Number of Steps"
)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
mean_total_number_of_steps_2 <- mean(total_steps_by_date_2$steps)
median_total_number_of_steps_2 <- median(total_steps_by_date_2$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
activity_include_na_with_na_replaced_with_weekday <-
  cbind(
    activity_include_na_with_na_replaced,
    weekdays(as.POSIXct(activity_include_na_with_na_replaced$date))
    )
colnames(activity_include_na_with_na_replaced_with_weekday) <- c("steps", "date", "interval", "day_name")
activity_include_na_with_na_replaced_with_weekday <-
  sqldf("
        SELECT
          act.*,
          CASE act.day_name
            WHEN 'Monday' THEN 'Weekday'
            WHEN 'Tuesday' THEN 'Weekday'
            WHEN 'Wednesday' THEN 'Weekday'
            WHEN 'Thursday' THEN 'Weekday'
            WHEN 'Friday' THEN 'Weekday'
            ELSE 'Weekend'
          END AS is_weekday
        FROM
          activity_include_na_with_na_replaced_with_weekday act
        ")
average_steps_by_interval_weekday <- aggregate(steps ~ interval + is_weekday, activity_include_na_with_na_replaced_with_weekday, mean)
par(mfrow = c(2, 1))
plot(
  main = "Weekday",
  subset(average_steps_by_interval_weekday, is_weekday == c("Weekday"))$interval,
  subset(average_steps_by_interval_weekday, is_weekday == c("Weekday"))$steps,
  type = "l",
  xlab = "",
  ylab = "Number of Steps"
)
plot(
  main = "Weekend",
  subset(average_steps_by_interval_weekday, is_weekday == c("Weekend"))$interval,
  subset(average_steps_by_interval_weekday, is_weekday == c("Weekend"))$steps,
  type = "l",
  xlab = "Interval",
  ylab = ""
)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 
