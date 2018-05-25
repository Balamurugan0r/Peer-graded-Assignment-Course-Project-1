
``` r
library(knitr)
library(ggplot2)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(plyr)
```

    ## -------------------------------------------------------------------------

    ## You have loaded plyr after dplyr - this is likely to cause problems.
    ## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
    ## library(plyr); library(dplyr)

    ## -------------------------------------------------------------------------

    ## 
    ## Attaching package: 'plyr'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     arrange, count, desc, failwith, id, mutate, rename, summarise,
    ##     summarize

1.  Code for reading in the dataset and/or processing the data

``` r
if(!file.exists("getdata-projectfiles-UCI HAR Dataset.zip")) {
  temp <- tempfile()
  download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",temp)
  unzip(temp)
  unlink(temp)
}

data <- read.csv("activity.csv")
```

1.  Histogram of the total number of steps taken each day

``` r
steps_by_day <- aggregate(steps ~ date, data, sum)
hist(steps_by_day$steps, main = paste("Total Steps Each Day"), col="orange", xlab="Number of Steps")
```

![](CourseProject1_files/figure-markdown_github/unnamed-chunk-3-1.png)

1.  Mean and median number of steps taken each day

Ignore the missing values in the dataset

``` r
rmean <- mean(steps_by_day$steps)
rmean
```

    ## [1] 10766.19

``` r
rmedian <- median(steps_by_day$steps)
rmedian
```

    ## [1] 10765

1.  Time series plot of the average number of steps taken

``` r
steps_by_interval <- aggregate(steps ~ interval, data, mean)
par(bg = 'lightblue')
plot(steps_by_interval$interval,steps_by_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![](CourseProject1_files/figure-markdown_github/unnamed-chunk-5-1.png)

1.  The 5-minute interval that, on average, contains the maximum number of steps

``` r
# Max average steps
max_interval <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_interval
```

    ## [1] 835

1.  Code to describe and show a strategy for imputing missing data

``` r
# Impute missing values. Compare imputed to non-imputed data

incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), steps_by_interval$steps[match(data$interval, steps_by_interval$interval)], data$steps))

imputed_data[as.character(imputed_data$date) == "2012-10-01", 1] <- 0
```

1.  Histogram of the total number of steps taken each day after missing values are imputed

``` r
steps_by_day_i <- aggregate(steps ~ date, imputed_data, sum)
hist(steps_by_day_i$steps, main = paste("Total Steps Each Day"), col="Green", xlab="Number of Steps")
```

![](CourseProject1_files/figure-markdown_github/unnamed-chunk-8-1.png)

1.  Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

``` r
# Calculate new mean and median for imputed data

rmean.i <- mean(steps_by_day_i$steps)
rmedian.i <- median(steps_by_day_i$steps)

# Calculate difference between imputed and non-imputed data

mean_diff <- rmean.i - rmean
med_diff <- rmedian.i - rmedian

# Total difference
total_diff <- sum(steps_by_day_i$steps) - sum(steps_by_day$steps)


weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)

library(lattice)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](CourseProject1_files/figure-markdown_github/unnamed-chunk-9-1.png)
