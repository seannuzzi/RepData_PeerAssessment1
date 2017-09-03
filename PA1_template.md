# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

The following code demonstrates:

1. Download source data,
1. Create data directory if it does not exists,
1. Unzipping into data directory,
1. Reading CSV into a table with NA processing and
1. Converting date strings to POSIX date objects

```r
#Clear workspace as needed
rm(list = ls())

# Set directory and path info.  Put ourselves in the correct directory
rootdir <-
  'F:/Users/Steve/Documents/RepData_PeerAssessment1'
setwd(rootdir)
destdir <- paste(rootdir, 'data',  sep = '/')
destfilename <- 'activitymonitordata.zip'
datafile <- paste(destdir, destfilename, sep = '/')

# Download file if we do not have source zip
if (!file.exists(datafile)) {
  if (!dir.exists(destdir)) {
    dir.create(destdir, FALSE)
  }
  myurl <-
    'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'
  download.file(myurl, datafile, mode = "wb")
  unzip(datafile, exdir = destdir)
}

# Read file and set NA. File is comma delimited
myData <- read.csv(
  file = paste(destdir, 'activity.csv', sep = '/'),
  header = TRUE,
  sep = ",",
  na.strings = 'NA'
)

#Convert date columns
myData[, 2] <- as.POSIXct(myData[, 2], format="%Y-%m-%d")
```


## What is the average daily activity pattern?
The following depicts how often a number of steps were taken per day.

```r
d <- tapply(myData$steps, myData$date, sum, na.rm = TRUE)
hist(d, breaks = 10, col = 'dark green',
     xlab = "Total steps", 
     ylab = "Frequency per day", 
     main = "Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

The mean and median of total number of steps taken per day are ``9354.23`` and ``10395``.


## Average steps taken at each minute interval
What minute interval is the wearer most active?  The following visualization shows clear patterns of inactivity (0 to 500 interval) with a peek highlighted on the graph.

```r
d <- na.omit(myData)
ts <- setNames(aggregate(d$steps,list(d$interval),mean),c('interval','steps'))

plot(ts$interval,ts$steps,type='l',xlab='Minute Interval',ylab='Steps',main='Daily Average Steps')
lines(lowess(ts$interval,ts$steps), col="blue")
text(ts[which.max(ts$steps),]$interval,ts[which.max(ts$steps),]$steps,round(ts[which.max(ts$steps),]$steps,0))
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Interval with maximum steps is ``835, 206``

## Imputing missing values
Imputing is to assign a value to something by inference from the value of the products or processes to which it contributes.  The purpose of imputing is to see if
missing data influences the overall observation.  Derived values are calculated by the average steps.  The mean/median comparison to imputed to original data reflects a small influence.


```r
d <- myData
# Add missing values 
d$imputed_steps <- with(d, impute(steps, mean))

h <- tapply(d$imputed_steps, d$date, sum)
hist(h, breaks = 10, col = 'dark green',
     xlab = "Total steps", 
     ylab = "Frequency per day", 
     main = "Steps per Day (imputed)")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

We are missing ``2304`` rows from the dataset.

The mean and median are ``10766.19`` and ``10766.19``


## Are there differences in activity patterns between weekdays and weekends?
Two observations can be made from the weekend to weekday analysis.

1. Subject is more 'inactive' in the early intervals (sleeping late?)
1. Subject is overall more active during the weekend implying a sedentary weekly active (desk job?)


```r
d <- myData
# Add missing values based upon average 
d$imputed_steps <- with(d, impute(steps, mean))
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
d$wDay <- c('weekend', 'weekday')[(weekdays(d$date) %in% weekdays1)+1L]
ts <- setNames(aggregate(d$imputed_steps,list(d$interval,d$wDay),mean),c('interval','weekday','steps'))
xyplot(steps~interval|weekday,ts,type='l',layout=(c(1,2)),xlab='Interval',ylab='Number of steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### End of Report
