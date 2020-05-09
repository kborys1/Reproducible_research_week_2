---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(dplyr)
library(ggplot2)
library(lubridate)
```


```r
df <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?



```r
df2 <- df %>% group_by(date) %>% summarise(DailySteps = sum(steps))
mean(df2$DailySteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
ggplot(df2, aes(x=DailySteps)) + geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean(df2$DailySteps, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
median(df2$DailySteps, na.rm = TRUE)
```

```
## [1] 10765
```


## What is the average daily activity pattern?


```r
df3 <- df %>% group_by(interval) %>% summarise(avg_steps = mean(steps, na.rm = TRUE)) 

ggplot(df3, aes(x=interval, y=avg_steps)) +
  geom_line() 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
df3 %>% filter(avg_steps == max(avg_steps))
```

```
## # A tibble: 1 x 2
##   interval avg_steps
##      <int>     <dbl>
## 1      835      206.
```


## Imputing missing values

```r
df4 <- df %>% filter(is.na(steps)) 
nrow(df4)                                                       
```

```
## [1] 2304
```

```r
df4 <- inner_join(df4, df3, by = "interval") 
df4$steps <- ceiling(df4$avg_steps)
df4 <- select(df4, steps, date, interval)

df4c <- filter(df, !is.na(steps))
df5 <- rbind(df4, df4c)

df6 <- df5 %>% group_by(date) %>% summarise(DailySteps = sum(steps))

ggplot(df6, aes(x=DailySteps)) + geom_histogram(binwidth = 1000)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
mean(df6$DailySteps, na.rm = TRUE)
```

```
## [1] 10784.92
```

```r
median(df6$DailySteps, na.rm = TRUE)
```

```
## [1] 10909
```

Note that after filling gaps both median and mean increased.

## Are there differences in activity patterns between weekdays and weekends?

```r
df7 <- df5 %>% mutate(weekend = wday(date) %in% c(1,7)) %>% 
  mutate(weekend = factor(weekend, levels = c("TRUE", "FALSE"), labels = c("Weekend", "Weekday")))

df8 <- df7 %>% group_by(interval,weekend) %>% summarise(avg_steps = mean(steps))


ggplot(df8, aes(x=interval, y=avg_steps)) +
  geom_line() + 
  facet_wrap( ~ weekend, nrow = 2)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
