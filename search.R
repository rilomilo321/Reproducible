
activity <- read.csv("~/project2/activity.csv")


library(ggplot2)
library(dplyr)


str(activity)


StepsPerDay <- aggregate(activity$steps, list(activity$date), FUN=sum)
colnames(StepsPerDay) <- c("Date", "Steps")
StepsPerDay


g <- ggplot(StepsPerDay, aes(Steps))

g+geom_histogram(boundary=0, binwidth=2500, col="darkblue", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,18,2))


mean(StepsPerDay$Steps, na.rm=TRUE)


median(StepsPerDay$Steps, na.rm=TRUE)


StepsPerTime <- aggregate(steps~interval,data=activity,FUN=mean,na.action=na.omit)

StepsPerTime$time <- StepsPerTime$interval/100

h <- ggplot(StepsPerTime, aes(time, steps))

h+geom_line(col="red")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))


ST <- tbl_df(StepsPerTime)

ST %>% select(time, steps) %>% filter(steps==max(ST$steps))


ACT <- tbl_df(activity)

ACT %>% filter(is.na(steps)) %>% summarize(missing_values = n())


activity$CompleteSteps <- ifelse(is.na(activity$steps), round(StepsPerTime$steps[match(activity$interval, StepsPerTime$interval)],0), activity$steps)


activityFull <- data.frame(steps=activity$CompleteSteps, interval=activity$interval, date=activity$date)

head(activityFull, n=10)


StepsPerDayFull <- aggregate(activityFull$steps, list(activityFull$date), FUN=sum)
colnames(StepsPerDayFull) <- c("Date", "Steps")

g <- ggplot(StepsPerDayFull, aes(Steps))

g+geom_histogram(boundary=0, binwidth=2500, col="orange", fill="lightblue")+ggtitle("Histogram of steps per day")+xlab("Steps")+ylab("Frequency")+theme(plot.title = element_text(face="bold", size=12))+scale_x_continuous(breaks=seq(0,25000,2500))+scale_y_continuous(breaks=seq(0,26,2))


mean(StepsPerDayFull$Steps)


median(StepsPerDayFull$Steps)


activityFull$RealDate <- as.Date(activityFull$date, format = "%Y-%m-%d")

activityFull$weekday <- weekdays(activityFull$RealDate)

activityFull$DayType <- ifelse(activityFull$weekday=='Saturday' | activityFull$weekday=='Sunday', 'weekend','weekday')

head(activityFull, n=10)


StepsPerTimeDT <- aggregate(steps~interval+DayType,data=activityFull,FUN=mean,na.action=na.omit)

StepsPerTimeDT$time <- StepsPerTime$interval/100

j <- ggplot(StepsPerTimeDT, aes(time, steps))
j+geom_line(col="darkgreen")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
