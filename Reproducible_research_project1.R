

library(knitr)
getwd()
setwd("/Users/umarfaruk/Desktop/Coursera Data Science Course/Reproducible Research/RepData_PeerAssessment1")
knit2html("document.Rmd")
browseURL("document.html")
library("data.table")
library(ggplot2)
install.packages("ggplot")

#fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
#download.file(fileUrl, destfile = paste0(getwd(),'/repdata%2Fdata%2Factivity.zip'), method = "curl")
#unzip("repdata%2Fdata%2Factivity.zip", exdir = "data")
getwd()

activityDT<- data.table::fread(input = "data/activity.csv")
activityDT
summary(activityDT)


## Calculation of total number of steps taken per day
Total_Steps <- activityDT[, c(lapply(.SD, sum, na.rm= FALSE)), .SDcols = c("steps"), by= .(date)]
head(Total_Steps, 10)

ggplot(Total_Steps, aes(x= steps)) + 
  geom_histogram(fill="blue", binwidth = 1000)+ labs(title = "Daily Steps", x= "steps", y= "Frequency")

#Calculation and the reporting of the mean and median of the total number of steps taken per day
Total_Steps[, .(Mean_Steps= mean(steps, na.rm = TRUE), Median_Steps = median(steps, na.rm = TRUE))]
 
## Mean_Steps       Median_Steps
##  10766.19          10765


##Make a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval)]
ggplot(IntervalDT, aes(x= interval, y=steps)) + geom_line(color= "blue", size=1) + labs(title = "Avg. Daily Steps", x= "Interval", y = "Avg. Steps per day")

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
IntervalDT[steps == max(steps), .(max_interval = interval)]


##Calculate and report the total number of missing values in the dataset
nrow(activityDT[is.na(steps)])


##Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophiscated. For example, you could use the mean/median for that day, or the mean for the 5-minute interval etc.
    #Filling in missing values with median of dataset.
activityDT[is.na(steps), "steps"]<- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), 
                                               .SDcols = c("steps")]

#Create a new dataset that is equal to the original dataset but with the missing data filled in.
data.table::fwrite(x = activityDT, file= "data/tidyData.csv", quote = FALSE)

#total number of steps taken per day
Total_Steps<- activityDT[, c(lapply(.SD, sum)), .SDcols = c("steps"), by = .(date)]

#mean and median total number of steps taken per day
Total_Steps[, .(Mean_Steps = mean(steps), Median_Steps = median(steps))]


#Histogram 
ggplot(Total_Steps, aes(x=steps)) + geom_histogram(fill = "blue", binwidth = 1000) + labs(title= "Daily Steps", x= "Steps", y= "Frequency")


# Just recreating activityDT from scratch then making the new factor variable. (No need to, just want to be clear on what the entire process is.) 
activityDT <- data.table::fread(input = "data/activity.csv")
activityDT[, date := as.POSIXct(date, format = "%Y-%m-%d")]
activityDT[, `Day of Week`:= weekdays(x = date)]
activityDT[grepl(pattern = "Monday|Tuesday|Wednesday|Thursday|Friday", x = `Day of Week`), "weekday or weekend"] <- "weekday"
activityDT[grepl(pattern = "Saturday|Sunday", x = `Day of Week`), "weekday or weekend"] <- "weekend"
activityDT[, `weekday or weekend` := as.factor(`weekday or weekend`)]
head(activityDT, 10)

###Make a panel plot containing a time series plot (i.e. 𝚝𝚢𝚙𝚎 = "𝚕") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
activityDT[is.na(steps), "steps"] <- activityDT[, c(lapply(.SD, median, na.rm = TRUE)), .SDcols = c("steps")]
IntervalDT <- activityDT[, c(lapply(.SD, mean, na.rm = TRUE)), .SDcols = c("steps"), by = .(interval, `weekday or weekend`)] 

ggplot(IntervalDT , aes(x = interval , y = steps, color=`weekday or weekend`)) + geom_line() + labs(title = "Avg. Daily Steps by Weektype", x = "Interval", y = "No. of Steps") + facet_wrap(~`weekday or weekend` , ncol = 1, nrow=2)