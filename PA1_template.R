## SECTION 1

## 1. READ DATA INTO R
## check if file is in working directory; if not, download and unzip file
if (!file.exists("./activity.csv")) {
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, destfile = "activity_monitoring_data.zip")
  dateDownloaded <- date()
  unzip("activity_monitoring_data.zip")
}

## read data into R
actmon <- read.csv("activity.csv")

## 2. PROCESS DATA AS NECESSARY FOR ASSIGNMENT
## Convert date column to date class; class(actmon$date) revealed factor
## Convert steps column to numeric class; class(actmon$date) revealed integer
## The latter step is required for making a histogram; x must be numeric
actmon$date <- as.Date(actmon$date)
actmon$steps <- as.numeric(actmon$steps)



## SECTION 2

## Per instructions: For this part of the assignment, you can ignore the missing values in the dataset
## Remove NAs from dataset
comactmon <- actmon[complete.cases(actmon),]

## 1. CALCULATE THE TOTAL NUMBER OF STEPS PER DAY 
tsteps <- aggregate(comactmon$steps, list(Day=comactmon$date), sum)

## 2. CREATE HISTOGRAM OF TOTAL STEPS PER DAY
## Note: breaks defaults to 5, which didn't make for a particularly interesting plot
## Setting breaks to 25 better depicts the data - each bar reflects a range of 1,000 steps
library(datasets)
png(file = "plot1.png", width = 480, height = 480, units = "px")
hist(tsteps$x, breaks=25, col="red", xlab="Number of Steps", ylab="Frequency", 
     main= "Histogram of Total Steps per Day")
dev.off()

## 3. CALCULATE MEAN AND MEDIAN OF TOTAL STEPS PER DAY
atsteps <- mean(tsteps$x)
mtsteps <- median(tsteps$x)
print(atsteps)
print(mtsteps)


## SECTION 3

## 1. MAKE TIME SERIES PLOT OF INTERVAL (X-AXIS) AND AVERAGE NUMBER OF STEPS
##    AVERAGED ACROSS ALL DAYS (Y-AXIS)
dayact <- aggregate(comactmon$steps, list(Interval=comactmon$interval), mean)
png(file = "plot2.png", width = 480, height = 480, units = "px")
plot(x=dayact$Interval, y=dayact$x, type="l", xlab="5 Minute Interval", ylab="Average Number of Steps", 
     main="Average Number of Steps")
dev.off()

## 2. DETERMINE THE INTERVAL CONTAINING MAX NUMBER OF STEPS
##    ON AVERAGE ACROSS ALL DAYS IN DATASET
mxstep <- dayact[which.max(dayact$x),]



## SECTION 4

## 1. CALCULATE TOTAL NUMBER OF MISSING VALUES

misno <- sum(is.na(actmon))

## 2. FILL MISSING VALUES

newdata <- actmon
nas <- newdata[!complete.cases(newdata),]
avg <- tapply(newdata$steps, newdata$interval, mean, na.rm=TRUE, simplify=TRUE)
nas$steps <- c(avg)


## 3. CREATE NEW DATASET WITH FILLED DATA (SAME NUMBER OF ROWS)
## Merge datasets (complete cases and replaced data)
filldata <- rbind(comactmon, nas)
filldata <- aggregate(filldata$steps, list(Day=filldata$date), sum)


## 4. MAKE HISTOGRAM OF TOTAL STEPS PER DAY
## Set breaks to 25; consistency with previous chart permits comparison.
library(datasets)
png(file = "plot3.png", width = 480, height = 480, units = "px")
hist(filldata$x, breaks=25, col="red", xlab="Number of Steps", ylab="Frequency", 
     main= "Histogram of Total Steps per Day, NAs replaced")
dev.off()

## 5. CALCULATE AND REPORT MEAN AND MEDIAN STEPS PER DAY

afill <- mean(filldata$x)
mfill <- median(filldata$x)
print(afill)
print(mfill)


## SECTION 5

## 1. CREATE FACTOR VARIABLE WITH TWO LEVELS - WEEKDAY AND WEEKEND
##    INDICATE WHETHER GIVEN DAY IS WEEKDAY OR WEEKEND

newdata <- comactmon
newdata$wd <- weekdays(comactmon$date)
newdata$facwd <- as.factor(c("weekday", "weekend"))
newdata[newdata$wd == "Sunday" | newdata$wd == "Saturday" ,5]<- factor("weekend")
newdata[!(newdata$wd == "Sunday" | newdata$wd == "Saturday"),5 ]<- factor("weekday")


## 2. MAKE PANEL PLOT CONTAINING TIME SERIES (TYPE = "l") WITH
##    5-MIN INTERVAL (X-AXIS) AND AVERAGE NUMBER OF STEPS TAKEN
##    AVERAGED ACROSS ALL WEEKDAYS OR WEEKEND DAYS (Y-AXIS)

avg_we <- subset(newdata, facwd == "weekend") 
avg_wd <- subset(newdata, facwd == "weekday") 
avg_we <- aggregate(avg_we$steps, list(Interval=avg_we$interval), mean)
avg_wd <- aggregate(avg_wd$steps, list(Interval=avg_wd$interval), mean)
d <- data.frame(avg_wd$Interval, avg_wd$x, avg_we$x)
colnames(d) <- c("Interval", "Weekday_Average", "Weekend_Average")


png(file = "plot4.png", width = 960, height = 480, units = "px")
par(mfrow=c(1,2))
plot(d$Interval, d$Weekday_Average, type="l", xlab = "5-Minute Interval", 
     main = "Weekday Activity Patterns", 
     ylab = "Average number of steps", 
     ylim =c(0, 250))
plot(d$Interval, d$Weekend_Average, type="l", xlab = "5-Minute Interval", 
     main = "Weekend Activity Patterns", 
     ylab = "Average number of steps", 
     ylim =c(0, 250))
dev.off()