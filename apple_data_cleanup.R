##### GET DATA

### from csv file

#library(readr)
#apple_health_data <- read_csv("C:/Users/karin/Google Drive/Projects/Data Analysis/Health Analysis/data/apple_health_data.csv", 
#                              col_types = cols(endDate = col_datetime(format = "%Y-%m-%d %H:%M:%S"), 
#                              year = col_integer(), month = col_integer(), 
#                              day = col_integer(), date = col_date(format = "%Y-%m-%d"), 
#                              hour = col_integer(), minutes = col_integer()))

### fresh from xml

library(AppleHealthAnalysis)

health_data <- ah_import_xml("C:/Users/karin/Google Drive/Projects/Data Analysis/Health Analysis/raw data/apple_health_export/Export.xml")
head(health_data)

##### Reduce Size By Eliminating Unneccesary Columns
names(health_data)
essentials <- health_data[,c(1,2,3,4,5)]
head(essentials)

##### Reduce Size By Reducing The Number Of Rows

### count of rows by type
type_count <-by(essentials, essentials$type, nrow)
tail(type_count[order(type_count)], 10)


### check distribution of types with most rows (increasing) for the biggest 5 

## essentials top 5
top5 <- rownames(tail(type_count[order(type_count)], 5))
ess_top5 <- essentials[which(essentials$type %in% top5),]

## distribution
tail(by(essentials$value, essentials$type, summary)[order(type_count)])

library(ggplot2)
p <- ggplot(ess_top5, aes(ess_top5$value)) + geom_histogram() 
p + facet_wrap(vars(ess_top5$type), scales = "free") 

### dheck how many 0 values in the biggest 5
by(ess_top5$value, essentials$type, function(x) sum(x == 0))
sum(ess_top5$value == 0) # -.-


#### frequency of measurements per day

essentials$date <- as.Date(essentials$endDate)

e <- essentials
avg_count_per_day <- by(e, e$type, function(x) length(unique(x$endDate))/length(unique(x$date)) )
avg_count_per_day[order(avg_count_per_day)]

### =====> daily aggregation will reduce the number of rows drastically


#### remove types with less then 50 entries (optional: 52 per fully tracked year)
types_over50 <- rownames(type_count[which(type_count>50)])
essentials <- essentials[which(essentials$type %in% types_over50),]

by(essentials$value, essentials$type, summary)

### ==> some are NA only; others aggregation: sum, avg/mean
types <- unique(essentials$type)
avg_types <- c("BodyMassIndex","Height","BodyMass","HeartRate","BodyFatPercentage","LeanBodyMass",
               "RestingHeartRate","WalkingHeartRateAverage","EnvironmentalAudioExposure",
               "HeadphoneAudioExposure","WalkingDoubleSupportPercentage","WalkingSpeed",
               "WalkingStepLength","WalkingAsymmetryPercentage","HeartRateVariabilitySDNN")
sum_types <- c("BodyMass","StepCount","DistanceWalkingRunning","BasalEnergyBurned",
               "ActiveEnergyBurned","FlightsClimbed", "AppleExerciseTime", "AppleStandTime",
               "AppleStandHour")





##### Menstrucation Cycle Day Index
e<- essentials
men<-(e[which(e$type=="MenstrualFlow"),])
men$Date <- as.Date(men$endDate)

first <- c(min(men$Date))
for (d in 2:nrow(men)){
  if (men$Date[d]-men$Date[d-1]>1){
    first <- cbind(first,as.Date(men$Date[d]))
  } 
}

library(anytime)
f <- anydate(first)

e$Date <- as.Date(e$endDate)


e <- e[which(apple_health_data$year==2020),]

index = 1
for (d in 2:nrow(e)){
  if (e$Date[d] %in% f){
    index <- c(index,1)
  } else if (e$Date[d]==e$Date[d-1]){
    index <- c(index,index[d-1])
  } else { 
    index <- c(index,(index[d-1]+1))
    }
}
