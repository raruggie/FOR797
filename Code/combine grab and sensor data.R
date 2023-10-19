#Align grab sample chemistry with sensor data for watershed 3

library(readxl)
Sensor <- read.csv("C:\\Users\\kelle\\OneDrive\\Documents\\Fall 2023\\Machine Learning Seminar\\HBF_WQual_Level4_W3Sensor.csv")
grab <- read.csv("C:\\Users\\kelle\\OneDrive\\Documents\\Fall 2023\\Machine Learning Seminar\\HubbardBrook_weekly_stream_chemistry (1).csv")
View(grab)

#subset datasets so I only have the columns I need
grab <- grab[grab$site == "W3", ]
View(Sensor)

library(tidyr)
library(dplyr)

Sensor <- Sensor %>% 
  select(Date, Year, Month, Hour, Minute, Second, FDOMRFU, FDOMQSU, FDOM_corrected_QSU)

#remove NA values from the time of grab, Sensor did not have any NAs to remove
grab <- subset(grab, !is.na(timeEST))
View(grab)


#set the date time correctly
library(lubridate)

class(grab$date) #currently character
grab$date <- ymd(grab$date)
grab$timeEST <- hm(grab$timeEST)

Sensor$Date <- mdy_hm(Sensor$Date)


#to rearrange grab data so it is mdy not ymd to match sensor data, seperate the month day year hour
#minutes into seperate columns then paste them together
grab$month <- month(grab$date)
grab$day <- day(grab$date)
grab$hour <- hour(grab$date)
grab$minute <- minute(grab$timeEST)
grab$year <- year(grab$date)

#filter year to desired year
grab <- grab[grab$year == "2013", ]
View(grab)

Sensor <- Sensor[Sensor$Year == 2013, ]
View(Sensor)

grab$datetime <- mdy_hm(paste(grab$month, grab$day, grab$year, grab$hour, grab$minute))
View(grab)
View(Sensor)
Sensor$datetime <- Sensor$Date

#now that both dataframes have compatable datetime columns, we match them up with each other
library(dplyr)

grab_sensor <- merge(grab, Sensor, by = "datetime", all = TRUE)
View(grab_sensor)

grab_sensor <- grab_sensor %>% 
  select(datetime, FDOMRFU, FDOMQSU, FDOM_corrected_QSU, DOC)

#visualize fdom sensor data versus grab DOC data on same day

library(ggplot2)

grab_sensor$datetime <- as.POSIXct(grab_sensor$datetime)

graph <- ggplot(grab_sensor, aes(x = datetime)) +
  geom_point(aes(y = FDOMRFU, color = "FDOMRFU")) +
  geom_point(aes(y = DOC, color = "DOC")) +
  labs(x = "datetime", y = "Concentration", color = "Dataset") +
  scale_color_manual(values = c("FDOMRFU" = "blue", "DOC" = "red")) +
  theme_minimal() 



graph
  

#random forest
install.packages("randomForest")
library(randomForest)

set.seed(123)
response_variable <- grab_sensor$DOC
predictors <- grab_sensor$FDOMRFU

rf_model <- randomForest(
  formula = as.formula(paste(response_variable, "~", paste(predictors, collapse = "+"))),
  data = grab_sensor,
  ntree = 100,  # Number of trees in the forest (you can adjust this)
  mtry = sqrt(length(predictors))  # Number of variables to consider at each split
)

predictions <- predict(rf_model, newdata = grab_sensor)

#I removed most of the predictors, I gotta put it back in and add it as predictors in my model

# codes to evaluate the model
#rf_model
#mod(rsq)
#plot(rf_model)
#importnace(rf_model)

varImpPlot(rf_model)


