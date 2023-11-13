library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)

# Run this line of code everytime you switch to a new watershed
load(file = 'Processed_Data/Data&Models.Rdata')
load('Processed_Data/W1_Import_Data.Rdata')

# Change the watershed name below when you start
dfWatershed <- W1TrainingData
varWatershed <- 'W1TrainingData'
dfWatershed<-dfWatershed[!grepl("#REF!", NO3_corrected_mgL),]
dfWatershed<- transform(dfWatershed, NO3_corrected_mgL = as.numeric(NO3_corrected_mgL))

str(dfWatershed)

################################################################################
### You shouldn't need to alter the code in the next section. You can scroll ###
### down to the next section to start plotting you data                      ###
################################################################################

# Creating lists for the for loop
ListOfSolutes <- list("Cl", "NO3", "SO4", "Na", "K", 
                      "Mg", "Ca", "NH4", "DON", "PO4", "DOC")
ListOfModels <- list(Modeled_Cl, Modeled_NO3, Modeled_SO4, Modeled_Na, Modeled_K, 
                     Modeled_Mg, Modeled_Ca, Modeled_NH4, Modeled_DON, Modeled_PO4, 
                     Modeled_DOC)

# Limits the grab sample data to the watershed of choice
#dfHydroGrab <- dfHydroGrab[dfHydroGrab$Sample.Name == varWatershed, ]
# Establishes a df that will only be used by the models to make predictions
#dfModelData <- dfWatershed[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
dfModelData <- dfWatershed[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]


# This first loop predicts solute concentrations and appends the data to dfWatershed
for (i in 1:length(ListOfSolutes)) {
  PredictedSolute <- predict(ListOfModels[[i]], newdata = dfModelData)
  #print(PredictedSolute)
  eval(parse(text = paste("dfWatershed$Pred", ListOfSolutes[i],"<- PredictedSolute", sep = "")))
}

importance(Modeled_Cl)

# This section combines the grab sample and sensor data
#setnames(dfWatershed, old = c("Date.Time.EST"), new = c("DATETIME"))
dfWatershed$BkpSenseDate <- dfWatershed$DATETIME
dfHydroGrab$BkpGrabDate <- dfHydroGrab$DATETIME
dfSensorGrab <- as.data.table(dfWatershed)[as.data.table(dfHydroGrab), on = .(DATETIME), roll = TRUE]
dfSensorGrab$SenseTimeDiff <- abs(dfSensorGrab$BkpSenseDate - dfSensorGrab$BkpGrabDate)
dfSensorGrab <- subset(dfSensorGrab, SenseTimeDiff  < 1800)
dfSensorGrab <- dfSensorGrab %>% drop_na()

#dfWatershed <- dfWatershed[-c(21), ]

# This loop calculates the NSE values for each solute. For now it simply prints the results.
# In the future we'll update it to store them somewhere useful
for (i in ListOfSolutes) {
  TempNSE <- eval(parse(text = paste("vnse(dfWatershed$Pred", i ,", dfWatershed$", i , ")", sep = "")))
  print(paste('The NSE for', i, "is:", round(TempNSE, digits=2)))
}


#############################################################################################
### You can begin to code below this line. Useful parameters are as follows:              ###
###                                                                                       ###
### dfWatershed - Contains all predicted solutes at 15 min intervals.                     ###
###             - Helpful for plotting time series                                        ###
###                                                                                       ###
### dfSensorGrab - Contains sensor and grab sample data.                                  ###
###              - Helpful for plotting actual vs predicted solute concentrations.        ###
###              - Also used to calculate NSE if the loop above doesn't do what you need. ###
#############################################################################################

library(ggplot2)
ggplot(dfWatershed, aes(x=NO3, y=PredNO3)) + 
  geom_point()

ggplot(dfWatershed, aes(x=NH4, y=PredNH4)) + 
  geom_point()

ggplot(dfWatershed, aes(x=DOC, y=PredDOC)) + 
  geom_point()

ggplot(dfWatershed, aes(x=DON, y=PredDON)) + 
  geom_point()

ggplot(dfWatershed, aes(x=Cl, y=PredCl)) + 
  geom_point()


summary(dfModelData)


data_test <- dfWatershed[ , c("NH4", "PredNH4")]
lmPred2 = lm(PredNH4 ~ NH4, data = data_test)
summary(lmPred2)
fig<-plot_ly(data = data_test, x = ~NH4, y = ~PredNH4, type = 'scatter')
fig <- fig %>% layout(title = varWatershed, yaxis = list(title = 'Predicted NH4 (mg/L)'), 
                      xaxis = list(title = 'Grab Sample NH4 (mg/L)'))
fig

importance(Modeled_Mg)
