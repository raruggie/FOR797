library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)

# Run this line of code when you switch to a new watershed
load(file = 'Processed_Data/Data&Models.Rdata')


# Change the watershed name below when you start
dfWatershed <- TPB
varWatershed <- 'TPB'

################################################################################
### You shouldn't need to alter the code in the next section. You can scroll ###
### down to the next section to start plotting your data                    ###
################################################################################

# Creating lists for the for loop
ListOfSolutes <- list("Cl", "NO3", "SO4", "Na", "K", 
                      "Mg", "Ca", "NH4", "DON", "PO4")
ListOfModels <- list(Modeled_Cl, Modeled_NO3, Modeled_SO4, Modeled_Na, Modeled_K, 
                     Modeled_Mg, Modeled_Ca, Modeled_NH4, Modeled_DON, Modeled_PO4)

# Limits the grab sample data to the watershed of choice
dfHydroGrab <- dfHydroGrab[dfHydroGrab$Sample.Name == varWatershed, ]
# Establishes a df that will only be used by the models to make predictions
dfModelData <- dfWatershed[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]

# This first loop predicts solute concentrations and appends the data to dfWatershed
for (i in 1:length(ListOfSolutes)) {
  PredictedSolute <- predict(ListOfModels[[i]], newdata = dfModelData)
  eval(parse(text = paste("dfWatershed$Pred", ListOfSolutes[i],"<- PredictedSolute", sep = "")))
}

# This section combines the grab sample and sensor data
setnames(dfWatershed, old = c("Date.Time.EST"), new = c("DATETIME"))
dfWatershed$BkpSenseDate <- dfWatershed$DATETIME
dfHydroGrab$BkpGrabDate <- dfHydroGrab$DATETIME
dfSensorGrab <- as.data.table(dfWatershed)[as.data.table(dfHydroGrab), on = .(DATETIME), roll = TRUE]
dfSensorGrab$SenseTimeDiff <- abs(dfSensorGrab$BkpSenseDate - dfSensorGrab$BkpGrabDate)
dfSensorGrab <- subset(dfSensorGrab, SenseTimeDiff  < 1800)
dfSensorGrab <- dfSensorGrab %>% drop_na()

# This loop calculates the NSE values for each solute. For now it simply prints the results.
# In the future we'll update it to store them somewhere useful
for (i in ListOfSolutes) {
  TempNSE <- eval(parse(text = paste("vnse(dfSensorGrab$Pred", i ,", dfSensorGrab$", i , ")", sep = "")))
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

# Make Plots - set up the dataframe for ggplot. To do this:

# subset just the constituent date and columns for obs and pred:

df.pred<-dfSensorGrab[,c(1,9:18)]
df.obs<-dfSensorGrab[,c(1,21:30)]

# pivot longer:

df.pred<-pivot_longer(df.pred, cols = 2:last_col(), names_to = 'Consit', values_to = 'Pred_Value')
df.obs<-pivot_longer(df.obs, cols = 2:last_col(), names_to = 'Consit', values_to = 'Obs_Value')

# remove the pred prefix from the pred df:

df.pred$Consit<-gsub('Pred', '', df.pred$Consit)

# merge these two dataframes:

df.plot<-left_join(df.pred, df.obs, by = c('DATETIME', 'Consit'))

# make plot:

ggplot(df.plot, aes(x = Obs_Value, y = Pred_Value))+
  geom_point()+
  geom_abline(slope=1, linetype = "dashed", color="Red")+
  facet_wrap('Consit', scales = 'free')+
  ggtitle(varWatershed)

