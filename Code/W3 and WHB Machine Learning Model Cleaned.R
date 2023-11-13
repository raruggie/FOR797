library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)
#library(plotly)
#library(processx)
#install.packages('reticulate')
#reticulate::install_miniconda()
#reticulate::conda_install('r-reticulate', 'python-kaleido')
#reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
#reticulate::use_miniconda('r-reticulate')

#################################################
### This First section is just importing data ###
#################################################

load('Processed_Data/W3GOFWHBImport_Data.Rdata')

####################################################
### The next section deals with making the model ###
####################################################
colnames(W3GOFWHBTraining)
TrainX_Dumb <- W3GOFWHBTraining[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]

TrainY <- W3GOFWHBTraining[ , c("Ca", "Mg", "K", "Na", "NH4", 
                         "SO4", "NO3", "Cl", "PO4", "DON")]

NewX_Dumb <- W3SensorData[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]


for (i in colnames(TrainY)) {
  
  print(i)
  set.seed=500
  vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
  SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
  if (nrow(SelectedX) == 0) {
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
  }
  if (ncol(SelectedX) == 0) {
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
  }
  mod <- randomForest(SelectedX, TrainY[[i]])
  assign(paste("Modeled_", i, sep = ""), mod)
}
#vsurfVar$varselect.interp
#ncol(SelectedX)
rm(list = c('mod', 'NewX_Dumb', 'NewX_Smart', 'SelectedX', 'TrainX_Dumb', 
            'TrainX_Smart', 'TrainY', 'vsurfVar', 'W3SensorData', 'W3TrainingData'))



save.image(file = 'Processed_Data/W3GOFWHBData&Models.Rdata')

#########################################################################################################
### Ignore below this line. The code below was useful when creating other plots but you won't need it ###
#########################################################################################################

#####




### DELETE BELOW WHEN DONE CHECKING


dfWatershed <- W3TrainingData
varWatershed <- 'TPB'

################################################################################
### You shouldn't need to alter the code in the next section. You can scroll ###
### down to the next section to start plotting you data                      ###
################################################################################

# Creating lists for the for loop
ListOfSolutes <- list("Cl", "NO3", "SO4", "Na", "K", 
                      "Mg", "Ca", "NH4", "DON", "PO4")
ListOfModels <- list(Modeled_Cl, Modeled_NO3, Modeled_SO4, Modeled_Na, Modeled_K, 
                     Modeled_Mg, Modeled_Ca, Modeled_NH4, Modeled_DON, Modeled_PO4)

# Limits the grab sample data to the watershed of choice
#dfHydroGrab <- dfHydroGrab[dfHydroGrab$Sample.Name == varWatershed, ]
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
  TempNSE <- eval(parse(text = paste("vnse(dfWatershed$Pred", i ,", dfWatershed$", i , ")", sep = "")))
  print(paste('The NSE for', i, "is:", round(TempNSE, digits=2)))
}




###
data_test <- dfWatershed[ , c("NH4", "PredNH4")]
#data_test <- dfWatershed[ , c("SpConductivity", "NO3", "PredNO3")]

#data_test$Pred <- predict(lmPred, data_test)
lmPred2 = lm(PredNH4 ~ NH4, data = data_test)
summary(lmPred2)
fig<-plot_ly(data = data_test, x = ~NH4, y = ~PredNH4, type = 'scatter')
fig <- fig %>% layout(title = 'W3', xaxis = list(title = 'Grab Sample NO3 (mg/L)'), 
                      yaxis = list(title = 'Pred NO3 (mg/L)'))
fig
###
























  
"""
  y_pred <- predict(mod, newdata = SelectedX) 
  dfPred <- data.frame(TrainY[[i]])
  namei = colnames(dfPred)
  colnames(dfPred)[colnames(dfPred) == namei] ="ActualVar"
  dfPred$PredVar <- y_pred
  
  figActualVPred <- plot_ly(data = dfPred, x = ~ActualVar, y = ~PredVar, type = 'scatter')
  figActualVPred <- figActualVPred %>% layout(title = i)
  OutputFile <- paste(mainDir, subDir, "/",i, "_ActualvPred.png", sep="")
  save_image(figActualVPred, OutputFile)
  
  NewSelectedX <- NewX %>% select(vsurfVar$varselect.pred)
  NewMod <- predict(mod, newdata = NewSelectedX)
  assign(paste("Predicted_", i, sep = ""), predict(mod, newdata = NewSelectedX))


  

  #The code below this line is for saving results
  
  for (q in colnames(SelectedX)) {
    OutputFile <- paste(mainDir, subDir, "/",i, "_", q,"_Partial.png", sep="")
    png(file=OutputFile,
        width=1200, height=700)
    eval(parse(text = paste("partialPlot(mod, SelectedX,", q, ")")))
    dev.off()
    
    
  }
  

  OutputFile <- paste(mainDir, subDir, "/",i, "_VSurfPlots.png", sep="")
  print(OutputFile)
  png(file=OutputFile,
      width=1800, height=1050)
  plot(vsurfVar, var.names = TRUE, main=i)
  dev.off()
  
  
"""
#}
####

plot_ly(data = dfDisSenseGrab, x = ~DOYSin, y = ~TempC, type = 'scatter')
plot_ly(data = dfDisSenseGrab, x = ~DOYSin, y = ~DOC, type = 'scatter')
plot_ly(data = dfDisSenseGrab, x = ~DOYSin, y = ~ionBalance, type = 'scatter')


colnames(dfDisSenseGrab)






#save.image(file = path) ##processed_data
#Create model objects
# Import watershed data
# Import model objects
# Run for watershed
### Below is the random era ###

dfPred <- TrainY[[i]]
New_df <- old_df[ , c(2:5, 8)]
typeof(dfPred)
dfPred <- data.frame(TrainY[[i]])
colnames(dfPred)

births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency = 12, start = c(1946,1))
birthstimeseriescomponents <- decompose(birthstimeseries)
plot(birthstimeseriescomponents)


install.packages("xts")                   

library("xts")
gfg_ts <- xts(dfDisSense$TempC, dfDisSense$DATETIME) 
gfg_ts 
tempCComponents <- decompose(gfg_ts)

temptimeseries <-

plot_ly(data = dfDisSense, x = ~DOYSin, y = ~TempC, type = 'scatter')


y_pred <- predict(mod, newdata = SelectdX) 

dfPred$ActualVar <- i
dfPred$PredVar <- y_pred


lm_model <- linear_reg() %>% 
  set_engine('lm') %>% 
  set_mode('regression') %>%
  fit(PredY ~ Y, data = dfPred) 

lmPred = lm(PredY~Y, data = dfPred) #Create the linear regression
summary(lmPred)

Xs = dfPred$Y
x_range <- seq(min(Xs), max(Xs), length.out = 225)
x_range <- matrix(x_range, nrow=225, ncol=1)
xdf <- data.frame(x_range)
colnames(xdf) <- c('Y')
ydf <- lm_model %>% predict(xdf)
colnames(ydf) <- c('PredY')
xy <- data.frame(xdf, ydf)


fig <- plot_ly(data = dfPred, x = ~Y, y = ~PredY, type = 'scatter')
fig <- fig %>% add_trace(data = xy, x = ~Y, y = ~PredY, name = 'Regression Fit', mode = 'lines', alpha = 1)
fig