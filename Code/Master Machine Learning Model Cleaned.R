library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)
library(plotly)
library(caret)
#install.packages('caret')
#library(processx)
#install.packages('reticulate')
#reticulate::install_miniconda()
#reticulate::conda_install('r-reticulate', 'python-kaleido')
#reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
#reticulate::use_miniconda('r-reticulate')

#################################################
### This First section is just importing data ###
#################################################

load('Processed_Data/Master_Import_Data.Rdata')

####################################################
### The next section deals with making the model ###
####################################################

Watersheds <- list("BDC")#, "BEF", "DCF", "GOF", "HBF", 
                         #"LMP", "MCQ", "SBM", "TPB", "WHB", "W1", "W3", "W9")
Watersheds <- list("GOF")

dfTraining <- dfMaster[dfMaster$Ca != Watersheds[[j]],]
dfTraining <- dfMaster[dfMaster$Watershed != "GOF"]

folds <- create_folds(dfTraining$Ca, k = 5, seed = 2734, m_rep = 3)
folds <- create_folds(dfTraining$Ca, k = 5, seed = 2734, m_rep = 3)
cv_rmse1 <- numeric(15)



for (j in seq_along(folds)) {
  print(j)
  insample <- dfTraining[folds[[j]], ]
  out <- dfTraining[-folds[[j]], ]
  
  TrainX_Dumb <- insample[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
  TrainY <- insample[ , c("Ca")]
  
  for (i in colnames(TrainY)) {
    
    print(i)
    set.seed=500
    vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
    if (nrow(SelectedX) == 0) {
      SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
    }
    mod <- randomForest(SelectedX, TrainY[[i]])#, nodesize = 25, maxnodes = 45)
    #assign(paste("AB1_", Watersheds[[j]],"_Modeled_", i, sep = ""), mod)
    assign(paste("AB1_GOF_Modeled_", i, sep = ""), mod)
    tempFileName <- paste(tempPath, "/Modeled_", i, ".Rdata", sep = "")
    save(list = paste("AB1_GOF_Modeled_", i, sep = ""), file = tempFileName)
  }
  
  
  #fit1 <- randomForest(TrainX_Dumb, TrainY)
  
  cv_rmse1[j] <- vnse(out$Ca, predict(mod, out))
  print(cv_rmse1)
}

dfTesting2 <- dfMaster[dfMaster$Watershed == "GOF"]
TestingX_Dumb <- dfTesting[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
predict(mod, dfTesting)

vnse(dfTesting2$Ca, predict(mod, dfTesting2))


typeof(TrainX_Dumb)

for (j in 1:length(Watersheds)) {
  print(Watersheds[[j]])
  
  
  dfTraining <- dfMaster[dfMaster$Watershed != Watersheds[[j]],]
  dfTesting <- dfMaster[dfMaster$Watershed == Watersheds[[j]],]
  #dfTraining <- dfMaster
  #dfTesting <- dfMaster
  
  TrainX_Dumb <- dfTraining[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
  
  TrainY <- dfTraining[ , c("Cl", "NO3", "SO4", "Na", "K", 
                            "Mg", "Ca", "NH4", "DON", "PO4", "DOC"
                            )]
  
  
  tempPath <- paste("Models/AB1_", Watersheds[[j]], sep = "")
  if(!dir.exists(tempPath)){
    dir.create(tempPath)
  }
  
  for (i in colnames(TrainY)) {
    
    print(i)
    set.seed=500
    vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
    if (nrow(SelectedX) == 0) {
      SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
    }
    mod <- randomForest(SelectedX, TrainY[[i]])#, nodesize = 25, maxnodes = 45)
    assign(paste("AB1_", Watersheds[[j]],"_Modeled_", i, sep = ""), mod)
    tempFileName <- paste(tempPath, "/Modeled_", i, ".Rdata", sep = "")
    save(list = paste("AB1_", Watersheds[[j]],"_Modeled_", i, sep = ""), file = tempFileName)
  }
  
}





#rm(list = c('mod', 'NewX_Dumb', 'NewX_Smart', 'SelectedX', 'TrainX_Dumb', 
#            'TrainX_Smart', 'TrainY', 'vsurfVar', 'W3SensorData', 'W3TrainingData',
#            'varWatershed', 'dfHydroGrabLMP', 'dfModelData', 'dfSensorGrab', 'dfWatershed',
#            'i'))



#save.image(file = 'Processed_Data/MasterData&Models.Rdata')




plot(AB1_GOF_Modeled_Cl)
importance(Master_Modeled_Ca)
hist(treesize(AB1_GOF_Modeled_Cl))






#########################################################################################################
### Ignore below this line. The code below was useful when creating other plots but you won't need it ###
#########################################################################################################

#####




### DELETE BELOW WHEN DONE CHECKING
WatershedVar = "GOF"

ModelList <- list.files(path=paste("Models/AB1_", WatershedVar, sep= ""), pattern=NULL, all.files=FALSE, 
           full.names=FALSE)

for (i in ModelList) {
  load(paste("Models/AB1_", WatershedVar, "/", i, sep = ""))
}




dfWatershed <- dfMaster[dfMaster$Watershed == WatershedVar,]
#dfWatershed <- dfTraining
#varWatershed <- 'TPB'

#dfWatershed <- dfWatershed %>% drop_na()

################################################################################
### You shouldn't need to alter the code in the next section. You can scroll ###
### down to the next section to start plotting you data                      ###
################################################################################

# Creating lists for the for loop

ListOfSolutes <- list("Ca")#, "Cl", "DOC", "DON", "K", 
                      #"Mg", "Na", "NH4", "NO3", "PO4", "SO4")
ListOfModels <- ls(pattern=paste("^AB1_", WatershedVar, sep= ""))

#get(ListOfModels[[1]])

# Limits the grab sample data to the watershed of choice
#dfHydroGrab <- dfHydroGrab[dfHydroGrab$Sample.Name == varWatershed, ]
# Establishes a df that will only be used by the models to make predictions
dfModelData <- dfWatershed[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]

# This first loop predicts solute concentrations and appends the data to dfWatershed
for (i in 1:length(ListOfSolutes)) {
  PredictedSolute <- predict(get(ListOfModels[[i]]), newdata = dfModelData)
  eval(parse(text = paste("dfWatershed$Pred", ListOfSolutes[i],"<- PredictedSolute", sep = "")))
}


# This loop calculates the NSE values for each solute. For now it simply prints the results.
# In the future we'll update it to store them somewhere useful
for (i in ListOfSolutes) {
  TempNSE <- eval(parse(text = paste("vnse(dfWatershed$Pred", i ,", dfWatershed$", i , ")", sep = "")))
  print(paste('The NSE for', i, "is:", round(TempNSE, digits=2)))
}








data_test <- dfWatershed[ , c("DOC", "PredDOC")]
varNames <- colnames(data_test)
varNames[2]
lmPred2 = lm(PredDOC ~ DOC, data = data_test)
r_squared <- summary(lmPred2)$r.squared
ModelVar <- as.data.frame(coef(lmPred2))
Inter <- ModelVar[1,1]
Slope <- ModelVar[2,1]
summary(lmPred2)
fig<-plot_ly(data = data_test, x = ~DOC, y = ~PredDOC, type = 'scatter')
fig <- fig %>% layout(title = paste('All-But-One-Stream Model Predicting', varNames[1] ,'in W3'), 
                      yaxis = list(title = 'Predicted DOC (mg/L)'), 
                      xaxis = list(title = 'Grab Sample DOC (mg/L)'), 
                      annotations =list(
                        x = 2, y = 4,
                        #xref = "paper", yref = "paper",
                        text = paste(varNames[2], " = ", round(Slope, 3), "*", varNames[1], "+", round(Inter, 5), "\n", 
                                     "R² = ", round(r_squared, 3)),
                        showarrow = FALSE
                      ))
fig




plot(AB1_W3_Modeled_Ca)
importance(AB1_GOF_Modeled_DOC)
importance(Master_Modeled_DOC)
















  
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
