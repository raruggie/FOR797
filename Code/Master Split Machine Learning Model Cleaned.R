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
library(splitTools)
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

#Watersheds <- list("BDC")#, "BEF", "DCF", "GOF", "HBF", 
                         #"LMP", "MCQ", "SBM", "TPB", "WHB", "W1", "W3", "W9")
Watersheds <- list("LMP")

#dfTraining <- dfMaster[dfMaster$Ca != Watersheds[[j]],]
dfTraining <- dfMaster

folds <- create_folds(Training_LMP$Ca, k = 5, seed = 2734, m_rep = 1)
#eval(parse(text = paste("dfWatershed$Pred", ListOfSolutes[i],"<- PredictedSolute", sep = "")))
cv_VNSE <- numeric(15)

VNSERepository <- setNames(data.frame(matrix(ncol = 11, nrow = 15)), c("Cl", "NO3", "SO4", "Na", "K", 
                                                                              "Mg", "Ca", "NH4", "DON", "PO4", "DOC"))
rowNum <- 1
for (j in seq_along(folds)) {
  print(j)
  
  
  #insample <- Training_LMP[folds[[j]], ]
  #out <- Training_LMP[-folds[[j]], ]
  insample <- Training_LMP[-folds[[j]], ]
  out <- Training_LMP[folds[[j]], ]
  
  dfTraining <- dfMaster[dfMaster$Watershed != "LMP"]
  insample <- do.call("rbind", list(dfTraining, insample))
  
  TrainX_Dumb <- insample[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
  TrainY <- insample[ , c("Cl", "NO3", "SO4")]#, "Na", "K", 
                          #"Mg", "Ca", "NH4", "DON", "PO4", "DOC")]
  
  #VNSERepository <- setNames(data.frame(matrix(ncol = 11, nrow = 15)), colnames(TrainY))
  
  colNum <- 1
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
    assign(paste("AB1_LMP_Modeled_", i, sep = ""), mod)
    #tempFileName <- paste(tempPath, "/Modeled_", i, ".Rdata", sep = "")
    #save(list = paste("AB1_GOF_Modeled_", i, sep = ""), file = tempFileName)
    
    #cv_VNSE[j] <- vnse(out$Ca, predict(mod, out))
    #eval(parse(text = paste("VNSERepository[", rowNum, colNum, "] <- vnse(out$Ca, predict(mod, out))", sep = "")))
    
    eval(parse(text = paste("VNSERepository[", rowNum, ",", colNum, "] <- vnse(out$", i,", predict(mod, out))", sep = "")))
    colNum <- colNum + 1
  }
  
  
  #fit1 <- randomForest(TrainX_Dumb, TrainY)
  
  #cv_VNSE[j] <- vnse(out$Ca, predict(mod, out))
  #print(cv_VNSE)
  rowNum <- rowNum + 1
  print(VNSERepository)
}

out$NO3
out[1,NO3]
VNSERepository
VNSERepository$NO3
VNSERepository[1,1]
VNSERepository[rowNum, colNum]
print(VNSERepository[rowNum, colNum])
vnse(out$SO4, predict(mod, out))
VNSERepository[rowNum, colNum] <- vnse(out$SO4, predict(mod, out))

paste("VNSERepository[", rowNum, colNum, "] <- vnse(out$", i,", predict(mod, out))", sep = "")


eval(parse(text = paste("VNSERepository[1,", i, "] <- vnse(out$Ca, predict(mod, out))", sep = "")))

eval(parse(text = paste("VNSERepository[", rowNum, colNum, "] <- vnse(out$Ca, predict(mod, out))", sep = "")))





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
