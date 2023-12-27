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

#################################################
### This First section is just importing data ###
#################################################

load('Processed_Data/Master_Import_Data.Rdata')
paramList <- list.files('Models/MasterParams')
for (o in paramList) {
  print(o)
  paramFile <- paste('Models/MasterParams/', o, sep='')
  load(paramFile)
}

####################################################
### The next section deals with making the model ###
####################################################

#Watersheds <- list("BDC")#, "BEF", "DCF", "GOF", "HBF", 
#"LMP", "MCQ", "SBM", "TPB", "WHB", "W1", "W3", "W9")
Watersheds <- list("W9")

dfTraining <- Training_MCQ

YSolutes <- list("Cl", "NO3", "SO4", "Na", "K", 
"Mg", "Ca", "NH4", "DON", "PO4", "DOC")


knum <- 5
kreps <- 6
seq_along(1:(knum*kreps))
#YSolutes[[1]]
set.seed=500

VNSERepository <- setNames(data.frame(matrix(ncol = 11, nrow = (knum*kreps))), YSolutes)



for (i in seq_along(1:length(YSolutes))) {
  
  eval(parse(text = paste("folds <- create_folds(dfTraining$", YSolutes[[i]], ", k = knum, seed = 2734, m_rep = kreps, type = 'stratified')",sep = "")))
  
  print(YSolutes[[i]])
  rowNum <- 1
  for (j in seq_along(folds)) {
    print(j)
  
    insample <- dfTraining[folds[[j]], ]
    out <- dfTraining[-folds[[j]], ]
    #out <- Training_BDC
    
    #insample <- dfTraining[folds[[j]], ]
    #out <- dfTraining[-folds[[j]], ]
    
    
    #dfTrainingTemp <- dfMaster[dfMaster$Watershed != "TPB"]
    #insample <- do.call("rbind", list(dfTrainingTemp, insample))
    #insample <- Training_DCF
    #insample <- dfTraining
    
    TrainX_Dumb <- insample[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]
    TrainY <- insample[ , c("Cl", "NO3", "SO4", "Na", "K", 
    "Mg", "Ca", "NH4", "DON", "PO4", "DOC")]
    
    #print(i)
    set.seed=500
    #vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
    #SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
    #if (nrow(SelectedX) == 0) {
    #  SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
    #}
    #mod <- randomForest(SelectedX, TrainY[[i]])
    
    
    
    
    
    eval(parse(text = paste("SelectedX <- TrainX_Dumb %>% select(all_of(Master_Params_", YSolutes[[i]], "))", sep="")))
    mod <- randomForest(SelectedX, TrainY[[i]])
  
    eval(parse(text = paste("VNSERepository[", rowNum, ",", i, "] <- vnse(out$", YSolutes[[i]],", predict(mod, out))", sep = "")))
    
    rowNum <- rowNum + 1
  }
  print(VNSERepository)
}

summary(VNSERepository)
#sapply(mtcars, function(x) c(summary(x), type = class(x)))
SumStats <- sapply(VNSERepository, function(x) c(summary(x), type = class(x)))

#write.csv(SumStats, file = 'Models/ProperTrainAndTestSplits/MCQ8020.csv')
#write.csv(SumStats, file = 'Models/MasterKFoldTests/MasterAndBDC.csv')
