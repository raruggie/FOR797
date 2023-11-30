library(dplyr, warn.conflicts = FALSE)
library(lubridate, warn.conflicts = FALSE)
library(tidyverse)
library(tidymodels)
library(data.table)
library(randomForest)
library(VSURF)
library(ie2misc)
library(plotly)
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


dfTraining <- dfMaster
dfTesting <- dfMaster

TrainX_Dumb <- dfTraining[ , c("FDOM_corrected_QSU", "NO3_corrected_mgL", "SpConductivity", "TempC", "DOYSin", "DOYCos")]

TrainY <- dfTraining[ , c("Cl", "NO3", "SO4", "Na", "K", 
                          "Mg", "Ca", "NH4", "DON", "PO4", "DOC")]

tempPath <- paste("Models/Master", sep = "")
if(!dir.exists(tempPath)){
  dir.create(tempPath)
}

tempParam <- paste("Models/MasterParams", sep = "")
if(!dir.exists(tempParam)){
  dir.create(tempParam)
}

for (i in colnames(TrainY)) {
  
  print(i)
  set.seed=500
  vsurfVar <- VSURF(TrainX_Dumb, TrainY[[i]], mtry = 100)
  SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.pred)
  parameterSelection <- vsurfVar$varselect.pred
  if (nrow(SelectedX) == 0) {
    SelectedX <- TrainX_Dumb %>% select(vsurfVar$varselect.interp)
    parameterSelection <- vsurfVar$varselect.interp
  }
  mod <- randomForest(SelectedX, TrainY[[i]])
  assign(paste("Master_Modeled_", i, sep = ""), mod)
  tempFileName <- paste(tempPath, "/Modeled_", i, ".Rdata", sep = "")
  save(list = paste("Master_Modeled_", i, sep = ""), file = tempFileName)
  
  assign(paste("Master_Params_", i, sep = ""), parameterSelection)
  tempParamFile <- paste(tempParam, "/Modeled_", i, "_Params.Rdata", sep = "")
  save(list = paste("Master_Params_", i, sep = ""), file = tempParamFile)
}

plot(Master_Modeled_Ca)
importance(Master_Modeled_Ca)


parameterSelection
vsurfVar$varselect.pred



ModelList <- list.files(path=paste("Models/Master", sep= ""), pattern=NULL, all.files=FALSE, 
                        full.names=FALSE)

for (i in ModelList) {
  load(paste("Models/Master/", i, sep = ""))
}



#WatershedVar <- "GOF"
#dfWatershed <- dfMaster[dfMaster$Watershed == WatershedVar,]
dfWatershed <- dfMaster
#varWatershed <- 'TPB'

################################################################################
### You shouldn't need to alter the code in the next section. You can scroll ###
### down to the next section to start plotting you data                      ###
################################################################################

# Creating lists for the for loop

ListOfSolutes <- list("Ca", "Cl", "DOC", "DON", "K", 
                      "Mg", "Na", "NH4", "NO3", "PO4", "SO4")
ListOfModels <- ls(pattern=paste("^Master_Mod", sep= ""))

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








data_test <- dfWatershed[ , c("DOC", "PredDOC", "Watershed")]
varNames <- colnames(data_test)
varNames[2]
lmPred2 = lm(PredDOC ~ DOC, data = data_test)
r_squared <- summary(lmPred2)$r.squared
ModelVar <- as.data.frame(coef(lmPred2))
Inter <- ModelVar[1,1]
Slope <- ModelVar[2,1]
summary(lmPred2)
#confidence.intervals<- confint(lmPred2, data_test$DOC, level=0.95)
fig<-plot_ly(data = data_test, x = ~DOC, y = ~PredDOC, type = 'scatter', mode = 'markers',
             color = ~Watershed, colors ="Dark2")
fig <- fig %>% layout(title = paste('Master Model Predicting', varNames[1] ,'in GOF'), 
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


library(ggplot2)
library(hrbrthemes)

p3 <- ggplot(data_test, aes(x=DOC, y=PredDOC)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE)

p3

hist(treesize(Master_Modeled_DOC))


