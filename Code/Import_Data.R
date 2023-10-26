library(tidyverse)

######################################
### Hubbard Brook Grab Sample Data ###
######################################

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/208/9/3b3cf7ea447cb875d7c7d68ebdfd24c7" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")


dt2 <-read.csv(infile2,header=F 
               ,skip=1
               ,sep=","  
               ,quot='"' 
               , col.names=c(
                 "site",     
                 "date",     
                 "timeEST",     
                 "barcode",     
                 "pH",     
                 "DIC",     
                 "spCond",     
                 "temp",     
                 "ANC960",     
                 "ANCMet",     
                 "gageHt",     
                 "hydroGraph",     
                 "flowGageHt",     
                 "fieldCode",     
                 "notes",     
                 "uniqueID",     
                 "waterYr",     
                 "Ca",     
                 "Mg",     
                 "K",     
                 "Na",     
                 "TMAl",     
                 "OMAl",     
                 "Al_ICP",     
                 "Al_ferron",     
                 "NH4",     
                 "SO4",     
                 "NO3",     
                 "Cl",     
                 "PO4",     
                 "DOC",     
                 "TDN",     
                 "DON",     
                 "SiO2",     
                 "Mn",     
                 "Fe",     
                 "F",     
                 "cationCharge",     
                 "anionCharge",     
                 "ionError",     
                 "duplicate",     
                 "sampleType",     
                 "ionBalance",     
                 "canonical",     
                 "pHmetrohm"    ), check.names=TRUE)

unlink(infile2)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt2$site)!="factor") dt2$site<- as.factor(dt2$site)                                   
# attempting to convert dt2$date dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2date<-as.Date(dt2$date,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2date) == length(tmp2date[!is.na(tmp2date)])){dt2$date <- tmp2date } else {print("Date conversion failed for dt2$date. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2date) 
if (class(dt2$barcode)!="factor") dt2$barcode<- as.factor(dt2$barcode)
if (class(dt2$pH)=="factor") dt2$pH <-as.numeric(levels(dt2$pH))[as.integer(dt2$pH) ]               
if (class(dt2$pH)=="character") dt2$pH <-as.numeric(dt2$pH)
if (class(dt2$DIC)=="factor") dt2$DIC <-as.numeric(levels(dt2$DIC))[as.integer(dt2$DIC) ]               
if (class(dt2$DIC)=="character") dt2$DIC <-as.numeric(dt2$DIC)
if (class(dt2$spCond)=="factor") dt2$spCond <-as.numeric(levels(dt2$spCond))[as.integer(dt2$spCond) ]               
if (class(dt2$spCond)=="character") dt2$spCond <-as.numeric(dt2$spCond)
if (class(dt2$temp)=="factor") dt2$temp <-as.numeric(levels(dt2$temp))[as.integer(dt2$temp) ]               
if (class(dt2$temp)=="character") dt2$temp <-as.numeric(dt2$temp)
if (class(dt2$ANC960)=="factor") dt2$ANC960 <-as.numeric(levels(dt2$ANC960))[as.integer(dt2$ANC960) ]               
if (class(dt2$ANC960)=="character") dt2$ANC960 <-as.numeric(dt2$ANC960)
if (class(dt2$ANCMet)=="factor") dt2$ANCMet <-as.numeric(levels(dt2$ANCMet))[as.integer(dt2$ANCMet) ]               
if (class(dt2$ANCMet)=="character") dt2$ANCMet <-as.numeric(dt2$ANCMet)
if (class(dt2$gageHt)=="factor") dt2$gageHt <-as.numeric(levels(dt2$gageHt))[as.integer(dt2$gageHt) ]               
if (class(dt2$gageHt)=="character") dt2$gageHt <-as.numeric(dt2$gageHt)
if (class(dt2$hydroGraph)!="factor") dt2$hydroGraph<- as.factor(dt2$hydroGraph)
if (class(dt2$flowGageHt)=="factor") dt2$flowGageHt <-as.numeric(levels(dt2$flowGageHt))[as.integer(dt2$flowGageHt) ]               
if (class(dt2$flowGageHt)=="character") dt2$flowGageHt <-as.numeric(dt2$flowGageHt)
if (class(dt2$fieldCode)!="factor") dt2$fieldCode<- as.factor(dt2$fieldCode)
if (class(dt2$notes)!="factor") dt2$notes<- as.factor(dt2$notes)
if (class(dt2$uniqueID)!="factor") dt2$uniqueID<- as.factor(dt2$uniqueID)
if (class(dt2$Ca)=="factor") dt2$Ca <-as.numeric(levels(dt2$Ca))[as.integer(dt2$Ca) ]               
if (class(dt2$Ca)=="character") dt2$Ca <-as.numeric(dt2$Ca)
if (class(dt2$Mg)=="factor") dt2$Mg <-as.numeric(levels(dt2$Mg))[as.integer(dt2$Mg) ]               
if (class(dt2$Mg)=="character") dt2$Mg <-as.numeric(dt2$Mg)
if (class(dt2$K)=="factor") dt2$K <-as.numeric(levels(dt2$K))[as.integer(dt2$K) ]               
if (class(dt2$K)=="character") dt2$K <-as.numeric(dt2$K)
if (class(dt2$Na)=="factor") dt2$Na <-as.numeric(levels(dt2$Na))[as.integer(dt2$Na) ]               
if (class(dt2$Na)=="character") dt2$Na <-as.numeric(dt2$Na)
if (class(dt2$TMAl)=="factor") dt2$TMAl <-as.numeric(levels(dt2$TMAl))[as.integer(dt2$TMAl) ]               
if (class(dt2$TMAl)=="character") dt2$TMAl <-as.numeric(dt2$TMAl)
if (class(dt2$OMAl)=="factor") dt2$OMAl <-as.numeric(levels(dt2$OMAl))[as.integer(dt2$OMAl) ]               
if (class(dt2$OMAl)=="character") dt2$OMAl <-as.numeric(dt2$OMAl)
if (class(dt2$Al_ICP)=="factor") dt2$Al_ICP <-as.numeric(levels(dt2$Al_ICP))[as.integer(dt2$Al_ICP) ]               
if (class(dt2$Al_ICP)=="character") dt2$Al_ICP <-as.numeric(dt2$Al_ICP)
if (class(dt2$Al_ferron)=="factor") dt2$Al_ferron <-as.numeric(levels(dt2$Al_ferron))[as.integer(dt2$Al_ferron) ]               
if (class(dt2$Al_ferron)=="character") dt2$Al_ferron <-as.numeric(dt2$Al_ferron)
if (class(dt2$NH4)=="factor") dt2$NH4 <-as.numeric(levels(dt2$NH4))[as.integer(dt2$NH4) ]               
if (class(dt2$NH4)=="character") dt2$NH4 <-as.numeric(dt2$NH4)
if (class(dt2$SO4)=="factor") dt2$SO4 <-as.numeric(levels(dt2$SO4))[as.integer(dt2$SO4) ]               
if (class(dt2$SO4)=="character") dt2$SO4 <-as.numeric(dt2$SO4)
if (class(dt2$NO3)=="factor") dt2$NO3 <-as.numeric(levels(dt2$NO3))[as.integer(dt2$NO3) ]               
if (class(dt2$NO3)=="character") dt2$NO3 <-as.numeric(dt2$NO3)
if (class(dt2$Cl)=="factor") dt2$Cl <-as.numeric(levels(dt2$Cl))[as.integer(dt2$Cl) ]               
if (class(dt2$Cl)=="character") dt2$Cl <-as.numeric(dt2$Cl)
if (class(dt2$PO4)=="factor") dt2$PO4 <-as.numeric(levels(dt2$PO4))[as.integer(dt2$PO4) ]               
if (class(dt2$PO4)=="character") dt2$PO4 <-as.numeric(dt2$PO4)
if (class(dt2$DOC)=="factor") dt2$DOC <-as.numeric(levels(dt2$DOC))[as.integer(dt2$DOC) ]               
if (class(dt2$DOC)=="character") dt2$DOC <-as.numeric(dt2$DOC)
if (class(dt2$TDN)=="factor") dt2$TDN <-as.numeric(levels(dt2$TDN))[as.integer(dt2$TDN) ]               
if (class(dt2$TDN)=="character") dt2$TDN <-as.numeric(dt2$TDN)
if (class(dt2$DON)=="factor") dt2$DON <-as.numeric(levels(dt2$DON))[as.integer(dt2$DON) ]               
if (class(dt2$DON)=="character") dt2$DON <-as.numeric(dt2$DON)
if (class(dt2$SiO2)=="factor") dt2$SiO2 <-as.numeric(levels(dt2$SiO2))[as.integer(dt2$SiO2) ]               
if (class(dt2$SiO2)=="character") dt2$SiO2 <-as.numeric(dt2$SiO2)
if (class(dt2$Mn)=="factor") dt2$Mn <-as.numeric(levels(dt2$Mn))[as.integer(dt2$Mn) ]               
if (class(dt2$Mn)=="character") dt2$Mn <-as.numeric(dt2$Mn)
if (class(dt2$Fe)=="factor") dt2$Fe <-as.numeric(levels(dt2$Fe))[as.integer(dt2$Fe) ]               
if (class(dt2$Fe)=="character") dt2$Fe <-as.numeric(dt2$Fe)
if (class(dt2$F)=="factor") dt2$F <-as.numeric(levels(dt2$F))[as.integer(dt2$F) ]               
if (class(dt2$F)=="character") dt2$F <-as.numeric(dt2$F)
if (class(dt2$cationCharge)=="factor") dt2$cationCharge <-as.numeric(levels(dt2$cationCharge))[as.integer(dt2$cationCharge) ]               
if (class(dt2$cationCharge)=="character") dt2$cationCharge <-as.numeric(dt2$cationCharge)
if (class(dt2$anionCharge)=="factor") dt2$anionCharge <-as.numeric(levels(dt2$anionCharge))[as.integer(dt2$anionCharge) ]               
if (class(dt2$anionCharge)=="character") dt2$anionCharge <-as.numeric(dt2$anionCharge)
if (class(dt2$ionError)=="factor") dt2$ionError <-as.numeric(levels(dt2$ionError))[as.integer(dt2$ionError) ]               
if (class(dt2$ionError)=="character") dt2$ionError <-as.numeric(dt2$ionError)
if (class(dt2$duplicate)!="factor") dt2$duplicate<- as.factor(dt2$duplicate)
if (class(dt2$sampleType)!="factor") dt2$sampleType<- as.factor(dt2$sampleType)
if (class(dt2$ionBalance)=="factor") dt2$ionBalance <-as.numeric(levels(dt2$ionBalance))[as.integer(dt2$ionBalance) ]               
if (class(dt2$ionBalance)=="character") dt2$ionBalance <-as.numeric(dt2$ionBalance)
if (class(dt2$canonical)!="factor") dt2$canonical<- as.factor(dt2$canonical)
if (class(dt2$pHmetrohm)!="factor") dt2$pHmetrohm<- as.factor(dt2$pHmetrohm)

# Convert Missing Values to NA for non-dates

dt2$site <- as.factor(ifelse((trimws(as.character(dt2$site))==trimws("NA")),NA,as.character(dt2$site)))
dt2$barcode <- as.factor(ifelse((trimws(as.character(dt2$barcode))==trimws("NA")),NA,as.character(dt2$barcode)))
dt2$pH <- ifelse((trimws(as.character(dt2$pH))==trimws("NA")),NA,dt2$pH)               
suppressWarnings(dt2$pH <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$pH))==as.character(as.numeric("NA"))),NA,dt2$pH))
dt2$DIC <- ifelse((trimws(as.character(dt2$DIC))==trimws("NA")),NA,dt2$DIC)               
suppressWarnings(dt2$DIC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DIC))==as.character(as.numeric("NA"))),NA,dt2$DIC))
dt2$spCond <- ifelse((trimws(as.character(dt2$spCond))==trimws("NA")),NA,dt2$spCond)               
suppressWarnings(dt2$spCond <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$spCond))==as.character(as.numeric("NA"))),NA,dt2$spCond))
dt2$temp <- ifelse((trimws(as.character(dt2$temp))==trimws("NA")),NA,dt2$temp)               
suppressWarnings(dt2$temp <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$temp))==as.character(as.numeric("NA"))),NA,dt2$temp))
dt2$ANC960 <- ifelse((trimws(as.character(dt2$ANC960))==trimws("NA")),NA,dt2$ANC960)               
suppressWarnings(dt2$ANC960 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANC960))==as.character(as.numeric("NA"))),NA,dt2$ANC960))
dt2$ANCMet <- ifelse((trimws(as.character(dt2$ANCMet))==trimws("NA")),NA,dt2$ANCMet)               
suppressWarnings(dt2$ANCMet <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ANCMet))==as.character(as.numeric("NA"))),NA,dt2$ANCMet))
dt2$gageHt <- ifelse((trimws(as.character(dt2$gageHt))==trimws("NA")),NA,dt2$gageHt)               
suppressWarnings(dt2$gageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$gageHt))==as.character(as.numeric("NA"))),NA,dt2$gageHt))
dt2$hydroGraph <- as.factor(ifelse((trimws(as.character(dt2$hydroGraph))==trimws("NA")),NA,as.character(dt2$hydroGraph)))
dt2$flowGageHt <- ifelse((trimws(as.character(dt2$flowGageHt))==trimws("NA")),NA,dt2$flowGageHt)               
suppressWarnings(dt2$flowGageHt <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$flowGageHt))==as.character(as.numeric("NA"))),NA,dt2$flowGageHt))
dt2$fieldCode <- as.factor(ifelse((trimws(as.character(dt2$fieldCode))==trimws("NA")),NA,as.character(dt2$fieldCode)))
dt2$notes <- as.factor(ifelse((trimws(as.character(dt2$notes))==trimws("NA")),NA,as.character(dt2$notes)))
dt2$uniqueID <- as.factor(ifelse((trimws(as.character(dt2$uniqueID))==trimws("NA")),NA,as.character(dt2$uniqueID)))
dt2$Ca <- ifelse((trimws(as.character(dt2$Ca))==trimws("NA")),NA,dt2$Ca)               
suppressWarnings(dt2$Ca <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Ca))==as.character(as.numeric("NA"))),NA,dt2$Ca))
dt2$Mg <- ifelse((trimws(as.character(dt2$Mg))==trimws("NA")),NA,dt2$Mg)               
suppressWarnings(dt2$Mg <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mg))==as.character(as.numeric("NA"))),NA,dt2$Mg))
dt2$K <- ifelse((trimws(as.character(dt2$K))==trimws("NA")),NA,dt2$K)               
suppressWarnings(dt2$K <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$K))==as.character(as.numeric("NA"))),NA,dt2$K))
dt2$Na <- ifelse((trimws(as.character(dt2$Na))==trimws("NA")),NA,dt2$Na)               
suppressWarnings(dt2$Na <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Na))==as.character(as.numeric("NA"))),NA,dt2$Na))
dt2$TMAl <- ifelse((trimws(as.character(dt2$TMAl))==trimws("NA")),NA,dt2$TMAl)               
suppressWarnings(dt2$TMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TMAl))==as.character(as.numeric("NA"))),NA,dt2$TMAl))
dt2$OMAl <- ifelse((trimws(as.character(dt2$OMAl))==trimws("NA")),NA,dt2$OMAl)               
suppressWarnings(dt2$OMAl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$OMAl))==as.character(as.numeric("NA"))),NA,dt2$OMAl))
dt2$Al_ICP <- ifelse((trimws(as.character(dt2$Al_ICP))==trimws("NA")),NA,dt2$Al_ICP)               
suppressWarnings(dt2$Al_ICP <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ICP))==as.character(as.numeric("NA"))),NA,dt2$Al_ICP))
dt2$Al_ferron <- ifelse((trimws(as.character(dt2$Al_ferron))==trimws("NA")),NA,dt2$Al_ferron)               
suppressWarnings(dt2$Al_ferron <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Al_ferron))==as.character(as.numeric("NA"))),NA,dt2$Al_ferron))
dt2$NH4 <- ifelse((trimws(as.character(dt2$NH4))==trimws("NA")),NA,dt2$NH4)               
suppressWarnings(dt2$NH4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NH4))==as.character(as.numeric("NA"))),NA,dt2$NH4))
dt2$SO4 <- ifelse((trimws(as.character(dt2$SO4))==trimws("NA")),NA,dt2$SO4)               
suppressWarnings(dt2$SO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SO4))==as.character(as.numeric("NA"))),NA,dt2$SO4))
dt2$NO3 <- ifelse((trimws(as.character(dt2$NO3))==trimws("NA")),NA,dt2$NO3)               
suppressWarnings(dt2$NO3 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$NO3))==as.character(as.numeric("NA"))),NA,dt2$NO3))
dt2$Cl <- ifelse((trimws(as.character(dt2$Cl))==trimws("NA")),NA,dt2$Cl)               
suppressWarnings(dt2$Cl <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Cl))==as.character(as.numeric("NA"))),NA,dt2$Cl))
dt2$PO4 <- ifelse((trimws(as.character(dt2$PO4))==trimws("NA")),NA,dt2$PO4)               
suppressWarnings(dt2$PO4 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$PO4))==as.character(as.numeric("NA"))),NA,dt2$PO4))
dt2$DOC <- ifelse((trimws(as.character(dt2$DOC))==trimws("NA")),NA,dt2$DOC)               
suppressWarnings(dt2$DOC <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DOC))==as.character(as.numeric("NA"))),NA,dt2$DOC))
dt2$TDN <- ifelse((trimws(as.character(dt2$TDN))==trimws("NA")),NA,dt2$TDN)               
suppressWarnings(dt2$TDN <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$TDN))==as.character(as.numeric("NA"))),NA,dt2$TDN))
dt2$DON <- ifelse((trimws(as.character(dt2$DON))==trimws("NA")),NA,dt2$DON)               
suppressWarnings(dt2$DON <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$DON))==as.character(as.numeric("NA"))),NA,dt2$DON))
dt2$SiO2 <- ifelse((trimws(as.character(dt2$SiO2))==trimws("NA")),NA,dt2$SiO2)               
suppressWarnings(dt2$SiO2 <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$SiO2))==as.character(as.numeric("NA"))),NA,dt2$SiO2))
dt2$Mn <- ifelse((trimws(as.character(dt2$Mn))==trimws("NA")),NA,dt2$Mn)               
suppressWarnings(dt2$Mn <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Mn))==as.character(as.numeric("NA"))),NA,dt2$Mn))
dt2$Fe <- ifelse((trimws(as.character(dt2$Fe))==trimws("NA")),NA,dt2$Fe)               
suppressWarnings(dt2$Fe <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$Fe))==as.character(as.numeric("NA"))),NA,dt2$Fe))
dt2$F <- ifelse((trimws(as.character(dt2$F))==trimws("NA")),NA,dt2$F)               
suppressWarnings(dt2$F <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$F))==as.character(as.numeric("NA"))),NA,dt2$F))
dt2$cationCharge <- ifelse((trimws(as.character(dt2$cationCharge))==trimws("NA")),NA,dt2$cationCharge)               
suppressWarnings(dt2$cationCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$cationCharge))==as.character(as.numeric("NA"))),NA,dt2$cationCharge))
dt2$anionCharge <- ifelse((trimws(as.character(dt2$anionCharge))==trimws("NA")),NA,dt2$anionCharge)               
suppressWarnings(dt2$anionCharge <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$anionCharge))==as.character(as.numeric("NA"))),NA,dt2$anionCharge))
dt2$ionError <- ifelse((trimws(as.character(dt2$ionError))==trimws("NA")),NA,dt2$ionError)               
suppressWarnings(dt2$ionError <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionError))==as.character(as.numeric("NA"))),NA,dt2$ionError))
dt2$duplicate <- as.factor(ifelse((trimws(as.character(dt2$duplicate))==trimws("NA")),NA,as.character(dt2$duplicate)))
dt2$sampleType <- as.factor(ifelse((trimws(as.character(dt2$sampleType))==trimws("NA")),NA,as.character(dt2$sampleType)))
dt2$ionBalance <- ifelse((trimws(as.character(dt2$ionBalance))==trimws("NA")),NA,dt2$ionBalance)               
suppressWarnings(dt2$ionBalance <- ifelse(!is.na(as.numeric("NA")) & (trimws(as.character(dt2$ionBalance))==as.character(as.numeric("NA"))),NA,dt2$ionBalance))
dt2$canonical <- as.factor(ifelse((trimws(as.character(dt2$canonical))==trimws("NA")),NA,as.character(dt2$canonical)))
dt2$pHmetrohm <- as.factor(ifelse((trimws(as.character(dt2$pHmetrohm))==trimws("NA")),NA,as.character(dt2$pHmetrohm)))

#######################################################################
### The snippet below is wrangle our data into a usable form for W3 ###
#######################################################################

dfGrab <- data.frame(dt2)
dfGrab <- dfGrab[dfGrab$site == 'W3' & dfGrab$waterYr >= 2013,]
dfGrab$DATETIME <- paste(dfGrab$date, dfGrab$timeEST, sep=" ")
dfGrab$DATETIME<-as.POSIXct(dfGrab$DATETIME, format = '%Y-%m-%d %H:%M')
dfGrab <- dfGrab[!(dfGrab$duplicate %in% "Dup"),]
dfGrab$BkpGrabDate <- dfGrab$DATETIME





###########################################################################
### Hubbard Brook W3 data. Note this excludes all other watershed data. ###
###########################################################################

inUrl12  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-hbr/1/15/8a9471ad299865102bbb934fb74fa05b" 
infile12 <- tempfile()
try(download.file(inUrl12,infile12,method="curl"))
if (is.na(file.size(infile12))) download.file(inUrl12,infile12,method="auto")


dt12 <-read.csv(infile12,header=F 
                ,skip=1
                ,sep=","  
                , col.names=c(
                  "WS",     
                  "DATETIME",     
                  "Discharge_ls",     
                  "Flag"    ), check.names=TRUE)

unlink(infile12)

# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings

if (class(dt12$WS)!="factor") dt12$WS<- as.factor(dt12$WS)                                   
# attempting to convert dt12$DATETIME dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp12DATETIME<-as.POSIXct(dt12$DATETIME,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp12DATETIME) == length(tmp12DATETIME[!is.na(tmp12DATETIME)])){dt12$DATETIME <- tmp12DATETIME } else {print("Date conversion failed for dt12$DATETIME. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp12DATETIME) 
if (class(dt12$Discharge_ls)=="factor") dt12$Discharge_ls <-as.numeric(levels(dt12$Discharge_ls))[as.integer(dt12$Discharge_ls) ]               
if (class(dt12$Discharge_ls)=="character") dt12$Discharge_ls <-as.numeric(dt12$Discharge_ls)
if (class(dt12$Flag)!="factor") dt12$Flag<- as.factor(dt12$Flag)

# Convert Missing Values to NA for non-dates



# Here is the structure of the input data frame:
str(dt12)                            
attach(dt12)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(WS)
summary(DATETIME)
summary(Discharge_ls)
summary(Flag) 
# Get more details on character variables

summary(as.factor(dt12$WS)) 
summary(as.factor(dt12$Flag))
detach(dt12)               

str(dt12)

dfDis <- data.frame(dt12)
dfDis$DATETIME<-as.POSIXct(dfDis$DATETIME, format = '%Y-%m-%d %H:%M:%S')
dfDis <- within(dfDis, Dis_L_15min <- Discharge_ls * 300)
#This is the one and only line you need to resample the data into daily data
DfDis15 <- dfDis %>% group_by(DATETIME=floor_date(DATETIME, '15 minutes')) %>% summarize(Flow15MinLiters = sum(Dis_L_15min))
DfDis15$BkpDisDate <- DfDis15$DATETIME










###################################################################################
### Each section below imports data for each watershed in New Hampshire         ###
### You can run them one at a time or all at once                               ###
### If you need to look for a different file then you can use the following URL ###
### https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/       ###
###################################################################################


###################################
### BDC Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BDC/BDC%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BDC/BDC%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BDC/BDC%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BDC/BDC%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BDC/BDC%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
BDC <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### BEF Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BEF/BEF%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BEF/BEF%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BEF/BEF%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BEF/BEF%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/BEF/BEF%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
BEF <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### DCF Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/DCF/DCF%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/DCF/DCF%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/DCF/DCF%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/DCF/DCF%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/DCF/DCF%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
DCF <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### GOF Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/GOF/GOF%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/GOF/GOF%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/GOF/GOF%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/GOF/GOF%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/GOF/GOF%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
GOF <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### HBF Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/HBF/HBF%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/HBF/HBF%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/HBF/HBF%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/HBF/HBF%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/HBF/HBF%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
HBF <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### LMP Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/LMP/LMP%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/LMP/LMP%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/LMP/LMP%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/LMP/LMP%20Nitrate.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/LMP/LMP%20Nitrate.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
LMP <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### MCQ Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/MCQ/MCQ%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/MCQ/MCQ%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/MCQ/MCQ%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/MCQ/MCQ%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/MCQ/MCQ%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
MCQ <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### SBM Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/SBM/SBM%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/SBM/SBM%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/SBM/SBM%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/SBM/SBM%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/SBM/SBM%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
SBM <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### TPB Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/TPB/TPB%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/TPB/TPB%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/TPB/TPB%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/TPB/TPB%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/TPB/TPB%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
TPB <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])


###################################
### WHB Watershed Data Download ###
###################################

TemporaryDis <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/WHB/WHB%20Discharge%20Storm%20ID.csv")
TemporaryfDOM <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/WHB/WHB%20fDOM.csv")
TemporaryNitrate <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/WHB/WHB%20Nitrate.csv")
TemporarySpecCond <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/WHB/WHB%20Spec%20Cond.csv")
TemporaryTemp <- read.csv("https://www.hydroshare.org/resource/8217eab0997d493782ff321ca5f95f28/data/contents/WHB/WHB%20Temperature.csv")

TemporaryList <- list(TemporaryDis, TemporaryfDOM, TemporaryNitrate, TemporarySpecCond, TemporaryTemp)
Temporarydf <- TemporaryList %>% reduce(full_join, by='Date.Time.EST')
Temporarydf$Date.Time.EST<-as.POSIXct(Temporarydf$Date.Time.EST, format = '%m/%d/%Y %H:%M')
drop <- c("Site", "Site.x", "Site.x.x","Site.y", "Site.y.y", "StormID", "Q_Ls")
WHB <- na.omit(Temporarydf[,!(names(Temporarydf) %in% drop)])

## Save:

save.image(file = 'C:/PhD/FOR797/FOR797_git/Processed_Data/Import_Data.Rdata')

# Load back in:

load('Processed_Data/Import_Data.Rdata')


