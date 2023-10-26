# Ryan Ruggiero
# FOR 797

####################### Load packages #######################

library(data.table)
library(randomForest)
library(lubridate)
library(tidyverse)

print('Hollis')

####################### Goal of code #######################

# WS3 in situ sensor data is used to predict sample Na concentrations

# Step 1: read in sensor and sample data and process: format dates, time series plots are examined, any other processing needed are completed
# Step 2: set up dataframe for RF model: pair up sample and sensor observations
# Step 3: build RF model and look at results

####################### Workflow #######################

####------------Step 1: Read in and Format Data------------####

# read in sensor, sample, and flow data:

Sensor<-read.csv("Raw_Data/HBF_WQual_Level4.csv")

Sample<-read.csv("Raw_Data/HubbardBrook_weekly_stream_chemistry.csv")

Flow<-read.csv("Raw_Data/w3_stmflow_2013-2022_5min.csv")

# convert dates:
# note for the sample dataframe, the DateTime column needs to be created first by pasting the date and time 

Sensor<-Sensor%>%mutate(DateTime = as.POSIXct(Date, format = '%m/%d/%Y %H:%M'))

Sample<-Sample%>%mutate(DateTime = as.POSIXct(paste(date, timeEST), format= '%Y-%m-%d %H:%M'), .before = 2)

Flow<-Flow%>%mutate(DateTime = as.POSIXct(DATETIME, format = '%Y-%m-%d %H:%M:%S'), .before = 2)%>%select(-3)

# filter samples for WS3:

Sample<-Sample%>%filter(site == 'W3')

# check to see if the dates are good by making time series plots:
# plotting the sensor data first will truncate the sample data to the sensor time period
# (commented out since takes a while):

# plot(Sensor$DateTime, as.numeric(Sensor$Nitrate_mg), xlab = 'Date', ylab = 'NO3_mg/L')
# points(Sample$DateTime, as.numeric(Sample$NO3)/4.2, col = 'red') # convert to N-NO3

# looks ok

# one think I a noticing is that the samples appear to be taken in the morning
# I want to look at the distribution of the time the samples were taken:

# to do this, strip the date element from the sample times, 
# and then to add an identical date (R will default to todays date) to all of the times:

# create dataframe of times on todays date:

df<-data.frame(time=as.POSIXct(strftime(Sample$DateTime, format="%H:%M"), format="%H:%M"))

# Extract Time

df$hour = hour(df$time) + minute(df$time)/60 + second(df$time)/3600

# Create Bins

bins=c(paste0(rep(c(paste0(0,0:9),10:23), each=4),".", c("00",25,50,75))[-1],"24:00")

# Divide Data Into Bins

df$bins = cut(df$hour, breaks=seq(0, 24, 0.25), labels=bins)

# Reformat to Numeric

df$bins <- as.numeric(as.character(df$bins))

# ggplot histogram

ggplot(df, aes(bins)) +
  geom_histogram()+
  xlab('Hour of day of Sample collection')

# Most of the samples are collected in the middle of the day
# this is not suprising, but may introduce some bias into our
# model if constituents have diurnal fluxuations, we should account for this
# in how the RF model is built/which training data we use?
# For example, maybe we want to train the RF model on a more uniform 
# distribution of times sampled? Or maybe we want to include a 
# time parameter in the RF model?

####------------Step 2: Pair Up Sensor, Sample, and Flow data (build predictor/predictand dataframe)------------####

#### Pair up Sensor and Sample first: Extract the sensor NO3 observation closest in time to the samples (same code as in WS3_sensor.R)

# filter the sample data to the start and end of the sensor data, 

Sample.1<-Sample%>%
  filter(DateTime>=min(Sensor$DateTime, na.rm=T))%>%
  filter(DateTime<=max(Sensor$DateTime, na.rm = T))

# pair up the sensor and sample by closest times:
# I found the solution here https://stackoverflow.com/questions/58609461/matching-values-by-nearest-posixct-in-two-data-frames-keeping-both-dates
# I don't understand the data table part

DF1<-Sample.1%>%select(c(2))%>%rename(date1 = 1)%>%mutate(Num1 = 1)
DF2<-Sensor%>%select(DateTime)%>%rename(date2 = 1)%>%mutate(Num2 = 2)

setDT(DF1)
setDT(DF2)

DF1[, c("NearestNum2", "NearestDate2") := 
      DF2[.SD, on = .(date2 = date1), .(x.Num2, x.date2), roll = "nearest"]
][,  TimeDiff := as.numeric(difftime(date1, NearestDate2))]

# now reformat this dataframe (for merging next)
# there is also an issue with duplicated times that may mess with the merge in the next step
# looking at where they came from, there are duplicated times in the sample dataframe, 
# where one of the duplicates will have missing data in certain constituent columns
# this can be fixed in the next step before merging the dataframes
# but removing duplicated datetimes here as well using distinct:
# also convert to a dataframe (because I dont like working with datatables)

df.RF<-DF1%>%
  as.data.frame()%>%
  select(c(1,4,5))%>%
  rename(Sample_Date = 1, Sensor_Date = 2)%>%
  distinct(Sample_Date, .keep_all = T)

# This looks like it worked, but with gaps in the sensor data there maybe paired sample-sensor observations that are far apart. 
# These paired observations should be removed
# to do so, filter rows based on a condition where the times are within 1 hour (the sensor data resolution is 15 minutes, so a 30 minute Margin of error should capture the sample observations, but using 1 hour here as to increase the MoE a little, and assuming we are ok with an hour difference in sample and sensor)
# the Timediff column already has this info! the units are minutes, so absolute value of 60:

df.RF<-df.RF%>%
  filter(abs(TimeDiff)<=60)

# before merging the sample and senor columns to this df,
# the duplicate rows in the sample data need to be combined into a single row:
# found the solution here: https://stackoverflow.com/questions/74535332/how-to-merge-rows-with-duplicate-id-replacing-nas-with-data-in-the-other-row-a

Sample.1<-Sample.1%>% 
  group_by(DateTime) %>% 
  summarise(across(everything(), ~ ifelse(any(complete.cases(.x)),first(.x[!is.na(.x)]),NA))) # insane verbage

# now merge the sample columns to this df:

df.RF<-left_join(df.RF, Sample.1, by = c('Sample_Date' = 'DateTime'))

# and merge the sensor columns to this df
# (also rename the TimeDiff column to reflect it is for the sensor-sample time difference, since next the flow timedifferences will be added):

df.RF<-left_join(df.RF, Sensor, by = c('Sensor_Date'='DateTime'))%>%
  rename(SenSamTimeDiff = TimeDiff)

# looking at this resulting df, the only duplicated columns looks like ph and DON (e.g. we get ph.x (Sample) and ph.y (Sensor) columns)
# I also checked to make sure everything lines up and it looks good

#### Pair up Flow datat: Extract the flow observation closest in time to the samples (similar code as above):

# same steps as above:

DF1<-df.RF%>%
  select(c(1))%>% # select datetime column
  rename(date1 = 1)%>% # renameto match the stackoverflow DT code below
  mutate(Num1 = 1) # add a column to ID the sample datetimes

DF2<-Flow%>%
  select(DateTime)%>%
  rename(date2 = 1)%>%
  mutate(Num2 = 2)

setDT(DF1)
setDT(DF2)

DF1[, c("NearestNum2", "NearestDate2") := 
      DF2[.SD, on = .(date2 = date1), .(x.Num2, x.date2), roll = "nearest"]
][,  TimeDiff := as.numeric(difftime(date1, NearestDate2))]

# it looks like other than the first few samples, the sample and flows line up exactly!
# which means that I can just use a left_join on df.RF to add the flow data to it!!

df.RF<-left_join(df.RF, Flow[,c(2,3)], by = c('Sample_Date'='DateTime'))

####------------Step 3: Build RF models------------####

# I was assigned Na but I want to build the models for all constituents

# ?randomForest

# lists will hold the different RF models

# create a df of the constituents that will be predicted:

df.predictors<-df.RF[,c(21:24,27,29:40)]

# create a df of the sensor predictands:
# note that the date column is kept here (see next step)

df.predictands<-df.RF[,c(1,57:61,63,64,66,100)] # nitrate to ODO (chose MGL instead of %), turbidityFNU, FDOMRFU, and flow

# create a list of dataframes for each constituents predictor and predictand dataset
# use lapply to loop through the columns of the dataframe and then add the columns of the predictand dataset
# this sets the name ofthe first column (which is the predict column) to `...1` in each dataframe (idk why)
# this can be changed using rename in a dplyr pipe in the same lapply:
# a second list is made that keeps the date column. This list will be used later when plotting:

l.RF_df<-lapply(df.predictors,\(i) bind_cols(i,df.predictands[,-1])%>%rename('Consit'=1)) # the [,-1] on the dataframe is getting rid of the datetime column. Dont need this in the dataframes used in the RF function call

l.RF_df_plot<-lapply(df.predictors,\(i) bind_cols(i,df.predictands)%>%rename('Consit'=1))

# now run RF for each dataframe (constituent) in the list:

l.RF<-lapply(l.RF_df, \(i) randomForest(Consit~., data = i, na.action=na.omit))

####------------Step 4: Look at RF Results------------####

# the number of predicted observations in the rf objects in the list
# are limted to the number of complete observations across the entire input dataframe,
# i.e. predictors and predictands.

# but the rf model objects contain the predictands
# so it is easy to extract the predicted and predictand and then plot

# but before this, the dates of the observations need to be added back
# the extra list create above is used for this:

# the dates of those used in the RF model (since na's were omitted) can be accessed using complete.cases funciton
# and select is used to just return the dates and the sample concentrations

l.Sample_Dates<-lapply(l.RF_df_plot, \(i) i[complete.cases(i),]%>%select(Sample_Date, Consit))

# now these dataframes can be merged with dataframes of the observed and predicted, which are extracted using lapply first:
# lapply can be used to return a dataframe of predicted and predictands for each constiuent:

l.Obs_Pred<-lapply(l.RF, \(i) data.frame(Observed = as.numeric(i$y), Predicted = as.numeric(i$predicted)))

# bind_cols of the two lists of df to get a single list of dfs with 4 columns for each df:

l.Obs_Pred<-lapply(seq_along(l.Obs_Pred), \(i) bind_cols(l.Sample_Dates[[i]], l.Obs_Pred[[i]]))%>%purrr::set_names(names(l.Sample_Dates))

# I checked by hand a few dataframes and observations to make sure that the dates lined up

# now bind_cols to get the list of dfs into asingle df
# and pivoting longer to get the dataframe ready for ggplot

RF_plot<-bind_rows(l.Obs_Pred, .id = 'Consit')%>%
  pivot_longer(cols = c(3,4), names_to = 'Type', values_to = 'Values')

# now use ggplot to create a facet plot for each conist:

ggplot(RF_plot, aes(x = Sample_Date, y = Values, color = Type))+
  geom_point()+
  facet_wrap('Consit',scales = 'free')

# just looking at the consituents assigned in class:

class_consit<-c('Al_ICP','DOC','PO4','Ca','Mg','Na','SO4')

# class_consit<-c('Ca')

RF_plot%>%filter(Consit %in% class_consit)%>%
  ggplot(., aes(x = Sample_Date, y = Values, color = Type))+
    geom_point()+
    facet_wrap('Consit',scales = 'free')

# add the model Rsquared to each plots title:

# create a dataframe of the rsquared values (extracted from the RF model objects) that can be joined with the plotting dataframe
# extract just the class conists from the over all list:

pvar<-l.RF[class_consit]

# extract the mean % of variation explained (mean over the 500 model runs) and convert to a df ready to merge to RF_plot:

pvar<-bind_rows(lapply(pvar, \(i) mean(i$rsq)))%>%
  t()%>%
  as.data.frame()%>%
  mutate(Consit = rownames(.), .before = 1)%>%
  rename(Rsq = 2)%>%
  mutate(Consit_Rsq=paste(Consit, round(Rsq,3)))%>%
  select(-2)

rownames(pvar)<-1:nrow(pvar)

# now replot but with a join with pvar:

RF_plot%>%filter(Consit %in% class_consit)%>%
  left_join(., pvar, by = 'Consit')%>%
  ggplot(., aes(x = Sample_Date, y = Values, color = Type))+
  geom_point()+
  facet_wrap('Consit_Rsq',scales = 'free')

# save workspace

save.image(file = 'C:/PhD/FOR797/FOR797_git/Processed_Data/WS3_RF_predict.Rdata')










