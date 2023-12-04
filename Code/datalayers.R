
####################### Load packages #######################

library(FedData)
library(sf)
library(climateR)
library(tidyverse)
library(mapview)
library(terra)
library(nhdR)

####################### Functions #######################

source("Code/Ryan_functions.R")

####################### package settings #######################

sf_use_s2(TRUE) # for sf

####################### variables #######################

meters_to_miles = 1/1609.334

# aggregate CDL: to do this:
# looking at the CDL legend (linkdata), I determined the following:

Ag<-c(1:6,10:14,21:39,41:61,66:72,74:77,204:214,216:227,229:250,254)
Pasture<-c(176)
Forest<-c(63,141:143)
Developed<-c(82,121:124)
Water<-c(83,111)
Wetlands_all<-c(87,190,195)
Other<-c(64,65,88,112,131,152) # Shrub<-c(64,152) Barren<-c()

l <- tibble::lst(Ag,Pasture,Forest,Developed,Water,Wetlands_all,Other)

reclass_CDL<-data.frame(lapply(l, `length<-`, max(lengths(l))))%>%
  pivot_longer(cols = everything(), values_to = 'MasterCat',names_to = 'Crop')%>%
  drop_na(MasterCat)

# adjust the NLCD reclassify legend to match the CDL reclassify df made above:

legend.NWIS<-legend
legend.NWIS$Class3[c(13,17)]<-'Pasture'
legend.NWIS$Class3[14]<-'Other';
legend.NWIS$Class3[1]<-'Water';
legend.NWIS$Class3[c(19,20)]<-'Wetlands_all'

sort(unique(legend.NWIS$Class3))==sort(unique(reclass_CDL$Crop))


####################### download watershed #######################

# read in station lat longs

df.sites<-read.csv("Raw_Data/stations.csv")

# convert to sf df:

df.sites<-st_as_sf(df.sites, coords = c("LONGITUDE", "LATITUDE"), crs = 4326, remove = FALSE)

# look at map of sites:

# mapview(df.sites, zcol = 'CODE')

# read in watershed shapefiles (!!):

df.ws<-st_read('Raw_Data/EPSCoR_wsheds6_gdb/EPSCoR_wsheds6.shp')

# look at map:

# mapview(df.sites, zcol = 'CODE')+mapview(df.ws, zcol = 'Site_Key')

# change three letter site key column in df.ws to match that of df.sites

dput(df.ws$Site_Key)

df.ws$Site_Key<-c("TPB", "BEF", "BDC", "HBF", "GOF", "DCF", "SBM", "WHB", 
  "LMP", "MCQ")

# convert to SpatVector:

vect.NH<-vect(df.ws)

# calculate DA using terra SpatVector:

vect.NH$area_KM2<-expanse(vect.NH, unit="km")

# add DA in km2 to df.ws:

df.ws$DA_sqkm_calculated<-expanse(vect.NH, unit="km")

# compare to DA listed in the paper:

# read in stations with DA (entered by hand from paper):

df.sites.DA<-read.csv('Raw_Data/stations_DA.csv')

# merge DA df with df.ws:

df.ws<-left_join(df.ws, df.sites.DA, by = c('Site_Key'='CODE'))

# add difference column:

df.ws<-mutate(df.ws, DA_error=(DA_sqkm-DA_sqkm_calculated)/DA_sqkm)%>%arrange(desc(abs(DA_error)))

# they look good!!

####################### Data Layers workflow #######################

# the dataframe dfHydroGrab in Processed_Data/Import_data.Rdata
# has the raw grab sample values and dates

load("Processed_Data/Import_Data.Rdata")

# remove NAs:

dfHydroGrab<-na.omit(dfHydroGrab)

# remove duplicate site+date combinations:

dfHydroGrab<-dfHydroGrab%>%mutate(DATETIME = as.Date(DATETIME))%>%
  distinct(Sample.Name, DATETIME, .keep_all = TRUE)

# remove sites not in df.ws:

dfHydroGrab<-dfHydroGrab%>%filter(Sample.Name %in% df.ws$Site_Key)

#### Land Use, NLCD ####

# download the 2016 NLCD for the watersheds into a list of Rasters:

l.NLCD.2016 <- lapply(seq_along(df.ws$Site_Key), \(i) get_nlcd(template = st_cast(df.ws, "MULTIPOLYGON")[i,], label = as.character(i), year = 2016))

# convert to SpatRasters:

l.NLCD.2016 <- lapply(l.NLCD.2016, rast)

# reproject to watershed vector data to match raster data crs:

vect.NH.proj<-terra::project(vect.NH, crs(l.NLCD.2016[[1]]))

# extract frequency tables for each sample watershed
# this returns a dataframe for each list element: (take a minute to run)

l.df.NLCD.2016 <- lapply(seq_along(l.NLCD.2016), \(i) terra::extract(l.NLCD.2016[[i]], vect.NH.proj[i], ID=FALSE)%>%group_by_at(1)%>%summarize(Freq=round(n()/nrow(.),2)))

# reclassify the NLCD using legend and clean up the dataframe from the next step

l.df.NLCD.2016<-lapply(l.df.NLCD.2016, \(i) left_join(as.data.frame(i), legend.NWIS%>%select(Class, Class3), by = 'Class')%>%mutate(Class = Class3)%>%select(-Class3))

l.df.NLCD.2016<-lapply(seq_along(l.df.NLCD.2016), \(i) l.df.NLCD.2016[[i]]%>%group_by(Class)%>%summarise(Freq = sum(Freq))%>%pivot_wider(names_from = Class, values_from = Freq)%>%mutate(Name = df.ws$Site_Key[i], .before = 1)%>%as.data.frame(.))

# bind list into single df:

df.NLCD<-bind_rows(l.df.NLCD.2016)

# Check row sums

rowSums(df.NLCD[,-1], na.rm = T)  # looks good

#### Elevation, NED ####

# download DEM. This will be a single raster for the entire study area, so watch out if this might be to big of an area:

DEM.NH<-get_ned(df.ws, label = '1') # already SpatRaster!

# extract elevation metrics over each sample watershed: to do this:
# build a function with multiple functions:

f <- function(x, na.rm = T) {
  c(mean=mean(x, na.rm = na.rm),
    range=max(x, na.rm = na.rm)-min(x, na.rm = na.rm),
    sd=sd(x, na.rm = na.rm)
  )
}

# reproject NH basins to DEM crs:

vect.NH.proj<-terra::project(vect.NH, crs(DEM.NH))

# extract the metrics over each watershed using the function above:

df.DEM.NH <- as.data.frame(terra::extract(DEM.NH, vect.NH.proj, f))

# set the names of the df:

names(df.DEM.NH)<-c('Name', 'Elev_Avg', 'Elev_Range', 'Elev_SD')

# set the names of the sites:

df.DEM.NH$Name<-df.ws$Site_Key

#### Climate (not run) ####

# the following climate variables are available for gridmet:

input_string<-'sph: (Near-Surface Specific Humidity)
vpd: (Mean Vapor Pressure Deficit)
pr: (Precipitation)
rmin: (Minimum Near-Surface Relative Humidity)
rmax: (Maximum Near-Surface Relative Humidity)
srad: (Surface Downwelling Solar Radiation)
tmmn: (Minimum Near-Surface Air Temperature)
tmmx: (Maximum Near-Surface Air Temperature)
vs: (Wind speed at 10 m)
th: (Wind direction at 10 m)
pdsi: (Palmer Drought Severity Index)
pet: (Reference grass evaportranspiration)
etr: (Reference alfalfa evaportranspiration)
erc: (model-G)
bi: (model-G)
fm100: (100-hour dead fuel moisture)
fm1000: (1000-hour dead fuel moisture)'

# Split the string by line breaks

lines <- strsplit(input_string, "\n")[[1]]

# Extract the text before each colon

result_vector <- as.character(sapply(lines, function(line) {
  parts <- strsplit(line, ":")[[1]]
  trimws(parts[1])
}))

# I want to download all these values for each watershed and date combination

i<-1
j<-1
k<-1

for (i in seq_along(result_vector)){
  for (j in seq_along(df.ws$Site_Key)){
    df.j<-dfHydroGrab%>%filter(Sample.Name == df.ws$Site_Key[j])
    for (k in seq_along(df.j$DATETIME)){
      rast.k<-getGridMET(vect.NH[j,], varname = result_vector[i], startDate = df.j$DATETIME[k]-21, endDate = df.j$DATETIME[k])[[1]]
    }
  }
}

plot(vect.NH[j,])
mapview(df.ws[df.ws$Site_Key=='TPB',])



# 1(24 hr),2,3,4,5,10,14 and 21 day cumulative rainfall prior to sample date
# 0,1(24 hr),2,3,4,5,10,14 and 21 day lag in tmax and tmin:
# 1(24 hr),2,3,4,5,10,14 and 21 day delta in tmax and tmin:



# create vectors of climate variables:

vars<-c('pr', 'tmmx', 'tmmn')

# create vectors of functions for group_by and summarize for each climate variables:

vars_funs<-c(`sum`, `max`, `min`)

# initalize dataframe:

temp_lags<-c(0,1,2,3,4,5,10,14,21); temp_lags_char<-as.character(temp_lags)

rain_cum_and_temp_deltas<-c(1,2,3,4,5,10,14,21); rain_cum_and_temp_deltas_char<-as.character(rain_cum_and_temp_deltas)

rain_colnames<-paste0('Rain_cummulative_', rain_cum_and_temp_deltas_char, '_day')

tmax_lag_colnames<-paste0('Tmax_lag_', temp_lags_char, '_day')

tmin_lag_colnames<-paste0('Tmin_lag_', temp_lags_char, '_day')

tmax_delta_colnames<-paste0('Tmax_delta_', rain_cum_and_temp_deltas, '_day')

tmin_delta_colnames<-paste0('Tmin_delta_', rain_cum_and_temp_deltas, '_day')

columnnames<-c('site_no', 'Date', rain_colnames, tmax_lag_colnames, tmax_delta_colnames, tmin_lag_colnames, tmin_delta_colnames)

df.Climate_features<-setNames(data.frame(matrix(ncol = length(columnnames), nrow = 1)), columnnames)

# loop through the climate variables:

# i<-2
# j<-1
# k<-1

# loop through the watersheds:


for (i in 1:dim(vect.NH)[1]){
  # save the samples with their dates for the watershed:
  df.C.j<-filter(dfHydroGrab, Sample.Name == vect.NH$Name[i])%>%drop_na(DATETIME)
  # # download the climate data for the full range of sample dates for this site:
  # rast.pr<-getGridMET(vect.NH[i,], varname = vars[1], startDate = as.character(as.Date(min(df.C.j$DATETIME))), endDate = as.character(as.Date(max(df.C.j$DATETIME))))[[1]]
  # rast.tmax<-getGridMET(vect.NH[i,], varname = vars[1], startDate = as.character(as.Date(min(df.C.j$DATETIME))), endDate = as.character(as.Date(max(df.C.j$DATETIME))))[[1]]
  # rast.tmin<-getGridMET(vect.NH[i,], varname = vars[1], startDate = as.character(as.Date(min(df.C.j$DATETIME))), endDate = as.character(as.Date(max(df.C.j$DATETIME))))[[1]]
  # 
  # loop through the sample dates for the watershed:
  for (j in seq_along(df.C.j$DATETIME)){
    # loop through the climate variables
    for (k in seq_along(vars)){
      # getGRIDMET only goes back as far as 1979-01-01 so if the sample date is before that omit:
      if (df.C.j$DATETIME[j] >= as.Date('1979-01-01')){
        # download:
        rast.climate.k<-getGridMET(vect.NH[i,], varname = vars[k], startDate = as.character(as.Date(df.C.j$DATETIME[j])-21), endDate = as.character(as.Date(df.C.j$DATETIME[j])))[[1]]
        # if the download doesnt work, omit: (when i=19, the download function returns and object of class 'glue'. no idea)
        if (class(rast.climate.k)[1]=="SpatRaster"){
          # reproject for extract:
          vect.NH.proj<-terra::project(vect.NH[i,], crs(rast.climate.k))
          # extract:
          df.climate.k <- terra::extract(rast.climate.k, vect.NH.proj, mean)
          # reformat to convert dates:
          df.climate.k.1<-df.climate.k%>%
            pivot_longer(cols= starts_with(paste0(vars[k],"_")), names_to = 'Date',values_to = "Value")%>%
            mutate(Date=as.Date(str_replace(Date, paste0(vars[k],"_"), "")))%>%
            arrange(desc(Date))
          # use logical statements to determine what to do this with info:
          if (k == 1){
            # create dataframe of the rain column names and theirvalues using cumsum and subseting vector using the vector of rain_cum_and_temp_deltas (prior to loo)
            df.rain<-data.frame(Name = rain_colnames, Value = round(cumsum(df.climate.k.1$Value)[rain_cum_and_temp_deltas],2))%>%
              pivot_wider(names_from = Name, values_from = Value)%>%
              mutate(site_no = vect.NH$Name[i], Date = as.Date(df.C.j$DATETIME[j]), .before = 1)
          }
          if (k == 2){
            df.tmax.lag<-data.frame(Name = tmax_lag_colnames, Value = df.climate.k.1$Value[temp_lags+1])%>%
              pivot_wider(names_from = Name, values_from = Value)%>%
              mutate(site_no = vect.NH$Name[i], Date = as.Date(df.C.j$DATETIME[j]), .before = 1)
            df.tmax.delta<-data.frame(Name = tmax_delta_colnames, Value = round(df.climate.k.1$Value[rain_cum_and_temp_deltas+1]-df.climate.k.1$Value[1],2))%>%
              pivot_wider(names_from = Name, values_from = Value)%>%
              mutate(site_no = vect.NH$Name[i], Date = as.Date(df.C.j$DATETIME[j]), .before = 1)
            df.tmax<-merge(df.tmax.lag, df.tmax.delta)
          }
          if (k == 3){
            df.tmin.lag<-data.frame(Name = tmin_lag_colnames, Value = df.climate.k.1$Value[temp_lags+1])%>%
              pivot_wider(names_from = Name, values_from = Value)%>%
              mutate(site_no = vect.NH$Name[i], Date = as.Date(df.C.j$DATETIME[j]), .before = 1)
            df.tmin.delta<-data.frame(Name = tmin_delta_colnames, Value = df.climate.k.1$Value[rain_cum_and_temp_deltas+1]-df.climate.k.1$Value[1])%>%
              pivot_wider(names_from = Name, values_from = Value)%>%
              mutate(site_no = vect.NH$Name[i], Date = as.Date(df.C.j$DATETIME[j]), .before = 1)
            df.tmin<-merge(df.tmin.lag, df.tmin.delta)
          }
        } 
        else {
          # print(vect.NH$Name[i])
          # print(df.C.j$DATETIME[j])
          # print(vars[k])
          # print('bad download')
          df.rain<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                  Rain_cummulative_1_day = NA_real_, Rain_cummulative_2_day = NA_real_, 
                                  Rain_cummulative_3_day = NA_real_, Rain_cummulative_4_day = NA_real_, 
                                  Rain_cummulative_5_day = NA_real_, Rain_cummulative_10_day = NA_real_, 
                                  Rain_cummulative_14_day = NA_real_, Rain_cummulative_21_day = NA_real_), row.names = c(NA, 
                                                                                                                         -1L), class = c("tbl_df", "tbl", "data.frame"))
          df.tmax<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                  Tmax_lag_0_day = NA_real_, Tmax_lag_1_day = NA_real_, Tmax_lag_2_day = NA_real_, 
                                  Tmax_lag_3_day = NA_real_, Tmax_lag_4_day = NA_real_, Tmax_lag_5_day = NA_real_, 
                                  Tmax_lag_10_day = NA_real_, Tmax_lag_14_day = NA_real_, Tmax_lag_21_day = NA_real_, 
                                  Tmax_delta_1_day = NA_real_, Tmax_delta_2_day = NA_real_, 
                                  Tmax_delta_3_day = NA_real_, Tmax_delta_4_day = NA_real_, 
                                  Tmax_delta_5_day = NA_real_, Tmax_delta_10_day = NA_real_, 
                                  Tmax_delta_14_day = NA_real_, Tmax_delta_21_day = NA_real_), row.names = c(NA, 
                                                                                                             -1L), class = "data.frame")
          df.tmin<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                  Tmin_lag_0_day = NA_real_, Tmin_lag_1_day = NA_real_, Tmin_lag_2_day = NA_real_, 
                                  Tmin_lag_3_day = NA_real_, Tmin_lag_4_day = NA_real_, Tmin_lag_5_day = NA_real_, 
                                  Tmin_lag_10_day = NA_real_, Tmin_lag_14_day = NA_real_, Tmin_lag_21_day = NA_real_, 
                                  Tmin_delta_1_day = NA_real_, Tmin_delta_2_day = NA_real_, 
                                  Tmin_delta_3_day = NA_real_, Tmin_delta_4_day = NA_real_, 
                                  Tmin_delta_5_day = NA_real_, Tmin_delta_10_day = NA_real_, 
                                  Tmin_delta_14_day = NA_real_, Tmin_delta_21_day = NA_real_), row.names = c(NA, 
                                                                                                             -1L), class = "data.frame")
        }
      }
      else{
        # print(vect.NH$Name[i])
        # print(df.C.j$DATETIME[j])
        # print(vars[k])
        # print('bad date')
        df.rain<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                Rain_cummulative_1_day = NA_real_, Rain_cummulative_2_day = NA_real_, 
                                Rain_cummulative_3_day = NA_real_, Rain_cummulative_4_day = NA_real_, 
                                Rain_cummulative_5_day = NA_real_, Rain_cummulative_10_day = NA_real_, 
                                Rain_cummulative_14_day = NA_real_, Rain_cummulative_21_day = NA_real_), row.names = c(NA, 
                                                                                                                       -1L), class = c("tbl_df", "tbl", "data.frame"))
        df.tmax<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                Tmax_lag_0_day = NA_real_, Tmax_lag_1_day = NA_real_, Tmax_lag_2_day = NA_real_, 
                                Tmax_lag_3_day = NA_real_, Tmax_lag_4_day = NA_real_, Tmax_lag_5_day = NA_real_, 
                                Tmax_lag_10_day = NA_real_, Tmax_lag_14_day = NA_real_, Tmax_lag_21_day = NA_real_, 
                                Tmax_delta_1_day = NA_real_, Tmax_delta_2_day = NA_real_, 
                                Tmax_delta_3_day = NA_real_, Tmax_delta_4_day = NA_real_, 
                                Tmax_delta_5_day = NA_real_, Tmax_delta_10_day = NA_real_, 
                                Tmax_delta_14_day = NA_real_, Tmax_delta_21_day = NA_real_), row.names = c(NA, 
                                                                                                           -1L), class = "data.frame")
        df.tmin<-structure(list(site_no = NA_character_, Date = structure(NA_real_, class = "Date"), 
                                Tmin_lag_0_day = NA_real_, Tmin_lag_1_day = NA_real_, Tmin_lag_2_day = NA_real_, 
                                Tmin_lag_3_day = NA_real_, Tmin_lag_4_day = NA_real_, Tmin_lag_5_day = NA_real_, 
                                Tmin_lag_10_day = NA_real_, Tmin_lag_14_day = NA_real_, Tmin_lag_21_day = NA_real_, 
                                Tmin_delta_1_day = NA_real_, Tmin_delta_2_day = NA_real_, 
                                Tmin_delta_3_day = NA_real_, Tmin_delta_4_day = NA_real_, 
                                Tmin_delta_5_day = NA_real_, Tmin_delta_10_day = NA_real_, 
                                Tmin_delta_14_day = NA_real_, Tmin_delta_21_day = NA_real_), row.names = c(NA, 
                                                                                                           -1L), class = "data.frame")
      }
    }
    # combine the three dataframes made in the k prior loop:
    df.ij.row<-merge(df.rain, df.tmax)%>%merge(.,df.tmin)
    # append to master df:
    df.Climate_features<-bind_rows(df.Climate_features, df.ij.row)
  }
}

# remove duplicate site+date combinations (should of removed duplicate dates before downloading climate data)

df.Climate_features<-df.Climate_features%>%distinct(site_no, Date, .keep_all = TRUE)

# remove first row:

df.Climate_features<-df.Climate_features[-1,]

# save df:

# save(df.Climate_features, file = 'Processed_Data/df.Climate_features.Rdata')
load('Processed_Data/df.Climate_features.Rdata')

#### Elevation: ####

# download NED for each watershed in sf df:

l.elevation<-lapply(seq_along(df.sf$Name), \(i) get_ned(df.sf[i,], label = paste(df.sf$Name[i], '2')))

# create function of functions for extracting elevation metrics:

f <- function(x, na.rm = T) {
  c(mean=mean(x, na.rm = na.rm),
    range=max(x, na.rm = na.rm)-min(x, na.rm = na.rm),
    sd=sd(x, na.rm = na.rm)
  )
}

# reproject NH basins to DEM crs:

vect.NH.proj<-terra::project(vect.NH, crs(l.elevation[[1]]))

# extract the metrics over each watershed using the function above:

l.elevation <- lapply(seq_along(df.sf$Name), \(i) as.data.frame(terra::extract(l.elevation[[i]], vect.NH.proj[i], f)))

# set the names of the columns for each dataframe:

l.elevation <- lapply(l.elevation, \(i) i%>%magrittr::set_colnames(c('Name', 'Elev_Avg', 'Elev_Range', 'Elev_SD')))

# remove first columns:

l.elevation<-lapply(l.elevation, \(i) i%>%select(-1))

# set names of list:

names(l.elevation)<-df.sf$Name

# create a df from list:

df.elevation<-bind_rows(l.elevation, .id = 'CODE')

# finally save the df:

# save(df.elevation, file= 'Processed_Data/df.elevation.Rdata')

load('Processed_Data/df.elevation.Rdata')

#### Land Use: ####

# download NLCD 2019:

l.NLCD <- lapply(seq_along(df.sf$Name), \(i) get_nlcd(template = st_cast(df.sf, "MULTIPOLYGON")[i,], label = paste(df.sf$Name[i], '5'), year = 2019))

# convert to SpatRaster (DEM was already downloaded as SpatRaster):

l.NLCD<-lapply(l.NLCD, rast)

# reproject to sample watershed vector data to match raster data:

vect.NH.proj<-terra::project(vect.NH, crs(l.NLCD[[1]]))

# extract frequency tables for each sample watershed

l.NLCD <- lapply(seq_along(l.NLCD), \(i) terra::extract(l.NLCD[[i]], vect.NH.proj[i], ID=FALSE)%>%group_by_at(1)%>%summarize(Freq=round(n()/nrow(.),2)))

# reclassify: to do this:

# adjust the NLCD reclassify legend to match the CDL reclassify df made above:

legend.NLCD<-legend
legend.NLCD$Class3<-legend.NLCD$Class2
legend.NLCD$Class3[c(13,17)]<-'Pasture'
legend.NLCD$Class3[14]<-'Other'
legend.NLCD$Class3[1]<-'Water'
legend.NLCD$Class3[c(19,20)]<-'Wetlands_all'

# reclassify the NLCD using this new legend and clean up the dataframe from the next step

l.NLCD<-lapply(l.NLCD, \(i) left_join(as.data.frame(i), legend.NLCD%>%select(Class, Class3), by = 'Class')%>%mutate(Class = Class3)%>%select(-Class3))

# pivot_wider the df in the lists and add a Name column for the site:

l.NLCD<-lapply(seq_along(l.NLCD), \(i) l.NLCD[[i]]%>%group_by(Class)%>%summarise(Freq = sum(Freq))%>%pivot_wider(names_from = Class, values_from = Freq)%>%mutate(Name = df.sf$Name[i], .before = 1)%>%as.data.frame(.))

# bind the lists into a single dataframe
# note some of the sites have different length dtaframes because they didnt have all the same number of NLCD classes. When binding rows this will give a dataframe of the maximum length and put NAs for sites where there wasn't a column: 

df.NLCD<-bind_rows(l.NLCD)%>%replace(is.na(.), 0) 

# look at rowsums:

x<-df.NLCD %>%mutate(sum = rowSums(across(where(is.numeric))))

# they look good

# save:

# save(df.NLCD, file= 'Processed_Data/df.NLCD.Rdata')
load('Processed_Data/df.elevation.Rdata')

#### Combine Climate, Elevation, and land use dfs ####

df.datalayers<-left_join(df.Climate_features, df.elevation, by = c('site_no'='CODE'))%>%left_join(., df.NLCD, by = c('site_no'='Name'))

# not that only the three sites thatworked in the climate workflow are in this final dataframe
# however there are three more sites in df.elevation and df.NLCD

# save:

# save(df.datalayers, file= 'Processed_Data/df.datalayers.Rdata')
load('Processed_Data/df.elevation.Rdata')

