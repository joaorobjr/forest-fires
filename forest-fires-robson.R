library(tidyverse)
library(funModeling)
library(measurements)
library(lubridate)
library(ggplot2)
library('rnoaa')
library(lubridate)
library(devtools)
options(noaakey = "mqEuOSuAUjyuGlTjVjxxCpzRlbrooRnr")

#Load raw data --------------------------------------------------
fires.raw = as_tibble(read.csv("data/fires2015_train.csv",
                               stringsAsFactors = FALSE, 
                               na.strings = c("-", "","NA"), 
                               encoding = "UTF-8"))

#fires.test.raw <- read.csv("data/fires2015_test.csv", stringsAsFactors = FALSE, na.strings = c("-", "","NA"), encoding = "UTF-8")

glimpse(fires.raw)
df_status(fires.raw)
head(fires.raw)
tail(fires.raw)

# DATA PREPARATION -------------------------------------------------------

# Data Cleaning: Identifying and correcting mistakes or errors in the data.

# Check if there are duplicated observations
anyDuplicated(fires.raw)


# Variables: Latitude e Longitude -----------------------------------------------------

# Fixing incorrect value in variable lat

# Get index from observations with wrong value in lat variable
idx.lat.wrong = which(str_detect(fires.raw$lat, '1900-01-01'))
lat.wrong = fires.raw[idx.lat.wrong,]
head(lat.wrong)

# Number of observation with wrong value in lat variable
print(str_c("There are",length(idx.lat.wrong),"observations with wrong value '1900-01-01'", sep = " "))

# Value imputation based on another observation thar has same: region, district, municipality, parish. 
for (idx in idx.lat.wrong) {
  df_temp = fires.raw[-idx.lat.wrong,] %>% 
    filter(region == fires.raw[idx,]$region, 
           district == fires.raw[idx,]$district,
           municipality ==fires.raw[idx,]$municipality,
           parish == fires.raw[idx,]$parish)
  fires.raw$lat[idx] = df_temp$lat[1]
  fires.raw$lon[idx] = df_temp$lon[1]
}
rm(df_temp)
# After the imputation 8 NA values were assign to observations

# Cleaning lat and lon variables and convert from GPS coodinate to decimals
vec_clean <- c("''"="", "E-12"="", "E-11"="", "E-02"="", ","=".", "º"=" ", "'"=" ", ":"=" ")
fires.raw$lat <- conv_unit(str_replace_all(str_trim(fires.raw$lat), vec_clean), "deg_min_sec", "dec_deg")
# Ocorre um warning ao executar essa linha
fires.raw$lon <- conv_unit(str_replace_all(str_trim(fires.raw$lon, side = "both"), vec_clean), "deg_min_sec", "dec_deg")
# Workaround
fires.raw$lon[7511] <- conv_unit("8 34 21.5868000000013", "deg_min_sec", "dec_deg")

# Data imputation: firstInterv_date and firstInterv_hour
# firstInterv_date = (if is.na(firstInterv_date) and !is.na(extintion_date) then extintion_date else firstInterv_date )
# firstInterv_hour = (if is.na(firstInterv_hour) and !is.na(extintion_hour) then extintion_hour else firstInterv_hour )
fires.raw <- fires.raw %>% rowwise() %>% mutate(firstInterv_date = if_else( is.na(firstInterv_date) && !is.na(extinction_date), extinction_date , firstInterv_date),
                                                firstInterv_hour = if_else( is.na(firstInterv_hour) && !is.na(extinction_hour), extinction_hour , firstInterv_hour))

# Fix data type 
fires.raw$region = as.factor(fires.raw$region)
fires.raw$district = as.factor(fires.raw$district)
fires.raw$municipality = as.factor(fires.raw$municipality)
fires.raw$parish = as.factor(fires.raw$parish)
fires.raw$origin = as.factor(fires.raw$origin)
fires.raw$cause_type = as.factor(fires.raw$cause_type)

# Creating new features

# Variable alert 
fires.raw <- fires.raw %>% rowwise() %>% 
  mutate(alert = ymd_hms(str_c(alert_date, alert_hour, sep=" ")),
         extinction = ymd_hms(str_c(extinction_date, extinction_hour, sep=" ")),
         firstInterv = ymd_hms(str_c(firstInterv_date, firstInterv_hour, sep=" ")),
         latency_alert_interv = (firstInterv - alert)/60,
         latency_interv_ext = (extinction - firstInterv)/60,
         latency_alert_ext = (extinction - alert)/60)

fires.raw <- as_tibble(fires.raw)
df_status(fires.raw)

save(fires.raw, file = "fires.raw.RData")
load("fires.raw.RData")

#bar plot of forests fires during 2015
ggplot(fires.raw, aes(x = month(alert, label = T))) + 
  geom_bar(fill = "red") + 
  ggtitle("Distribution of forests fires across 2015") +
  labs(x="Months", y="Total")


# CREATE WEATHER HISTORICAL DATASET ----------------------------------------------

load("data/station_data.Rdata")

get_nearby_stations = function (df, measure){
  nearby_stations = meteo_nearby_stations(lat_lon_df = df,
                                          station_data = station_data, 
                                          radius = 1000, 
                                          var = measure,
                                          year_min = 2015, year_max = 2015,
                                          limit = 10)
  return(nearby_stations)
}

get_weather_data = function (station, measure) {
  wd <- ghcnd_search(station, var = measure, date_min = "2015-01-01", date_max = "2015-12-31")
  return(wd)
}

# Create dataframe with all distinct parish, lat and lon. 
# After Rename columns to names accepted by meteo_nearby_stations method
df_parish = distinct(fires.raw, parish, lat, lon) %>% 
  rename(id = parish, latitude = lat, longitude = lon)

weather_measures = c("TAVG", "TMAX", "TMIN", "PRCP")

stationsByParish = get_nearby_stations(df_parish, weather_measures)

weather_data = c()

for (parish in names(stationsByParish)) {
  station = eval(parse(text=sprintf("stationsByParish$'%s'$id[1]", parish)))
  wd = get_weather_data(station, weather_measures)
  weather_data = append(weather_data, eval(parse(text=sprintf("list('%s'= wd)", parish))))
}


#save(weather_data, file = "weather_data.RData")
load("weather_data.RData")


# MERGE WEATHER DATA AND FOREST FIRES -----------------------------------------

# Create new attributes: tavg, tmax, tmin, prcp
fires.raw = fires.raw %>% mutate(tavg = 0, tmax = 0, tmin = 0, prcp = 0)

for (parish_name in names(weather_data[1:1])) {
  
  arr_alert_dates = fires.raw %>% filter(parish == parish_name) %>% select(alert_date)
  
  for (dt in arr_alert_dates$alert_date) {
    
    wdTAVG = eval(parse(text=sprintf("filter(weather_data$'%s'$tavg, date == '%s')", parish_name, dt)))
    wdTMAX = eval(parse(text=sprintf("filter(weather_data$'%s'$tmax, date == '%s')", parish_name, dt)))
    wdTMIN = eval(parse(text=sprintf("filter(weather_data$'%s'$tmin, date == '%s')", parish_name, dt)))
    wdPRCP = eval(parse(text=sprintf("filter(weather_data$'%s'$prcp, date == '%s')", parish_name, dt)))
    
    idx = which(fires.raw$parish == parish_name & fires.raw$alert_date == dt)
    
    if(nrows(wdTAVG)>0){
      fires.raw[idx,]$tavg = wdTAVG$tavg/10
    }
    if(nrows(wdTMAX)>0){
      fires.raw[idx,]$tmax = wdTMAX$tmax/10
    }
    if(nrows(wdTMIN)>0){
      fires.raw[idx,]$tmin = wdTMIN$tmin/10
    }
    if(nrows(wdPRCP)>0){
      fires.raw[idx,]$prcp = wdPRCP$prcp/10
    }
  }
}




parish_name = "A dos Cunhados"
dt = "2015-08-05"
wd.tavg = eval(parse(text=sprintf("filter(weather_data$'%s'$tavg, date == '%s')", parish_name, dt)))
wd.tmax = eval(parse(text=sprintf("filter(weather_data$'%s'$tmax, date == '%s')", parish_name, dt)))
wd.tmin = eval(parse(text=sprintf("filter(weather_data$'%s'$tmin, date == '%s')", parish_name, dt)))
wd.prcp = eval(parse(text=sprintf("filter(weather_data$'%s'$prcp, date == '%s')", parish_name, dt)))

dim(wd.tavg)[1] > 0
dim(wd.tmax)[1] > 0


idx = which(fires.raw$parish == parish_name)
fires.parish = fires.raw[idx,]
alert_dates = fires.raw[idx,"alert_date"]
idx2 = which(fires.raw$parish == parish_name && as.Date(fires.raw$alert_date) %in% as.Date("2015-05-17"))

which(fires.raw$parish == parish_name & fires.raw$alert_date == "2015-05-17")

wd.tavg %>% filter(date == "2015-05-17")


fires.raw[2403,]

for (dt in alert_dates$alert_date) {
  wd = wd.tavg %>% filter(date == dt)
  idx = which(fires.raw$parish == parish_name & fires.raw$alert_date == dt)
  fires.raw[idx,]$tavg = wd$tavg/10
}

names(weather_data[1:1])



freguesia = "Abragão"
df = eval(parse(text=sprintf("weather_data$'%s'$tavg", freguesia)))
df_subset = subset(df, as.Date(df$date) %in% as.Date(dates$alert_date[1]))

idx = which(fires.raw$parish == freguesia)
dates = fires.raw[idx,"alert_date"]
date = dates[1,]

for (dt in dates[1]) {
  idx.dt = which(df$date %in% as.Date(dt))
  df_tavg = df[idx.dt,]
}
df_tavg

#fires.raw %>% mutate(tavg = if_else(parish == freguesia && as.Date(alert_date) %in% ) )



df_subset = subset(df, as.Date(df$date) %in% as.Date(dates$alert_date[1]))



for (name in names(weather_data)) {
  df.tavg = eval(parse(text=sprintf("weather_data$'%s'$tavg", name)))
  
  
 
  wd = get_weather_data(station, c("TAVG","TMAX","TMIN","PRCP"))
  weather_data = append(weather_data, eval(parse(text=sprintf("list('%s'= wd)", parish))))
}

