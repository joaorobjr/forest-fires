library(tidyverse)
library(funModeling)
library(measurements)
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

#save(fires.raw, file = "fires.raw.RData")
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
                                          year_min = 2014, year_max = 2015,
                                          limit = 10)
  return(nearby_stations)
}

get_weather_data = function (station, measure) {
  wd <- ghcnd_search(station, var = measure, date_min = "2014-12-16", date_max = "2015-12-31")
  return(wd)
}

# Create dataframe with all distinct parish, lat and lon. 
# After Rename columns to names accepted by meteo_nearby_stations method
df_parish = distinct(fires.raw, parish, lat, lon) %>% 
  rename(id = parish, latitude = lat, longitude = lon)

parishes = distinct(fires.raw, parish) %>% arrange(parish) 

weather_measures = c("TAVG", "TMAX", "TMIN", "PRCP", "AWND")

stations = get_nearby_stations(df_parish, weather_measures)

weather_data = c()

for (parish in parishes$parish) {
  station = eval(parse(text=sprintf("stations$'%s'$id[1]", parish)))
  wd = get_weather_data(station, weather_measures)
  weather_data = append(weather_data, eval(parse(text=sprintf("list('%s'= wd)", parish))))
}

save(weather_data, file = "weather_data.RData")
load("weather_data.RData")


# MERGE WEATHER DATA AND FOREST FIRES -----------------------------------------

# Create new attributes: tavg, tmax, tmin, prcp
fires.raw = fires.raw %>% mutate(tavg = 0, tmax = 0, tmin = 0, prcp = 0)

for (parish_name in parishes$parish) {
  
  arr_alert_dates = fires.raw %>% filter(parish == parish_name) %>% select(alert_date)
  
  wdTAVG = eval(parse(text=sprintf("weather_data$'%s'$tavg", parish_name)))
  wdTMAX = eval(parse(text=sprintf("weather_data$'%s'$tmax", parish_name)))
  wdTMIN = eval(parse(text=sprintf("weather_data$'%s'$tmin", parish_name)))
  wdPRCP = eval(parse(text=sprintf("weather_data$'%s'$prcp", parish_name)))
  
  for (dt in arr_alert_dates$alert_date) {
    
    idx = which(fires.raw$parish == parish_name & fires.raw$alert_date == dt)
    
    if(!is.null(wdTAVG)){
      #wdTAVG = eval(parse(text=sprintf("filter(wdTAVG, date == '%s')", dt)))
      wdTAVG = filter(wdTAVG, date == dt)
      if(nrow(wdTAVG)>0){
        fires.raw[idx,]$tavg = wdTAVG$tavg/10
      }
    }
    
    if(!is.null(wdTMAX)){
      wdTMAX = filter(wdTMAX, date == dt)
      if(nrow(wdTMAX)>0){
        fires.raw[idx,]$tmax = wdTMAX$tmax/10
      }
    }
    
    if(!is.null(wdTMIN)){
      wdTMIN = filter(wdTMIN, date == dt)
      if(nrow(wdTMIN)>0){
        fires.raw[idx,]$tmin = wdTMIN$tmin/10
      }
    }
    
    if(!is.null(wdPRCP)){
      wdPRCP = filter(wdPRCP, date == dt)
      if(nrow(wdPRCP)>0){
        fires.raw[idx,]$prcp = wdPRCP$prcp/10
      }
    }
  }
}

ffires = fires.raw

save(ffires, file = "ffires.RData")
load("ffires.RData")



#Remove Redundant Features
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load the data
#data(ffires)
# calculate correlation matrix: village_area until total_area
correlationMatrix <- cor(ffires[,16:20])
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated)
#highly correlated: village_veget_area and total area




#Rank Features By Importance
set.seed(7)
# load the library
#library(mlbench)
#library(caret)
# load the dataset
#data(PimaIndiansDiabetes)
# prepare training scheme
control <- trainControl(method="repeatedcv", number=10, repeats=3)
# train the model
model <- train(cause_type~id + origin, data=ffires, method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)





#loading graphics library
library(ggplot2)

#bar plot of forests fires during 2015
# Conclusion: july and august were the months with a great number of ocurrences
ggplot(fires.raw, aes(x = month(alert_date, label = T))) +
  geom_bar(fill = "red") +
  ggtitle("Distribution of forests fires across 2015") +
  labs(x="Months", y="Total")

#bar plot of forests fires during 2015 by region
#Conclusion: Entre Douro e Minho was the region with mores forests fires
ggplot(fires.raw, aes(x = region)) +  theme(axis.text.x = element_text(angle = 90)) + geom_bar() + ggtitle("Distribution of forests fires by region") +
  labs(x="Region", y="Total")

#bar plot of forests fires during 2015 by district
#Porto was the district with more forests fires
ggplot(fires.raw, aes(x = district)) +  theme(axis.text.x = element_text(angle = 90))+geom_bar() + ggtitle("Distribution of forests fires by district") +
  labs(x="District", y="Total")

#bar plot of forests fires during 2015 by causes
# negligence was the big cause of the forests fires
ggplot(fires.raw, aes(x = cause_type)) + geom_bar(color = "black", fill = "light blue") + ggtitle("Distribution of causes of fires")+
  labs(x="Causes of Fires", y="Total")

#bar plot of forests fires during 2015 by origin
#firepit was the origin of the most forests fires
ggplot(fires.raw, aes(x = origin)) + geom_bar(fill = "blue") + ggtitle("Distribution of forests fires by origin across 2015")+
  labs(x="Origin", y="Total")

#Relationship between district, month and causes
ggplot(fires.raw, aes(x = district, y = month(alert_date, label = T))) +  theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(color = cause_type)) + ggtitle("Relationship between district, month and cause_type")+
  labs(x="District", y="Months")

#Relationship between region, month and causes
ggplot(fires.raw, aes(x = region, y = month(alert_date, label = T))) +  theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(color = cause_type)) + ggtitle("Relationship between region and month") + labs(x="Region", y="Months")




#creating datasets of train and teste
library(caret)
#generate the same datasets in any computer
set.seed(123)
#generate randomly a dataset
lines <- createDataPartition(y=fires.raw$cause_type,p=.7,list=FALSE)
#divide into two datasets
fires.raw_train <- fires.raw %>% slice(lines)
fires.raw_test <- fires.raw %>% slice(-lines)

#creating a model of decision tree
library(rpart)
#using cause_type variable in function of the all others and with high complexity
modelo <- rpart(cause_type ~., data = fires.raw_train, control = rpart.control(cp=0))

#predicting
fires.raw_test$Previsão <-predict(modelo, fires.raw_test)
view(fires.raw_test)


#creating a model of knn
fires.raw_test_causes <- fires.raw_test$cause_type
fires.raw_test <- fires.raw_test %>% select(-cause_type)

#DEPOIS DO TIL TEM DE VIR NUM OU FACTORS
knn.model <- knn3(cause_type ~region + origin + lat + lon,data=fires.raw_train,k=4)

knn.model
#making predictions on data test
knn.preds <- predict(knn.model,fires.raw_test,type="class")

#obtain the confusion matrix
knn.confM <- confusionMatrix(fires.raw_test_causes,knn.preds)
knn.confM

#repeat the process for different k´s