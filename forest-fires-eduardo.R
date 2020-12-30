library(tidyverse)
library(funModeling)
library(measurements)
library(lubridate)
library(ggplot2)
library('rnoaa')
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

# Get observations index with wrong value in lat variable
idx.lat.wrong = which(str_detect(fires.raw$lat, '1900-01-01'))
lat.wrong = fires.raw[idx.lat.wrong,]
head(select(lat.wrong$lat, lat.wrong$lon))

# Number of observation with wrong value in lat variable
print(str_c("There are",length(idx.lat.wrong),"observations with wrong value '1900-01-01'", sep = " "))

# Value imputation based on another observation that has same: region, district, municipality, parish. 
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

# Cleaning lat and lon variables and convert from GPS coodinate to decimals
vec_clean <- c("''"="", "E-12"="", "E-11"="", "E-02"="", ","=".", "º"=" ", "'"=" ", ":"=" ")
fires.raw$lat <- conv_unit(str_replace_all(str_trim(fires.raw$lat), vec_clean), "deg_min_sec", "dec_deg")
# Ocorre um warning ao executar essa linha
fires.raw$lon <- conv_unit(str_replace_all(str_trim(fires.raw$lon, side = "both"), vec_clean), "deg_min_sec", "dec_deg")
# Workaround
fires.raw$lon[7511] <- conv_unit("8 34 21.5868000000013", "deg_min_sec", "dec_deg")

# After the imputation 8 NAs values were assign to observations in latitude and longitude variables

# Insert latitude and longitude for parish with missing values

idx.latlon.na = which(is.na(fires.raw$lat) & is.na(fires.raw$lon))
latlon.na = fires.raw[idx.latlon.na,]
head(select(latlon.na$parish, latlon.na$lat, latlon.na$lon))

#Alentejo - Évora - Mora - Cabeção
fires.raw$lat[439] = 38.954167
fires.raw$lon[439] = -8.072778
#Alentejo -	Évora - Montemor-o-Novo - Cortiçadas de Lavre
fires.raw$lat[1722] = 38.786577
fires.raw$lon[1722] = -8.432094
#Alentejo - Évora - Montemor-o-Novo - Ciborro
fires.raw$lat[2633] = 38.800833
fires.raw$lon[2633] = -8.228056
#Alentejo -	Évora -	Mourão - Granja
fires.raw$lat[3007] = 38.3 
fires.raw$lon[3007] = -7.255
#Alentejo -	Évora -	Évora	- Horta das Figueiras
fires.raw$lat[3443] = 38.545
fires.raw$lon[3443] = -7.905556
#Alentejo -	Évora - Montemor-o-Novo - Cortiçadas de Lavre
fires.raw$lat[3586] = 38.786577
fires.raw$lon[3586] = -8.432094
#Alentejo - Évora - Estremoz - São Lourenço de Mamporcão	
fires.raw$lat[5284] = 38.890833
fires.raw$lon[5284] = -7.545833
#	Alentejo - Évora - Mora - Brotas
fires.raw$lat[7228] = 38.873056
fires.raw$lon[7228] = -8.15

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

# MERGE WEATHER DATA AND FOREST FIRES --------------------------------------------
load("data/station_data.Rdata")

get_nearby_stations = function (df, measure){
  nearby_stations = meteo_nearby_stations(lat_lon_df = df,
                                          station_data = station_data, 
                                          radius = 1000, 
                                          var = measure,
                                          year_min = 2014, year_max = 2015,
                                          limit = 5)
  return(nearby_stations)
}

fires.raw = fires.raw %>% mutate(tavg = 0, tavg15d = 0, tmax = 0, tmin = 0, prcp = 0)

fires = distinct(fires.raw, parish, lat, lon, alert_date)

for (i in 1:dim(fires)[1]) {
  fire = fires[i,]
  idx = which(fires.raw$parish == fire$parish & fires.raw$lat == fire$lat & fires.raw$lon == fire$lon & fires.raw$alert_date == fire$alert_date)
  print(idx)
  df = data.frame(id=fire$parish, latitude=fire$lat, longitude=fire$lon, stringsAsFactors = F)
  stations = get_nearby_stations(df, c("TAVG", "TMAX", "TMIN", "PRCP"))
  
  # Set TAVG
  for(j in 1:dim(stations[[1]])[1] ){
    wd = ghcnd_search(stations[[1]]$id[j], var = "TAVG", date_min = fire$alert_date, date_max = fire$alert_date)
    if(!is.null(wd$tavg) && nrow(wd$tavg)>0 && !is.na(wd$tavg$tavg) && wd$tavg$tavg > 0){
      tavg = wd$tavg$tavg
      #print(str_c("Data: ", fire$alert_date, " tavg: ", tavg))
      fires.raw[idx,]$tavg = tavg/10
      break()
    }
  }
  
  dt_ini = as.character(as.Date(fire$alert_date) - ddays(15))
  
  # Set TAVG15D
  for(j in 1:dim(stations[[1]])[1] ){
    wd = ghcnd_search(stations[[1]]$id[j], var = "TAVG", date_min = dt_ini, date_max = fire$alert_date)
    if(!is.null(wd$tavg) && nrow(wd$tavg)>0){
      tavg15d = mean(wd$tavg$tavg, na.rm = TRUE)
      #print(str_c("Data: ", fire$alert_date, " tavg15d: ", tavg15d))
      fires.raw[idx,]$tavg15d = tavg15d/10
      break()
    }
  }
  
  # Set TMAX
  for(j in 1:dim(stations[[1]])[1] ){
    wd = ghcnd_search(stations[[1]]$id[j], var = "TMAX", date_min = fire$alert_date, date_max = fire$alert_date)
    if(!is.null(wd$tmax) && nrow(wd$tmax)>0 && !is.na(wd$tmax$tmax) && wd$tmax$tmax > 0){
      tmax = wd$tmax$tmax
      #print(str_c("Data: ", fire$alert_date, " tmax: ", tmax))
      fires.raw[idx,]$tmax = tmax/10
      break()
    }
  }
  
  # Set TMIN
  for(j in 1:dim(stations[[1]])[1] ){
    wd = ghcnd_search(stations[[1]]$id[j], var = "TMIN", date_min = fire$alert_date, date_max = fire$alert_date)
    if(!is.null(wd$tmin) && nrow(wd$tmin)>0 && !is.na(wd$tmin$tmin) && wd$tmin$tmin > 0){
      tmin = wd$tmin$tmin
      #print(str_c("Data: ", fire$alert_date, " tmin: ", tmin))
      fires.raw[idx,]$tmin = tmin/10
      break()
    }
  }
  
  # Set PRCP
  for(j in 1:dim(stations[[1]])[1] ){
    wd = ghcnd_search(stations[[1]]$id[j], var = "PRCP", date_min = fire$alert_date, date_max = fire$alert_date)
    if(!is.null(wd$prcp) && nrow(wd$prcp)>0 && !is.na(wd$prcp$prcp) && wd$prcp$prcp > 0){
      prcp = wd$prcp$prcp
      #print(str_c("Data: ", fire$alert_date, " prcp: ", prcp))
      fires.raw[idx,]$prcp = prcp/10
      break()
    }
  }
}

ffires = as_tibble( fires.raw)

#Value imputation

#Imputation of value in tavg variable based on tmax and tmin
ffires = ffires %>% rowwise() %>%  mutate(tavg = if_else(tavg == 0 & tmax != 0 & tmin != 0, (tmax+tmin)/2, tavg))

#Imputation of value in tavg variable based on tavg15d
ffires = ffires %>% rowwise() %>%  mutate(tavg = if_else(tavg == 0 & tavg15d != 0, tavg15d, tavg))

#Imputation of value in tavg15d variable if NaN
ffires = ffires %>% rowwise() %>%  mutate(tavg15d = if_else(is.nan(tavg15d), 0, tavg15d))

#Imputation of value in tmax variable based on tavg and tmin
ffires = ffires %>% rowwise() %>%  mutate(tmax = if_else(tmax == 0 & tavg != 0 & tmin != 0, (2*tavg)-tmin, tmax))

#Imputation of value in tmin variable based on tavg and tmax
ffires = ffires %>% rowwise() %>%  mutate(tmin = if_else(tmin == 0 & tavg != 0 & tmax != 0, (2*tavg)-tmax, tmin))

ffires = as_tibble(ffires)

#save(ffires, file = "ffires.RData")
load("ffires.RData")

df_status(ffires)


# FEATURE SELECTION ----------------------------------------------

#library(Boruta)

library(caret)

ffires.rf = ffires

ffires.rf = ffires.rf %>% select(-id, -region, -municipality, -parish, -alert_source, -alert_date, -alert_hour, -firstInterv_date, -firstInterv_hour, -extinction_date, -extinction_hour)
ffires.rf = ffires.rf %>% filter(!is.na(extinction))
ffires.rf = ffires.rf %>% filter(!is.na(firstInterv))

#ffires$district = as.character(ffires$district)
#ffires$municipality = as.character(ffires$municipality)
#ffires$parish = as.character(ffires$parish)
ffires.rf$lat = as.numeric(ffires.rf$lat)
ffires.rf$lon = as.numeric(ffires.rf$lon)
ffires.rf$latency_alert_ext = as.numeric(ffires.rf$latency_alert_ext)
ffires.rf$latency_alert_interv = as.numeric(ffires.rf$latency_alert_interv)
ffires.rf$latency_interv_ext = as.numeric(ffires.rf$latency_interv_ext)


df_status(ffires.rf)
set.seed(123456)
idx.trainset = createDataPartition(ffires.rf$cause_type, p = 0.7, list = FALSE)
trainSet = ffires.rf[ idx.trainset,] 
testSet <- ffires.rf[-idx.trainset,]


#rfFit <- train(cause_type ~ ., data = trainSet, method = "rf")

outcomeName <-'cause_type'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName] 
rfControl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 3, verbose = TRUE) 
rfProfile <- rfe(trainSet[,predictors], trainSet[[outcomeName]], sizes=c(1:20), rfeControl = rfControl) 


















cause_pred_profile

save(cause_pred_profile, file = "cause_pred_profile.RData")

predictors(cause_pred_profile)
cause_pred_profile$fit

trellis.par.set(caretTheme())
plot(cause_pred_profile, type = c("g", "o"))

#Recursive feature selection 
#Outer resampling method: Cross-Validated (10 fold, repeated 3 times) 
#Resampling performance over subset size: 
#  Variables Accuracy  Kappa AccuracySD KappaSD Selected 
#4   0.7737 0.4127    0.03707 0.09962         
#8   0.7874 0.4317    0.03833 0.11168         
#16   0.7903 0.4527    0.04159 0.11526         
#18   0.7882 0.4431    0.03615 0.10812         
#The top 5 variables (out of 16): 
#  Credit_History, LoanAmount, Loan_Amount_Term, ApplicantIncome, CoapplicantIncome 
#Taking only the top 5 predictors predictors<-c("Credit_History", "LoanAmount", "Loan_Amount_Term", "ApplicantIncome", "CoapplicantIncome")




rPartMod <- train(Class ~ ., data=trainData, method="rpart")





#--------------------------------------------------

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
model <- train(cause_type~., data=ffires.rf, method="lvq", preProcess="scale", trControl=control)
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
lines <- createDataPartition(y=ffires.rf$cause_type,p=.7,list=FALSE)
#divide into two datasets
ffires.rf_train <- ffires.rf %>% slice(lines)
ffires.rf_test <- ffires.rf %>% slice(-lines)

#-------------------DECISION TREE ----------------------------
#creating a model of decision tree
library(rpart)
#using cause_type variable in function of the all others and with high complexity
modelo <- rpart(cause_type ~., data = ffires.rf_train, control = rpart.control(cp=0))

#predicting
ffires.rf_tree_test$Previsão <-predict(modelo, ffires.rf_test)
view(ffires.rf_tree_test)


#------------------------KNN--------------------------------
#creating a model of knn
ffires.rf_test_causes <- ffires.rf_test$cause_type
ffires.rf_knn_test <- ffires.rf_test %>% select(-cause_type)

#DEPOIS DO TIL TEM DE VIR NUM OU FACTORS
knn.model <- knn3(cause_type ~ district + origin + alert + extinction, data=ffires.rf_train,k=5)


#making predictions on data test
knn.preds <- predict(knn.model,ffires.rf_knn_test,type="class")

#obtain the confusion matrix
knn.confM <- confusionMatrix(ffires.rf_test_causes,knn.preds)
knn.confM

#repeat the process for different k´s


