library(tidyverse)
library(funModeling)
library(measurements)
library(lubridate)
library(ggplot2)
library('rnoaa')
library(devtools)
library(caret)
library(Boruta)
library(nnet)
library(pROC)
library(rpart)
library(rpart.plot)


options(noaakey = "mqEuOSuAUjyuGlTjVjxxCpzRlbrooRnr")
options(scipen= 999)

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

# MERGE WEATHER DATA AND FOREST FIRES 
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

#Create feature: alert_month
ffires$alert_month = month(ffires$alert, label = FALSE)

#Create feature: alert_period
ffires$alert_period = if_else(between(hour(ffires$alert),6,11),"Morning",if_else(between(hour(ffires$alert),12,17),"Afternoon",if_else(between(hour(ffires$alert),18,23),"Night", "Dawn")))

#Create feature: duration
ffires = ffires %>% rowwise() %>% mutate(duration = (extinction-alert)/60)

ffires.full = as_tibble(ffires)

# Fix 
idx.extinction.wrong = which(ffires.full$latency_alert_ext<0)
for (idx in idx.extinction.wrong) {
  ffires.full[idx,]$extinction = ffires.full[idx,]$extinction + ddays(1)
}

#Create feature: duration
ffires.full = ffires.full %>% rowwise() %>% mutate(duration = (extinction-alert)/60)

save(ffires.full, file = "ffiresFull.RData")


# FEATURE SELECTION ----

load("ffiresFull.RData")

# Selection of relevant features (remove redundancy) ----
ffires = ffires.full %>% select(district, origin, alert_month, alert_period, duration, village_area, vegetation_area, farming_area, tavg, tavg15d, prcp, cause_type)

# fix data type
ffires$alert_month = as.factor(ffires$alert_month)
ffires$alert_period = as.factor(ffires$alert_period)
ffires$duration = as.numeric(ffires$duration)
ffires = ffires %>% filter(!is.na(duration))

df_status(ffires)

#save(ffires, file = "ffires.RData")
load("ffires.RData")

# Create train set and test set ----
set.seed(123456)
idx.trainset = createDataPartition(ffires$cause_type, p = 0.7, list = FALSE)
trainSet = ffires[ idx.trainset,] 
testSet <- ffires[-idx.trainset,]

# Recursive Feature Elimination -------------------------------------------------
outcomeName <-'cause_type'
predictors <- names(trainSet)[!names(trainSet) %in% outcomeName] 
rfControl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 5, verbose = TRUE) 
rfProfile <- rfe(trainSet[,predictors], trainSet[[outcomeName]], sizes=c(1:11), rfeControl = rfControl) 

save(rfProfile, file = "rfProfile.RData")
load("rfProfile.RData")

rfProfile
predictors(rfProfile)
plot(rfProfile, type = c("g", "o"))

# Boruta ------------------------------------------------------------------------
boruta <- Boruta(cause_type ~ ., data = trainSet, doTrace = 2)
save(boruta, file = "boruta.RData")
load("boruta.RData")

boruta
print(boruta)
plot(boruta)
attStats(boruta)

# FIT MODELS ----------------------------------------------------------------------

# Refazer o train set e o teste se com base no resultado do RFE e do Boruta, 
# ou seja, remover a variavel PRCP
ffires = ffires %>% select(-prcp)

idx.dist.VC = which(ffires$district == "Viana Do Castelo")
for (idx in idx.dist.VC) {
  ffires[idx,]$district = "Viana do Castelo" 
}

set.seed(123456)
idx.trainset = createDataPartition(ffires$cause_type, p = 0.7, list = FALSE)
trainSet = ffires[ idx.trainset,] 
testSet <- ffires[-idx.trainset,]

ctrl <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = T, verboseIter = TRUE)

# Distance-Based Approach -----------------------------------------------------------

# KNN ----
knnModel <- train(cause_type ~ ., data = trainSet, method = "kknn", trControl = ctrl)
#save(knnModel, file = "models/knnModel.RData")
load("models/knnModel.RData")
knnModel
knnModel$pred
knnModel$results$
knnModel$bestTune
final = knnModel$finalModel

final$best.parameters
cm=confusionMatrix(knnModel)


# Make predictions
knnPredict = predict(knnModel, testSet, type = "prob")
predKnn = knnPredict[,2]
df = data.frame(predKnn,testSet$cause_type)

knnPredict2 = prediction(predKnn, testSet$cause_type) 

knnPredict[,2]

library("ROCR")
Pred.cart = predict(train.cart, newdata = Test, type = "prob")[,2] 
Pred2 = prediction(Pred.cart, Test$n.use) 
plot(performance(Pred2, "tpr", "fpr"))
abline(0, 1, lty = 2)

library(trainSet)


par(mfrow=c(4,3))

multiclass.roc(testSet$cause_type, knnPredict, percent = T, plot = T, print.auc = T)

auc=auc(knnRoc)


names(knnRoc$rocs)
rs <- knnRoc[['rocs']]
plot.roc(rs[1])
sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))

knnRoc$
# Model accuracy
mean(knnPredict == testSet$cause_type)

multiclass.roc(testSet$cause_type, knnPredict, percent = T, plot = T)

# Probabilistic Approach ------------------------------------------------------------

# Naive Bayes ----
nbModel <- train(cause_type ~ ., data = trainSet, method = "naive_bayes", trControl = ctrl)
#save(nbModel, file = "models/nbModel.RData")
#load("models/nbModel.RData")
nbModel
nbModel$pred
nbModel$results
nbModel$bestTune
nbModel$finalModel
confusionMatrix(nbModel)

# Make predictions
nbPredict = predict(nbModel, testSet)
# Model accuracy
mean(nbPredict == testSet$cause_type)

# Logist Regression (Multinominal) ----
lrModel <- train(cause_type ~ ., data = trainSet, method = "multinom", trControl = ctrl,)
#save(lrModel, file = "models/lrModel.RData")
load("models/lrModel.RData")
lrModel
lrModel$pred
lrModel$results
lrModel$bestTune
lrModel$finalModel
confusionMatrix(lrModel)

# Make predictions
lrPredict = predict(lrModel, testSet)
# Model accuracy
mean(lrPredict == testSet$cause_type)

# Mathematical Approach -------------------------------------------------------------

# Linear Discriminants ----
ldaModel <- train(cause_type ~ ., data = trainSet, method = "lda", trControl = ctrl)
#save(ldaModel, file = "models/ldaModel.RData")
load("models/ldaModel.RData")
ldaModel
ldaModel$pred
ldaModel$results
ldaModel$bestTune
ldaModel$finalModel
confusionMatrix(ldaModel)

# Make predictions
ldaPredict = predict(ldaModel, testSet)
# Model accuracy
mean(ldaPredict == testSet$cause_type)

# Linear Discriminants (Penalized Discriminant Analysis) ----
pdaModel <- train(cause_type ~ ., data = trainSet, method = "pda", trControl = ctrl)
#save(pdaModel, file = "models/pdaModel.RData")
load("models/pdaModel.RData")
pdaModel
pdaModel$pred
pdaModel$results
pdaModel$bestTune
pdaModel$finalModel
confusionMatrix(pdaModel)

# Make predictions
pdaPredict = predict(pdaModel, testSet)
# Model accuracy
mean(pdaPredict == testSet$cause_type)

# Logical Approach -----------------------------------------------------------------

# Decision Tree ----
treeModel <- train(cause_type ~ ., data = trainSet, method = "rpart", trControl = ctrl)
#save(treeModel, file = "models/treeModel.RData")
load("models/treeModel.RData")
treeModel
treeModel$pred
treeModel$results
treeModel$bestTune
treeModel$finalModel
confusionMatrix(treeModel)
#Plot tree
prp(treeModel$finalModel, box.palette = "Reds", tweak = 1.2)

# Make predictions
treePredict = predict(treeModel, testSet)
# Model accuracy
mean(treePredict == testSet$cause_type)

# Tree Bag ----  
tbModel <- train(cause_type ~ ., data = trainSet, method = "treebag", trControl = ctrl)
#save(tbModel, file = "models/tbModel.RData")
load("models/tbModel.RData")
tbModel
tbModel$pred
tbModel$results
tbModel$bestTune
tbModel$finalModel
confusionMatrix(tbModel)

# Make predictions
tbPredict = predict(tbModel, testSet)
# Model accuracy
mean(tbPredict == testSet$cause_type)

# Optimization Approach ------------------------------------------------------------

# Neural Networks ----
nnModel <- train(cause_type ~ ., data = trainSet, method = "nnet", trControl = ctrl)
#save(nnModel, file = "models/nnModel.RData")
load("models/nnModel.RData")
nnModel
nnModel$pred
nnModel$results
nnModel$bestTune
nnModel$finalModel
confusionMatrix(nnModel)

# Make predictions
nnPredict = predict(nnModel, testSet)
# Model accuracy
mean(nnPredict == testSet$cause_type)

# SVM Linear ----
svmLinModel <- train(cause_type ~ ., data = trainSet, method = "svmLinear2", trControl = ctrl)
#save(svmLinModel, file = "models/svmLinModel.RData")
load("models/svmLinModel.RData")
svmLinModel
svmLinModel$pred
svmLinModel$results
svmLinModel$bestTune
svmLinModel$finalModel
confusionMatrix(svmLinModel)

# Make predictions
svmLinPredict = predict(svmLinModel, testSet)
# Model accuracy
mean(svmLinPredict == testSet$cause_type)

# SVM Poly ----
svmPolModel <- train(cause_type ~ ., data = trainSet, method = "svmPoly", trControl = ctrl)
#save(svmPolModel, file = "models/svmPolModel.RData")
load("models/svmPolModel.RData")
svmPolModel
svmPolModel$pred
svmPolModel$results
svmPolModel$bestTune
svmPolModel$finalModel
confusionMatrix(svmPolModel)

# Make predictions
svmPolPredict = predict(svmPolModel, testSet)
# Model accuracy
mean(svmPolPredict == testSet$cause_type)

# SVM Radial ----
svmRadModel <- train(cause_type ~ ., data = trainSet, method = "svmRadial", trControl = ctrl)
#save(svmRadModel, file = "models/svmRadModel.RData")
load("models/svmRadModel.RData")
svmRadModel
svmRadModel$pred
svmRadModel$results
svmRadModel$bestTune
svmRadModel$finalModel
confusionMatrix(svmRadModel)

# Make predictions
svmRadPredict = predict(svmRadModel, testSet)
# Model accuracy
mean(svmRadPredict == testSet$cause_type)

# Ensembles ------------------------------------------------------------------------

# Random Forest ----
rfModel <- train(cause_type ~ ., data = trainSet, method = "rf", trControl = ctrl)
#save(rfModel, file = "models/rfModel.RData")
load("models/rfModel.RData")
rfModel$
rfModel$pred
rfModel$results
rfModel$bestTune
rfModel$finalModel
confusionMatrix(rfModel)

# Make predictions
rfPredict = predict(rfModel, testSet)
# Model accuracy
mean(rfPredict == testSet$cause_type)

# XGBoost ----
xgbModel <- train(cause_type ~ ., data = trainSet, method = "xgbTree", trControl = ctrl)
#save(xgbModel, file = "models/xgbModel.RData")
#load("models/xgbModel.RData")
xgbModel
xgbModel$pred
xgbModel$results
xgbModel$bestTune
xgbModel$finalModel
confusionMatrix(xgbModel)

cmxgb$table

# Make predictions
xgbPredict = predict(xgbModel, testSet)
# Model accuracy
mean(xgbPredict == testSet$cause_type)


# ASSESS MODELS PERFORMANCE -------------------------------------------------------

rs <- resamples(list("K-Nearest Neighbors" = knnModel,
                     "Naive Bayes" = nbModel,
                     "Logistic Regression" = lrModel,
                     "Linear Discriminants" = ldaModel,
                     "Penalized Discriminants" = pdaModel,
                     "Decision Tree" = treeModel, 
                     "Tree Bag" = tbModel,
                     "Neural Networks" = nnModel,
                     "SVM (Linear)" = svmLinModel,
                     "SVM (Poly)" = svmPolModel,
                     "SVM (Radial)" = svmRadModel,
                     "Random Forest" = rfModel, 
                     "XGBoost" = xgbModel)) 

rs$timings$
                 
summary(rs)

library(DALEX)

predictors = testSet[,-11]
outcome = testSet$cause_type

explainer_knn <- explain(rfModel, predictors, outcome, label = "K-Nearest Neighbors")
explainer_nb  <- explain(nbModel, predictors, outcome, label = "Naive Bayes")
explainer_lr <- explain(lrModel, predictors, outcome, label = "Logistic Regression")
explainer_lda <- explain(ldaModel, predictors, outcome, label = "Linear Discriminants")
explainer_pda <- explain(pdaModel, predictors, outcome, label = "Penalized Discriminants")
explainer_tree <- explain(treeModel, predictors, outcome, label = "Decision Tree")
explainer_tb <- explain(tbModel, predictors, outcome, label = "Tree Bag")
explainer_nn <- explain(nnModel, predictors, outcome, label = "Neural Networks")
explainer_svmLin <- explain(svmLinModel, predictors, outcome, label = "SVM Linear")
explainer_svmPol <- explain(svmPolModel, predictors, outcome, label = "SVM Polynomyal")
explainer_svmRad <- explain(svmRadModel, predictors, outcome, label = "SVM Radial")
explainer_rf <- explain(rfModel, predictors, outcome, label = "Random Forest")
explainer_xgb <- explain(xgbModel, predictors, outcome, label = "XGBoost")

save(explainer_knn, file="results/explainer_knn.RData")
save(explainer_nb, file="results/explainer_nb.RData")
save(explainer_lr, file="results/explainer_lr.RData")
save(explainer_lda, file="results/explainer_lda.RData")
save(explainer_pda, file="results/explainer_pda.RData")
save(explainer_tree, file="results/explainer_tree.RData")
save(explainer_tb, file="results/explainer_tb.RData")
save(explainer_nn, file="results/explainer_nn.RData")
save(explainer_svmLin, file="results/explainer_svmLin.RData")
save(explainer_svmPol, file="results/explainer_svmPol.RData")
save(explainer_svmRad, file="results/explainer_svmRad.RData")
save(explainer_rf, file="results/explainer_rf.RData")
save(explainer_xgb, file="results/explainer_xgb.RData")

eva_knn = model_performance(explainer_knn)
eva_nb = model_performance(explainer_nb)
eva_lr = model_performance(explainer_lr)
eva_lda = model_performance(explainer_lda)
eva_pda = model_performance(explainer_pda)
eva_tree = model_performance(explainer_tree)
eva_tb = model_performance(explainer_tb)
eva_nn = model_performance(explainer_nn)
eva_svmLin = model_performance(explainer_svmLin)
eva_svmPol = model_performance(explainer_svmPol)
eva_svmRad = model_performance(explainer_svmRad)
eva_rf = model_performance(explainer_rf)
eva_xgb = model_performance(explainer_xgb)


load("results/eva_xgb.RData")

eva_xgb$measures

save(eva_knn, file="results/eva_knn.RData")
save(eva_nb, file="results/eva_nb.RData")
save(eva_lr, file="results/eva_lr.RData")
save(eva_lda, file="results/eva_lda.RData")
save(eva_pda, file="results/eva_pda.RData")
save(eva_tree, file="results/eva_tree.RData")
save(eva_tb, file="results/eva_tb.RData")
save(eva_nn, file="results/eva_nn.RData")
save(eva_svmLin, file="results/eva_svmLin.RData")
save(eva_svmPol, file="results/eva_svmPol.RData")
save(eva_svmRad, file="results/eva_svmRad.RData")
save(eva_rf, file="results/eva_rf.RData")
save(eva_xgb, file="results/eva_xgb.RData")

load("results/eva_knn.RData")
load("results/eva_nb.RData")
load("results/eva_lr.RData")
load("results/eva_lda.RData")
load("results/eva_pda.RData")
load("results/eva_tree.RData")
load("results/eva_tb.RData")
load("results/eva_nn.RData")
load("results/eva_svmLin.RData")
load("results/eva_svmPol.RData")
load("results/eva_svmRad.RData")
load("results/eva_rf.RData")
load("results/eva_xgb.RData")



models = c("K-Nearest Neighbors", 
           "Naive Bayes",
           "Logistic Regression", 
           "Linear Discriminants", 
           "Penalized Discriminants", 
           "Decision Tree",
           "Tree Bag",
           "Neural Networks",
           "SVM Linear",
           "SVM Polynomyal",
           "SVM Radial",
           "Random Forest",
           "XGBoost")

accuracys = c(round(eva_knn$measures$accuracy*100,2),
              round(eva_nb$measures$accuracy*100,2),
              round(eva_lr$measures$accuracy*100,2),
              round(eva_lda$measures$accuracy*100,2),
              round(eva_pda$measures$accuracy*100,2),
              round(eva_tree$measures$accuracy*100,2),
              round(eva_tb$measures$accuracy*100,2),
              round(eva_nn$measures$accuracy*100,2),
              round(eva_svmLin$measures$accuracy*100,2),
              round(eva_svmPol$measures$accuracy*100,2),
              round(eva_svmRad$measures$accuracy*100,2),
              round(eva_rf$measures$accuracy*100,2),
              round(eva_xgb$measures$accuracy*100,2))

models_acc = data.frame(model=models, accuracy=accuracys)

ggplot(models_acc, aes(x=reorder(model, accuracy), y=accuracy, color=accuracy)) + 
  geom_bar(stat="identity", color='skyblue',fill='steelblue', width = 0.7) +
  geom_text(aes(label=accuracy), vjust=0.4, hjust=1.5, color="white", size=3.5) +
  xlab("Model") + ylab("Accuracy") +
  ggtitle("Rank of Model Performance by Accuracy (%)") +
  theme_minimal() + 
  coord_flip()

plot(eva_knn, eva_nb, eva_lr, eva_lda, eva_pda, eva_tree, eva_tb, eva_nn, eva_svmLin, eva_svmPol, eva_svmRad, eva_rf, eva_xgb, geom = "boxplot", lossFunction = loss_accuracy()) 


#TEST ZONE (NOT RUN) -----------------------------------------------------------------------------




myWorkFlow <- function(form, training, testing, type, ctrl) {
  require(caret, quietly=TRUE)
  ## cary out some data pre-processing
  #myTrain <- mySpecificPreProcessingSteps(train)
  ## now obtain the model
  model <- train(form, training, method = type, trControl = ctrl)
  ## obtain the predictions
  preds <- predict(model, testing)
  ## cary out some predictions post-processing
  #newPreds <- mySpecificPostProcessingSteps(form,train,test,preds)
  #names(newPreds) <- rownames(test)
  ## finally produce the list containing the output of the workflow
  res <- list(trues=responseValues(form, testing), preds)
}

library(performanceEstimation)

knnWF <- function(form, train, test) {
  res <- list(trues=responseValues(form, testSet), preds=knnPredict)
  return(res)
}

cctrl <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = T, verboseIter = TRUE)

#se=c(0,1),step=c(TRUE,FALSE),weightRT=c(0.4,0.5,0.6)

#pre=c("centralImp","scale"),
#post="onlyPos",

wfLogisticRegreession = Workflow(wf='myWorkFlow', wfID="Logistic Regreession", type="multinom", ctrl=cctrl)

res <- performanceEstimation(PredTask(cause_type ~ ., trainSet,"Logistic Regression"),
                             wfLogisticRegreession,
                             EstimationTask(metrics = "acc", method=CV()))

res <- performanceEstimation(PredTask(cause_type ~ ., trainSet,"Linear Discriminants"),
                             workflowVariants("myWorkFlow", type="lda" , ctrl = cctrl),
                             EstimationTask(metrics = "acc",method=CV()))

ussummary(res)
plot(res)


diag_rf = model_diagnostics(explainer_rf)
diag_knn = model_diagnostics(explainer_knn)

plot(diag_knn)
plot(diag_rf, variable = "y", yvariable = "y_hat")
plot(diag, variable = "y", yvariable = "abs_residuals")

explainer_knn <- explain(
  model = knnModel,
  data = ffires[,-11],
  y = ffires$cause_type,
  label = "K-Nearest Neighbor"
)

eva_rf = model_performance(explainer_rf)
eva_knn = model_performance(explainer_knn)

eva_rf$measures$accuracy
eva_knn$measures$accuracy


plot(eva_knn, geom = "boxplot")
plot(mp_ranger, mp_ranger2, geom = "roc")
plot(mp_ranger, mp_ranger2, geom = "lift")
plot(mp_ranger, mp_ranger2, geom = "gain")
plot(mp_ranger, mp_ranger2, geom = "boxplot", lossFunction = loss_accuracy())
plot(mp_ranger, mp_ranger2, geom = "histogram")
plot(mp_ranger, mp_ranger2, geom = "ecdf")

plot(eva_rf, eva_knn, geom = "roc") 
plot(eva_rf, eva_knn, geom = "histogram") 
plot(eva_rf, eva_knn, geom = "boxplot") 

?plot




# Use predict with type="prob" to get class probabilities
knnPredict = predict(knnModel, testSet, type="prob")
xgbPredict = predict(xgbModel, testSet, type="prob")

library(pROC)

knnRoc = multiclass.roc(testSet$cause_type, knnPredict, percent = T, plot = T)
xgbRoc = multiclass.roc(testSet$cause_type, xgbPredict, percent = T, plot = T)



library("patchwork")
knnRoc$predictor + xgbRoc

roc.test(knnRoc$auc,xgbRoc$auc)

a <- auc(knnRoc)


plot.roc(knnRoc$response)
roc$rocs
auc(roc)
roc$rocs












plot(eva_rf, eva_knn, geom = "roc" )


set.seed(1980)
mp <- model_parts(explainer_rf, loss_function = loss_one_minus_auc, B = 1)
mpKnn <- model_parts(explainer_knn, loss_function = loss_cross_entropy)
plot(mp)
plot(mpKnn)

load("rfProfile.RData")

ggplot(rfProfile, metric = "Accuracy")

loss_accuracy(observed = testSet$cause_type, predicted = rfPredict)
loss_one_minus_auc(testSet$cause_type,rfPredict)

set.seed(1980)
model_parts(explainer = explainer_rf, 
            loss_function = loss_one_minus_auc,
            B = 1)

# Fit the model
lrModel <- multinom(cause_type ~., data = trainSet)

lrModel$fitted.values
lrModel$residuals
#Atomic Index Composition
lrModel$AIC

# Summarize the model
summary(lrModel)

# Make predictions
predicted.classes <- lrModel %>% predict(testSet)

head(predicted.classes)
# Model accuracy
mean(predicted.classes == testSet$cause_type)

head(predict(lrModel, newdata = testSet, type = "prob"))
coef(lrModel)

exp(coef(lrModel))

p_hat <- fitted(lrModel)
head(round(p_hat, digits=5))