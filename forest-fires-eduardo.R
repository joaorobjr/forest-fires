library(dplyr)
library(funModeling)
library(stringr)
library(chron)
library(measurements)
library(lubridate)

#Load raw data --------------------------------------------------
fires.raw = as_tibble(read.csv("C:/Users/edamr/Downloads/fires2015_train.csv",
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
# Workaround
fires.raw$lon[7511] <- conv_unit("8 34 21.5868000000013", "deg_min_sec", "dec_deg")
# Ocorre um warning ao executar essa linha
fires.raw$lon <- conv_unit(str_replace_all(str_trim(fires.raw$lon), vec_clean), "deg_min_sec", "dec_deg")


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

#saving this point
#save(fires.raw, file = "fires.raw.RData")
#load("fires.raw.RData")

#extracting month from the alert_date and creating a month column
fires.raw$month <- str_extract(fires.raw$alert_date, "\\-[:digit:][:digit:]\\-")

#change "-" by "" on the month column
fires.raw$month <- str_replace_all(fires.raw$month, "\\-", "")

#change to factor
fires.raw$month = as.factor(fires.raw$month)

#bar plot of forests fires during 2015
# july and august were the months with a great number of ocurrences
ggplot(fires.raw, aes(x = month)) + geom_bar() + ggtitle("Distribution of forests fires across 2015")

#bar plot of forests fires during 2015 by region
#Entre Douro e Minho was the region with mores forests fires
ggplot(fires.raw, aes(x = region)) +  theme(axis.text.x = element_text(angle = 90)) + geom_bar() + ggtitle("Distribution of forests fires by region")

#bar plot of forests fires during 2015 by district
#Porto was the district with more forests fires
ggplot(fires.raw, aes(x = district)) +  theme(axis.text.x = element_text(angle = 90))+geom_bar() + ggtitle("Distribution of forests fires by district")

#bar plot of forests fires during 2015 by causes
# negligence was the big cause of the forests fires
ggplot(fires.raw, aes(x = cause_type)) + geom_bar(color = "black", fill = "light blue") + ggtitle("Distribution of causes of fires")

#bar plot of forests fires during 2015 by origin
#firepit was the origin of the most forests fires
ggplot(fires.raw, aes(x = origin)) + geom_bar(fill = "blue") + ggtitle("Distribution of forests fires by origin across 2015")

#Relationship between district, month and causes
ggplot(fires.raw, aes(x = district, y = month)) +  theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(color = cause_type)) + ggtitle("Relationship between district, month and cause_type")

#Relationship between region, month and causes
ggplot(fires.raw, aes(x = region, y = month)) +  theme(axis.text.x = element_text(angle = 90)) + geom_point(aes(color = cause_type)) + ggtitle("Relationship between region and month")

#Points of fires according latitude,longitude and region
ggplot(fires.raw, aes(x = lat, y = lon)) +  theme(axis.text.x = element_text(angle = 90)) +  scale_x_discrete(labels = abbreviate) +  theme(axis.text.y = element_text(angle = 0)) +  scale_y_discrete(labels = abbreviate) + geom_point(aes(color = region)) + ggtitle("Points of fires according lat and lon")
