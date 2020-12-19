library(dplyr)
library(funModeling)
library(stringr)
library(chron)
library(measurements)

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

# Data Preparation -------------------------------------------------------

# Data Cleaning: Identifying and correcting mistakes or errors in the data.

# Check if there are duplicated observations
anyDuplicated(fires.raw)

# Variable: Latitude -----------------------------------------------------

# Fixing incorrect value in variable lat

# Get index from observations with wrong value in lat variable
idx.lat.wrong = which(str_detect(fires.raw$lat, '1900-01-01'))

head(fires.raw[idx.lat.wrong,])

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
df_status(fires.raw$lat)
idx.lat.na = which(is.na(fires.raw$lat))
fires.raw[idx.lat.na,]

#Convert latitude and longitue in decimal format
convert_lat_lon <- function (value){
  vec_clean <- c("''"="", "E-12"="", "E-02"="", ","= ".", "º"=" ", "'"=" ", ":"=" ")
  vec <- unlist(str_split(str_replace_all(value, vec_clean), " "))
  x <-  str_c(vec[1], vec[2], vec[3], sep = " " )
  res <- conv_unit(x, "deg_min_sec", "dec_deg")
  return(res)
}

#save(fires.raw, file = "fires.raw.RData")
load("fires.raw.RData")

df_status(fires.raw$lat)
df_status(fires.raw$lon)

vec_clean <- c("''"="", "E-12"="", "E-02"="", ","=".", "º"=" ", "'"=" ", ":"=" ")
fires.raw$lat <- str_replace_all(fires.raw$lat, vec_clean)
fires.raw$lon <- str_replace_all(fires.raw$lon, vec_clean)
fires.raw$lat <- str_squish(fires.raw$lat)
fires.raw$lon <- str_squish(fires.raw$lon)
fires.raw$lat <- conv_unit(fires.raw$lat, "deg_min_sec", "dec_deg")
# Ocorre um warning ao executar essa linha
fires.raw$lon <- conv_unit(fires.raw$lon, "deg_min_sec", "dec_deg")

# Workaround
fires.raw$lon[7511] <- conv_unit("8 34 21.5868000000013", "deg_min_sec", "dec_deg")

df_status(fires.raw$lat)
df_status(fires.raw$lon)

idx.lon.zero = which(fires.raw$lon == "0")
zero.lat = fires.raw[idx.lon.zero,]



str_replace(str_trim("8:34:21.5868000000013''	"), "''", "")

idx.lat.na = which(is.na(fires.raw$lat))
fires.raw[idx.lat.na,]


fires.raw <- fires.raw %>%  rowwise() %>% mutate(lat_dec = convert_lat_lon(lat)) 
df_status(fires.raw$lat_dec)

idx.lat_dec.na = which(is.na(fires.raw$lat_dec))
na.lat_dec = fires.raw[idx.lat_dec.na,]

df_status(fires.raw$lat)

idx.lat.zero = which(fires.raw$lon == "0")
zero.lat = fires.raw[idx.lat.zero,]

"39º26'27.9060000000038''"


conv_unit("8 34 21.5868000000013", "deg_min_sec", "dec_deg")



lat.raw = "14:57:01"
lon.raw = "8 34 21.5868000000013"
convert_lat_lon(lat.raw)
convert_lat_lon(lon.raw)



# Fix data type 
fires.raw$region = as.factor(fires.raw$region)
fires.raw$district = as.factor(fires.raw$district)
fires.raw$municipality = as.factor(fires.raw$municipality)
fires.raw$parish = as.factor(fires.raw$parish)
fires.raw$origin = as.factor(fires.raw$origin)
fires.raw$cause_type = as.factor(fires.raw$cause_type)

date_time_format = c(dates="y-m-d", times="h:m:s")

fires.raw$lat = fires.raw$lat %>% str_replace_all(latin_character_map)

fires.raw <- fires.raw %>% mutate(alert = chron(fires.raw$alert_date, fires.raw$alert_hour, format = date_time_format),
                                  extinction = chron(fires.raw$extinction_date, fires.raw$extinction_hour, format = date_time_format),
                                  firstInterv = chron(fires.raw$firstInterv_date, fires.raw$firstInterv_hour, format = date_time_format))



df_status(fires.raw$alert)
df_status(fires.raw$extinction)
df_status(fires.raw$firstInterv)

fires.raw %>% select(extinction_date, extinction_hour, extinction, firstInterv) %>% filter(is.na(extinction))
fires.raw %>% select(alert_date, alert_hour, alert) %>% filter(is.na(alert))
fires.raw %>% select(firstInterv_date, firstInterv_hour, extinction, firstInterv) %>% filter(is.na(firstInterv))




fires.raw %>% mutate(alert_datetime = cro)


#Cleanind data
#latin_character_map = c("Ã§" = "ç", "Ã£" = "ã", "Ã¢" = "â", "Ã©" = "é", "Ã³" = "ó", "Ã¡" = "á", "Ãº" = "ú", "Ã´" = "ô", "Ãª" = "ê", "Ã\u008d" = "Í", "Ãµ" = "õ", "Ã‰" = "É", "Ã" = "Á", "à€" = "à","Ã" = "à","à“" = "Ó","à‚" = "Â","à" = "Á")
#fires.raw$region = fires.raw$region %>% str_replace_all(latin_character_map)
#fires.raw$district = fires.raw$district %>% str_replace_all(latin_character_map)
#fires.raw$municipality = fires.raw$municipality %>% str_replace_all(latin_character_map)
#fires.raw$parish = fires.raw$parish %>% str_replace_all(latin_character_map)
