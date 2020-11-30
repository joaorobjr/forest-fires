library(dplyr)

d <- read.csv('data/fires2015_train.csv')
dim(d)
summary(d$alert_source)
glimpse(d)

select(d, district = 'viseu')
help("filter")

d %>% filter(district=='Viseu')

load("data/station_data.RData")
