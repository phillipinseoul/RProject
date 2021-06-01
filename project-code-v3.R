library(lubridate)
library(dplyr)
library(ggmap)
library(ggplot2)
library(raster)
library(rgeos)
library(maptools)
library(rgdal)
library(readxl)
library(tidyverse)
library(gridExtra)
library(qdap)

# setwd("/Users/phillip/Desktop/CS492_R/RProject")
setwd("/home/yuseung/Desktop/RProject")

# Load subway dataset
subway <- read.csv("seoul_subway_by_time.csv", header = T, fileEncoding = "euc-kr")

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# Get 2019, 2020 data
subway2019 <- subway %>%
  filter(month >= 201901, month < 202001)

subway2020 <- subway %>%
  filter(month >= 202001, month < 202101)

# Add period (time7 ~ time9, time17 ~ time19)
data2019 <- subway2019 %>%
  mutate(period1_2019 = Time07_1 + Time07_2 + Time08_1 + Time08_2 + Time09_1 + Time09_2, period2_2019 = Time17_1 + Time17_2 + Time18_1 + Time18_2 + Time19_1 + Time19_2) %>%
  group_by(station) %>%
  summarise(period1_2019 = sum(period1_2019), period2_2019 = sum(period2_2019)) %>%
  dplyr::select(station, period1_2019, period2_2019) 

data2020 <- subway2020 %>%
  mutate(period1_2020 = Time07_1 + Time07_2 + Time08_1 + Time08_2 + Time09_1 + Time09_2, period2_2020 = Time17_1 + Time17_2 + Time18_1 + Time18_2 + Time19_1 + Time19_2) %>%
  group_by(station) %>%
  summarise(period1_2020 = sum(period1_2020), period2_2020 = sum(period2_2020)) %>%
  dplyr::select(station, period1_2020, period2_2020) 

period_data <- merge(data2019, data2020, by="station")

period_data <- period_data %>%
  mutate(rate1 = period1_2020 / period1_2019,
         rate2 = period2_2020 / period2_2019)


# Add types
period_data$type_v1 <- ifelse(period_data$rate1 >= 0.9, "Type 1: Over 0.9", ifelse(
  period_data$rate1 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    period_data$rate1 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )
))

period_data$type_v2 <- ifelse(period_data$rate2 >= 0.9, "Type 1: Over 0.9", ifelse(
  period_data$rate2 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    period_data$rate2 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )
))

period_data$station <- as.character(period_data$station)

period_data$station <- strsplit(period_data$station, split = "")[[1]]

write.csv(period_data_final, "period_data.csv", fileEncoding = "euc-kr")



# Map
P <- read.csv("sample.csv", header = TRUE, fileEncoding = "euc-kr")
map <- shapefile("TL_SCCO_SIG.shp")
map <- spTransform(map, CRSobj = CRS('+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs'))
new_map <- fortify(map, region = 'SIG_CD')
View(new_map)

theme_set(theme_gray(base_family='NanumGothic'))

new_map$id <- as.numeric(new_map$id)
seoul_map <- new_map[new_map$id <= 11740,]
P_merge <- merge(seoul_map, P, by='id')

station_coordinate <- read.csv("station_coordinate.csv", header = T)

station_coordinate_v2 <- station_coordinate %>%
  filter(lat < 37.71, lat > 37.4, lng > 126.75, lng < 127.2) %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  dplyr::select(name, lat, lng) %>%
  rename(station = name)

subwaydata <- merge(station_coordinate_v2, period_data, by="station")

# plot period1
ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=type_v1), size=4) +
  scale_color_manual(values=c("#ffd6a1", "#ff9595", "#d60a24", "#690c22"))

# plot period2
ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=type_v2), size=4) +
  scale_color_manual(values=c("#ffd6a1", "#ff9595", "#d60a24", "#690c22"))




















