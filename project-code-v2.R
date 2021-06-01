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

# setwd("/Users/phillip/Desktop/CS492_R/RProject")
setwd("/home/yuseung/Desktop/RProject")


# Load subway dataset
subway2019 <- read.csv("2019_subway.csv", header = T)
subway2020 <- read.csv("2020_subway.csv", header = T, fileEncoding = "euc-kr")

weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")

# 출근시간 (06-09)
Work2019 <- subway2019 %>%
  mutate(toWork2019 = time6 + time7 + time8, fromWork2019 = time17 + time18 + time19) %>%
  filter(is.element(weekdays(as.Date(ymd(date))), weekday)) %>%
  group_by(station) %>%
  summarise(toWork2019 = sum(toWork2019), fromWork2019 = sum(fromWork2019))

Work2020 <- subway2020 %>%
  mutate(toWork2020 = time6 + time7 + time8, fromWork2020 = time17 + time18 + time19) %>%
  filter(is.element(weekdays(as.Date(ymd(date))), weekday)) %>%
  group_by(station) %>%
  summarise(toWork2020 = sum(toWork2020), fromWork2020 = sum(fromWork2020))

# Weekend: All day
weekend2019 <- subway2019 %>%
  mutate(allday = before6 + time6 + time7 + time8 + time9 + time10 + time11 + time12 +
           time13 + time14 + time15 + time16 + time17 + time18 + time19 + time20 + time21 +
           time22 + time23 + after24) %>%
  dplyr::select(date, line, station, allday) %>%
  filter(is.element(weekdays(as.Date(ymd(date))), weekday) == FALSE) %>%
  group_by(station) %>%
  summarise(weekend2019 = sum(allday))

# Period (time7 ~ time9, time17 ~ time19)
data2019 <- subway2019 %>%
  mutate(period1_2019 = time7 + time8 + time9, period2_2019 = time17 + time18 + time19) %>%
  group_by(station) %>%
  summarise(period1_2019 = sum(period1_2019), period2_2019 = sum(period2_2019)) %>%
  dplyr::select(station, period1_2019, period2_2019) 

data2020 <- subway2020 %>%
  mutate(period1_2020 = time7 + time8 + time9, period2_2020 = time17 + time18 + time19) %>%
  group_by(station) %>%
  summarise(period1_2020 = sum(period1_2020), period2_2020 = sum(period2_2020)) %>%
  dplyr::select(station, period1_2020, period2_2020) 

period_data <- merge(data2019, data2020, by="station")
period_data_final <- period_data %>%
  mutate(period1 = period1_2020 / period1_2019,
         period2 = period2_2020 / period2_2019)

period_data_final$periodType1 <- ifelse(period_data_final$period1 >= 0.9, "Type 1: Over 0.9", ifelse(
  period_data_final$period1 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    period_data_final$period1 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )
))

period_data_final$periodType2 <- ifelse(period_data_final$period2 >= 0.9, "Type 1: Over 0.9", ifelse(
  period_data_final$period2 >= 0.8, "Type 2: 0.8 to 0.9", ifelse(
    period_data_final$period2 >= 0.7, "Type 3: 0.7 to 0.8", "Type 4: Under 0.7"
  )
))

write.csv(period_data_final, "period_data.csv", fileEncoding = "euc-kr")




weekend2020 <- subway2020 %>%
  mutate(allday = before6 + time6 + time7 + time8 + time9 + time10 + time11 + time12 +
           time13 + time14 + time15 + time16 + time17 + time18 + time19 + time20 + time21 +
           time22 + time23 + after24) %>%
  dplyr::select(date, line, station, allday) %>%
  filter(is.element(weekdays(as.Date(ymd(date))), weekday) == FALSE) %>%
  group_by(station) %>%
  summarise(weekend2020 = sum(allday))
  

# Merge all weekday data
weekendSum <- merge(weekend2019, weekend2020, by="station")
temp <- merge(Work2019, Work2020, by="station") 
finaldata <- merge(temp, weekendSum, by="station")

# Change rate 2019 to 2020
ratedata <- finaldata %>%
  mutate(toWorkRate = toWork2020 / toWork2019,
         fromWorkRate = fromWork2020 / fromWork2019,
         totalWorkRate = (toWork2020 + fromWork2020) / (toWork2019 + fromWork2019),
         weekendRate = weekend2020 / weekend2019) %>%
        dplyr::select(station, toWorkRate, fromWorkRate, totalWorkRate, weekendRate)

ratedata$weekdayType <- ifelse(ratedata$totalWorkRate < 0.7, "< 0.7", ifelse(
                              ratedata$totalWorkRate > 0.9, "> 0.9", "Between"))
ratedata$weekendType <- ifelse(ratedata$weekendRate < 0.7, "< 0.7 ", ifelse(
                              ratedata$weekendRate > 0.9, "> 0.9", "Between"))

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

View(P_merge)

station_coordinate <- read.csv("station_coordinate.csv", header = T)

station_coordinate_final <- station_coordinate %>%
  filter(lat < 37.71, lat > 37.4, lng > 126.75, lng < 127.2) %>%
  filter(line == "01호선" | line == "02호선" | line == "03호선" |line == "04호선" | line == "05호선" |
      line == "06호선" | line == "07호선" | line == "08호선") %>%
  group_by(name) %>%
  filter(row_number() == 1) %>%
  dplyr::select(name, lat, lng) %>%
  rename(station = name)

subwaydata <- merge(station_coordinate_final, ratedata, by="station")

write.csv(subwaydata, "subway_data_final.csv")

#ggplot() +
#  geom_point(data=station_coordinate, aes(x=lng, y=lat)) +
#  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black')

period1_type1 <- period_data_final %>%
  filter(periodType1 == "Over 0.9")
period1_type2 <- period_data_final %>%
  filter(periodType1 == "0.8 to 0.9")
period1_type3 <- period_data_final %>%
  filter(periodType1 == "0.7 to 0.8")
period1_type4 <- period_data_final %>%
  filter(periodType1 == "Under 0.7")

#subwaydata_type2 <- subwaydata %>%
#  filter(weekdayType == "Between")

#subwaydata_type3 <- subwaydata %>%
#  filter(weekdayType == "> 0.9")

period_data_final <- merge(station_coordinate_final, period_data_final, by="station")

ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=period_data_final, aes(x=lng, y=lat, color=periodType1), size=2) +
  scale_color_manual(values=c("#fc0303", "#fca103", "#8cfc03", "#03c2fc"))




ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2) +
  scale_color_manual(values=c("#ff0000", "#008cff", "#000000"))


ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2) +
  stat_density2d(data=subwaydata_type1, aes(x=lng, y=lat, color=weekdayType)) +
  scale_color_manual(values=c("#ff0000", "#008cff", "#000000"))

ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2) +
  stat_density2d(data=subwaydata_type2, aes(x=lng, y=lat, color=weekdayType)) +
  scale_color_manual(values=c("#ff0000", "#008cff", "#000000"))

ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2) +
  stat_density2d(data=subwaydata_type3, aes(x=lng, y=lat, color=weekdayType)) +
  scale_color_manual(values=c("#ff0000", "#008cff", "#000000"))

ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group, fill=A)) +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2)

# Period1 plot
ggplot() +
  geom_polygon(data = P_merge, aes(x=long, y=lat, group=group), fill = 'white', color='black') +
  geom_point(data=subwaydata, aes(x=lng, y=lat, color=weekdayType), size=2) +
  scale_color_manual(values=c("#ff0000", "#008cff", "#000000"))



View(station_coordinate)





