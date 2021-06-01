library(readr)
library(ggplot2)
library(dplyr)

# Dataset 1: COVID19 Patients in Seoul
# http://data.seoul.go.kr/dataList/OA-20279/S/1/datasetView.do
covid19_seoul <- read.csv("covid_seoul.csv", header = TRUE, fileEncoding = "euc-kr")

View(covid19_seoul)
str(covid19_seoul)
summary(covid19_seoul)

# Number of patients by area
# par(family  = "AppleGothic")
theme_set(theme_gray(base_family='NanumGothic'))
qplot(covid19_seoul$지역)

# Make "확진일" into Date type
covid19_seoul$확진일 <- as.Date(covid19_seoul$확진일, format = "%Y.%m.%d")
str(covid19_seoul)


covid19_seoul_total <- covid19_seoul %>%
  filter(확진일 < "2021-01-01", 확진일 > "2020-04-30") %>%
  select("연번", "확진일", "지역")

View(covid19_seoul_total)

table(covid19_seoul_total$확진일)
plot(table(covid19_seoul_total$확진일), type = "l")


covid19_seoul_seocho <- covid19_seoul %>%
  filter(확진일 < "2021-01-01", 확진일 > "2020-04-30") %>%
  filter(지역 == "서초구") %>%
  select("연번", "확진일", "지역")

View(covid19_seoul_seocho)

table(covid19_seoul_seocho$확진일)
plot(table(covid19_seoul_seocho$확진일), type = "l")

table(covid19_seoul$확진일)
plot(table(covid19_seoul$확진일))


### Load Seoul metro dataset ###

#temp <- read.csv(paste(metro.dir, "CARD_SUBWAY_MONTH_202009.csv", sep="/"),
#               head = T, fileEncoding = "euc-kr")

temp <- read_csv(paste(metro.dir, "CARD_SUBWAY_MONTH_202009.csv", sep="/"), locale=locale('ko',encoding='euc-kr'))
View(temp)

# temp <- read_csv(paste(metro.dir, data, sep="/"))
# View(temp)

metro.dir <- "/Users/phillip/Desktop/CS492_R/RProject/metrodata"
data.list <- list.files(metro.dir)
data.list
'''
# May - Dec, 2020
metro.data <- data.frame()
for(data in data.list) {
  print(data)
  temp <- read_csv(paste(metro.dir, data, sep="/"), locale=locale('ko',encoding='euc-kr'))
  
#  temp <- temp %>%
#    mutate(id = paste(temp$사용일자, temp$노선명, temp$역명, sep="_"))
  
  #temp <- read.csv(paste(metro.dir, data, sep="/"), 
  #                row.names = NULL, header = TRUE, fileEncoding = "euc-kr")
  
  temp <- temp %>%
    select("사용일자", "노선명", "역명", "승차총승객수", "하차총승객수")
  
  # Change "사용일자" into Date type
  temp$사용일자 <- as.Date(as.character(temp$사용일자), format = "%Y%m%d")
  # Change "역명" into character type
  temp$역명 <- as.character(temp$역명)
  # metro.data <- rbind(metro.data, temp)
  metro.data <- bind_rows(metro.data, temp)
}'''

metro.05 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202005.csv")
str(metro.05)
metro.06 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202006.csv")
str(metro.06)
metro.07 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202007.csv")
str(metro.07)
metro.08 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202008.csv")
str(metro.08)
metro.09 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202009.csv", locale=locale('ko',encoding='euc-kr'))
str(metro.09)
metro.10 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202010.csv")
str(metro.10)
metro.11 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202011.csv")
str(metro.11)
metro.12 <- read_csv("metrodata/CARD_SUBWAY_MONTH_202012.csv")
str(metro.12)

metro.data <- bind_rows(metro.05, metro.06, metro.07, metro.08, metro.09, metro.10,
                        metro.11, metro.12)
View(metro.data)

metro.data <- metro.data %>%
  mutate(total_users = metro.data$승차총승객수 + metro.data$하차총승객수)
View(metro.data)

metro.data$사용일자 <- as.Date(as.character(metro.data$사용일자), format = "%Y%m%d")
View(metro.data)

metro.data.Gangnam <- metro.data %>%
  filter(역명 == "강남") %>%
  select("사용일자", "역명", "total_users")
View(metro.data.Gangnam)

ggplot(data = metro.data.Gangnam, aes(x = 사용일자, y = total_users)) +
  geom_line() +
  geom_smooth() +
  labs(x = "Date",
       y = "Metro Users",
       title = "Metro User Data (May-Dec, 2020)")




# Add new column: total_users
metro.11.v2 <- metro.11 %>%
  mutate(total_users = metro.11$승차총승객수 + metro.11$하차총승객수)
# Change 사용일자 into Date type
metro.11.v2$사용일자 <- as.Date(as.character(metro.11.v2$사용일자), format = "%Y%m%d")


View(metro.11.v2)

# Filter: 역명 = 강남
metro.11.Gangnam <- metro.11.v2 %>%
  filter(역명 == "강남")
View(metro.11.Gangnam)

qplot(data = metro.11.Gangnam, 사용일자, total_users, geom = "line")





