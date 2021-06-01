library(dplyr)

### Step 1: Load & Manipulate Data
# Load "seoul_subway_by_time.csv"
seoul_subway_by_time <- read.csv("/Users/phillip/Desktop/CS492_R/RProject/seoul_subway_by_time.csv", header = T, fileEncoding = "euc-kr")

seoul_subway_by_time <- seoul_subway_by_time %>%
  mutate("sum_04_05" = X04시.05시.승차인원 + X04시.05시.하차인원,
         "sum_05_06" = X05시.06시.승차인원 + X05시.06시.하차인원,
         "sum_06_07" = X06시.07시.승차인원 + X06시.07시.하차인원,
         "sum_07_08" = X07시.08시.승차인원 + X07시.08시.하차인원,
         "sum_08_09" = X08시.09시.승차인원 + X08시.09시.하차인원,
         "sum_09_10" = X09시.10시.승차인원 + X09시.10시.하차인원,
         "sum_10_11" = X10시.11시.승차인원 + X10시.11시.하차인원,
         "sum_11_12" = X11시.12시.승차인원 + X11시.12시.하차인원,
         "sum_12_13" = X12시.13시.승차인원 + X12시.13시.하차인원,
         "sum_13_14" = X13시.14시.승차인원 + X13시.14시.하차인원,
         "sum_14_15" = X14시.15시.승차인원 + X14시.15시.하차인원,
         "sum_15_16" = X15시.16시.승차인원 + X15시.16시.하차인원,
         "sum_16_17" = X16시.17시.승차인원 + X16시.17시.하차인원,
         "sum_17_18" = X17시.18시.승차인원 + X17시.18시.하차인원,
         "sum_18_19" = X18시.19시.승차인원 + X18시.19시.하차인원,
         "sum_19_20" = X19시.20시.승차인원 + X19시.20시.하차인원,
         "sum_20_21" = X20시.21시.승차인원 + X20시.21시.하차인원,
         "sum_21_22" = X21시.22시.승차인원 + X21시.22시.하차인원,
         "sum_22_23" = X22시.23시.승차인원 + X22시.23시.하차인원,
         "sum_23_24" = X23시.24시.승차인원 + X23시.24시.하차인원,
         "sum_00_01" = X00시.01시.승차인원 + X00시.01시.하차인원,
         "sum_01_02" = X01시.02시.승차인원 + X01시.02시.하차인원,
         "sum_02_03" = X02시.03시.승차인원 + X02시.03시.하차인원,
         "sum_03_04" = X03시.04시.승차인원 + X03시.04시.하차인원) %>%
  select("사용월", "호선명", "지하철역",
         "sum_04_05", "sum_05_06", "sum_06_07", "sum_07_08", "sum_08_09", "sum_09_10",
         "sum_10_11", "sum_11_12", "sum_12_13", "sum_13_14", "sum_14_15", "sum_15_16",
         "sum_16_17", "sum_17_18", "sum_18_19", "sum_19_20", "sum_20_21", "sum_21_22",
         "sum_22_23", "sum_23_24", "sum_00_01", "sum_01_02", "sum_02_03", "sum_03_04"
         )

# Get 2019 data (201901 <= 사용월 < 202001) 
seoul_subway_2019 <- seoul_subway_by_time %>%
  filter(사용월 >= 201901, 사용월 < 202001)

# Get 2020 data (202001 <= 사용월 < 202101) 
seoul_subway_2020 <- seoul_subway_by_time %>%
  filter(사용월 >= 202001, 사용월 < 202101)

# Group by (지하철역, 사용월) -> Get data by month
seoul_subway_2019_by_month <- seoul_subway_2019 %>%
  group_by(지하철역, 사용월) %>%
  summarise(sum(sum_04_05), sum(sum_05_06), sum(sum_06_07), sum(sum_07_08), sum(sum_08_09), sum(sum_09_10),
            sum(sum_10_11), sum(sum_11_12), sum(sum_12_13), sum(sum_13_14), sum(sum_14_15), sum(sum_15_16),
            sum(sum_16_17), sum(sum_17_18), sum(sum_18_19), sum(sum_19_20), sum(sum_20_21), sum(sum_21_22),
            sum(sum_22_23), sum(sum_23_24), sum(sum_00_01), sum(sum_01_02), sum(sum_02_03), sum(sum_03_04))

seoul_subway_2020_by_month <- seoul_subway_2020 %>%
  group_by(지하철역, 사용월) %>%
  summarise(sum(sum_04_05), sum(sum_05_06), sum(sum_06_07), sum(sum_07_08), sum(sum_08_09), sum(sum_09_10),
            sum(sum_10_11), sum(sum_11_12), sum(sum_12_13), sum(sum_13_14), sum(sum_14_15), sum(sum_15_16),
            sum(sum_16_17), sum(sum_17_18), sum(sum_18_19), sum(sum_19_20), sum(sum_20_21), sum(sum_21_22),
            sum(sum_22_23), sum(sum_23_24), sum(sum_00_01), sum(sum_01_02), sum(sum_02_03), sum(sum_03_04))

# View(seoul_subway_2019_by_month)
# View(seoul_subway_2020_by_month)

# Group by 지하철역 -> Get data for whole year
seoul_subway_2019_total <- seoul_subway_2019 %>%
  group_by(지하철역) %>%
  summarise(sum(sum_04_05), sum(sum_05_06), sum(sum_06_07), sum(sum_07_08), sum(sum_08_09), sum(sum_09_10),
            sum(sum_10_11), sum(sum_11_12), sum(sum_12_13), sum(sum_13_14), sum(sum_14_15), sum(sum_15_16),
            sum(sum_16_17), sum(sum_17_18), sum(sum_18_19), sum(sum_19_20), sum(sum_20_21), sum(sum_21_22),
            sum(sum_22_23), sum(sum_23_24), sum(sum_00_01), sum(sum_01_02), sum(sum_02_03), sum(sum_03_04))

seoul_subway_2020_total <- seoul_subway_2020 %>%
  filter(지하철역 %in% seoul_subway_2019_total$지하철역) %>%
  group_by(지하철역) %>%
  summarise(sum(sum_04_05), sum(sum_05_06), sum(sum_06_07), sum(sum_07_08), sum(sum_08_09), sum(sum_09_10),
            sum(sum_10_11), sum(sum_11_12), sum(sum_12_13), sum(sum_13_14), sum(sum_14_15), sum(sum_15_16),
            sum(sum_16_17), sum(sum_17_18), sum(sum_18_19), sum(sum_19_20), sum(sum_20_21), sum(sum_21_22),
            sum(sum_22_23), sum(sum_23_24), sum(sum_00_01), sum(sum_01_02), sum(sum_02_03), sum(sum_03_04))

# View(seoul_subway_2019_total)
# View(seoul_subway_2020_total)

### Step 2: Get the most crowded time period for each station



### Step 3: 




