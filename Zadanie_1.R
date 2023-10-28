library(rnoaa)
library(dplyr)
library(readr)
library(lubridate)
library(tidyverse)

#Трифонова Алёна Александровна — для региона 16 рассчитайте урожайность пшеницы в 2003 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с метеостанций на расстоянии от 70 до 210 км 

station_data = ghcnd_stations()
write.csv(station_data, file="station_data.csv")
station_data = read.csv("station_data.csv")
#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
kazan = data.frame(id = "KAZAN", latitude = 55.78487,  longitude = 49.12360)

kazan_around = meteo_nearby_stations(lat_lon_df = kazan, station_data = station_data,
                                    var = c("PRCP", "TAVG"),
                                    year_min = 1994, year_max = 2003,  radius = 210)
# Проблема
kazan_around = kazan_around %>% filter(distance>70)

kazan_id = kazan_around[["KAZAN"]][["id"]][1]
#Для получения всех данных с метеостанции, зная ее идентификатор, используйте след. команду
all_kazan_data = meteo_tidy_ghcnd(stationid = kazan_id)

all_kazan_data=all_kazan_data %>% mutate(tmax= tmax/10,
                     tmin= tmin/10,
                     tavg= tavg/10,
                     prcp= prcp/10)

class(all_kazan_data$date)
month(all_kazan_data$date)
day(all_kazan_data$date)
year(all_kazan_data$date)
yday(all_kazan_data$date)

all_kazan_data=all_kazan_data %>% mutate(year = year(date),
                     month =month(date),
                     day = day(date),
                     doy = yday(date))

clim = all_kazan_data %>% group_by(id,year,month) %>%
  summarise(tavg = mean(tavg, na.rm = T))

all_kazan_data=all_kazan_data %>% filter(year > 1993 & year < 2003)
all_kazan_data=all_kazan_data %>% select (-snwd)
all_kazan_data=all_kazan_data %>% select(-prcp)


sum(is.na(all_kazan_data$tavg))

all_kazan_data= all_kazan_data %>% mutate(tavg_f= case_when(
  is.na(tavg) ~ (tmin+tmax)/2,
  TRUE ~ tavg))

all_kazan_data= all_kazan_data %>% mutate(tavg_a= case_when(
  tavg_f < 5 ~ 0,
  TRUE ~ tavg_f))

active = all_kazan_data %>% group_by(id,year,month) %>%
  summarise(sum_a= sum(tavg_a, na.rm =T))

# Зададим переменные

m=c(1:12);m
afi= c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73,16.30,13.83,0.00,0.00); afi
bfi= c(0.00, 0.00, 0.00, 11.30,9.26,9.03,8.16,6.59,5.73,4.87,0.00,0.00);bfi
di= c(0.00, 0.00, 0.00,0.33,1,1,1,0.32,0.00, 0.00, 0.00,0.00); di

Fi= afi + bfi*di*active$sum_a

Yi=10^6*sum((Fi*di)*300/1600*2.2*(100-25))
