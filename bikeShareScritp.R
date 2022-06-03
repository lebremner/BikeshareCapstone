#Load Packages----
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(lubridate)

#Read in data----

jan_21 <- read_csv(here("data", "202101-divvy-tripdata.csv"))
feb_21 <- read_csv(here("data", "202102-divvy-tripdata.csv"))
mar_21 <- read_csv(here("data", "202103-divvy-tripdata.csv"))
april_21 <- read.csv(here("data", "202104-divvy-tripdata.csv"))
may_21 <- read_csv(here("data", "202105-divvy-tripdata.csv"))
june_21 <- read_csv(here("data", "202106-divvy-tripdata.csv"))
july_21 <- read_csv(here("data", "202107-divvy-tripdata.csv"))
aug_21 <- read_csv(here("data", "202108-divvy-tripdata.csv"))
sept_21 <- read_csv(here("data", "202109-divvy-tripdata.csv"))
oct_21 <- read_csv(here("data", "202110-divvy-tripdata.csv"))
nov_21 <- read_csv(here("data", "202111-divvy-tripdata.csv"))
dec_21 <- read_csv(here("data", "202112-divvy-tripdata.csv"))


#weather data
wjan_21 <- read_csv(here("data", "weather_jan21.csv"))
wfeb_21 <- read_csv(here("data", "weather_feb21.csv"))
wmarch_21 <- read_csv(here("data", "weather_march21.csv"))
wapril_21 <- read_csv(here("data", "weather_april21.csv"))
wmay_21 <- read_csv(here("data", "weather_may21.csv"))
wjune_21 <- read_csv(here("data", "weather_june21.csv"))
wjuly_21 <- read_csv(here("data", "weather_july21.csv"))
waug_21 <- read_csv(here("data", "weather_aug21.csv"))
wsep_21 <- read_csv(here("data", "weather_sep21.csv"))
woct_21 <- read_csv(here("data", "weather_oct21.csv"))
wnov_21 <- read_csv(here("data", "weather_nov21.csv"))
wdec_21 <- read_csv(here("data", "weather_dec21.csv"))










#read in cleaned data
full_year <- read_csv(here("data", "full_year.csv"))
count_weather <- read_csv(here("data", "count_weather.csv"))


#Cleaning----
#merging data frames vertically
full_year <- rbind(jan_21, feb_21, mar_21, april_21, may_21, june_21, july_21, aug_21, sept_21, oct_21, nov_21, dec_21)

weather <- rbind(wjan_21, wfeb_21, wmarch_21, wapril_21, wmay_21, wjune_21, wjuly_21, waug_21, wsep_21, woct_21, wnov_21, wdec_21)


#checking new dataframe
head(full_year)
tail(full_year)

head(weather)
tail(weather)


#remove na and blank values in start_station_column
full_year[!(is.na(full_year$start_station_name) | full_year$start_station_name==""), ]

#split date/time, add day of week column
full_year$start_time <- format(as.POSIXct(full_year$started_at),format = "%H:%M:%S")
full_year$start_date <- as.Date (full_year$started_at)

full_year$end_time <- format(as.POSIXct(full_year$ended_at),format = "%H:%M:%S")
full_year$end_date <- as.Date (full_year$ended_at)

full_year$day <- strftime(full_year$start_date, "%A")

#checking split
colnames(full_year)


#calclulate trip duration, select appropriate column for analysis
full_year <- full_year %>% 
  mutate(trip_duration = difftime(ended_at, started_at, units = "min")) %>% 
  select(rideable_type, member_casual,start_station_name, start_lat, start_lng, start_date, start_time, day, trip_duration)

avg_feel <- weather %>% 
  select(date, avg_feel)

#check updated frames
head(full_year)

#Make data more user/reader friendly
full_year$rideable_type<-replace(full_year$rideable_type,full_year$rideable_type=="classic_bike","Classic")
full_year$rideable_type<-replace(full_year$rideable_type,full_year$rideable_type=="docked_bike","Docked")
full_year$rideable_type<-replace(full_year$rideable_type,full_year$rideable_type=="electric_bike","Electric")
full_year$member_casual<-replace(full_year$member_casual,full_year$member_casual=="member","Member")
full_year$member_casual<-replace(full_year$member_casual,full_year$member_casual=="casual","Casual")


#write new csv files
write_csv (full_year,here("data", "full_year.csv"))
write_csv (avg_feel, here("data", "avg_feel21.csv"))
#compare membership types seasonality----

#count membership types, average trip duration by membership type
full_year %>% 
  group_by(member_casual) %>% 
  count(member_casual)




full_year %>% 
  group_by(member_casual) %>% 
  summarise(mean_duration = mean(trip_duration [trip_duration < 1440.0]))


#Comparison statistics
#Shout out to Caitlin who helped me with this code, specifically mean under 24 hours
# Member rides over 24 hr = 341. Casual rides over 24hr = 2541. Total is 2882 for the whole year. This represents 0.83% of all observations 



 full_year %>% 
  group_by(member_casual) %>% 
  summarise(median_duration = median(trip_duration, na.rm = TRUE),
            max_duration = max(trip_duration, na.rm = TRUE), 
            mean_duration = mean(trip_duration [trip_duration < 1440.0]))

   

 #Mean trip duration by date, shows seasonality and splits out by membership types----
 full_year_date <- full_year %>% 
   group_by(start_date, member_casual) %>% 
   summarise(meanduration = mean(trip_duration [trip_duration < 1440.0]),
             medianduration = median(trip_duration, na.rm = TRUE))
write.csv(full_year_date, here("data", "full_year_date.csv")) 

#Count by date, sorted by membership type----
full_year_count <- full_year %>% 
  group_by(start_date, member_casual) %>% 
  count(start_date)

write.csv(full_year_count, here("data", "full_year_count.csv"))


  

#Count by rides left from start station, sorted by membership type----
#Caitlin made this section work too

popular_stations_member <- full_year %>%
  na.omit() %>%    # I removed the select function as it is not needed, with the other functions within this pipe, your data will automatically be filtered down to just the columns of interest
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "Member") %>%
  summarise(n = n(), start_lat = mean(start_lat), start_lng = mean(start_lng)) %>%  #using the summarise function instead of count as this allows us to apply logic to the start_lat and start_lng columns. the n = n() portion of the function is creating a new column "n" which contains a count of all of the unique combinations of the entries for the columns we are grouping by. in this case, member_casual has only one value, so it is in practice counting the unique entries for start_station_name. because we are using the summarize function, the output only includes one entry/row for each unique combination of the columns we are grouping by.
  arrange(desc(n))  #sort the data based on the new count

popular_stations_casual <- full_year %>%
  na.omit() %>%    
  group_by(member_casual, start_station_name) %>%
  filter(member_casual == "Casual") %>%
  summarise(n = n(), start_lat = mean(start_lat), start_lng = mean(start_lng)) %>%  
  arrange(desc(n))


  

#Top 20 files 
member_top_20 <- head(arrange(popular_stations_member, desc(n)), n = 20)
casual_top_20 <- head(arrange(popular_stations_casual, desc(n)), n = 20)

combined_top_20 <- rbind(member_top_20, casual_top_20)
write.csv(combined_top_20, here("data", "combined_top_20.csv"))


#Count rideable type by member_casual

bike_type_member <- full_year %>% 
  na.omit() %>% 
  group_by(member_casual, rideable_type) %>% 
  count(rideable_type)

#Day of week analysis (I made this work all by myself!!!)

day_of_week <- full_year %>% 
  group_by(member_casual, day) %>% 
  summarise(count_day = n()) 
  

write_csv(day_of_week, here("data", "day_of_week.csv"))
write.csv(bike_type_member, here("data", "bike_type_member.csv"))


