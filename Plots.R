#Load Packages----
library(tidyverse)
library(here)
library(skimr)
library(janitor)
library(ggbeeswarm)
library(ggplot2)
library(RColorBrewer)

#Read in Data----
full_year <- read_csv(here("data", "full_year.csv"))
year_compare <- read_csv(here("data", "year_compare.csv"))
full_year_date <- read_csv(here("data", "full_year_date.csv"))
full_year_count <- read_csv(here("data", "full_year_count.csv"))
bike_type_member <- read_csv(here("data", "bike_type_member.csv"))
day_of_week <- read_csv(here("data", "day_of_week.csv"))
count_weather <- read_csv(here("data", "count_weather.csv"))

#Set Theme
theme_set(theme_bw())

#Graph of rideable types


bike_type_member %>% 
  group_by(member_casual, rideable_type) %>% 
  ggplot(aes(x = rideable_type, y = n, fill = member_casual)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_fill_brewer(palette = "Set2", name = "Rider Type", labels = c("Casual", "Member"))+
  labs(title = "Comparison of Bicycle Use by Type", 
       subtitle = "January 2021 - December 2021",
       x = "Bicycle Type",
       y = "Number of Rides",
       caption = "Data obtained from https://divvy-tripdata.s3.amazonaws.com/index.html")+
  theme(plot.title =element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
        plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 2),
        axis.title.x = element_text(face = "bold", vjust = -1.5),
        axis.title.y = element_text(face = "bold"),
        plot.margin = unit(c(2.5,2,2,2), "cm"))
 
#Plot of Bike Use by Day of the Week
day_order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

day_of_week %>% 
  group_by(member_casual, day) %>% 
  ggplot(aes(x = day, y = count_day, fill = member_casual)) +
  geom_bar(stat = "identity",position = "dodge")+
  scale_x_discrete(limits = day_order)+
  scale_fill_brewer(palette = "Set2", name = "Rider Type", labels = c("Casual", "Member"))+
  labs(title = "Comparison of Bicycle Use by Day of the Week", 
       subtitle = "January 2021 - December 2021",
       x = "Day of the Week",
       y = "Number of Rides",
       caption = "Data obtained from https://divvy-tripdata.s3.amazonaws.com/index.html")+
  theme(plot.title =element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
        plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 2),
        axis.title.x = element_text(face = "bold", vjust = -1.5),
        axis.title.y = element_text(face = "bold"),
        plot.margin = unit(c(2.5,2,2,2), "cm"))




#Plot of year, average trip duration by date----
full_year_date %>% 
  na.omit() %>% 
  filter(meanduration > 0) %>% 
  filter(meanduration < 60) %>% 
  ggplot(aes(x = start_date, y = meanduration, color= member_casual))+
  geom_jitter()+
  facet_wrap(~member_casual)+
  scale_color_brewer(palette="Set2")+
  theme(legend.position = "none",
        plot.title =element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
        plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 2),
        axis.title.x = element_text(face = "bold", vjust = -1.5),
        axis.title.y = element_text(face = "bold"),
        plot.margin = unit(c(2.5,2,2,2), "cm"))  +
  labs(title = "Average Trip Duration", 
       subtitle = "January 2021 - December 2021",
       x = "Date",
       y = "Average Trip Duration (minutes)",
       caption = "Data obtained from https://divvy-tripdata.s3.amazonaws.com/index.html")
    
  

#Plot of year, number of rides per day by membership type----
count_weather%>% 
  na.omit() %>% 
  ggplot(aes(x = start_date, y = n, color= avg_feel))+
  geom_jitter()+
  facet_wrap(~member_casual)+
  scale_colour_distiller(palette = "RdYlBu")+
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, vjust = 3),
        plot.subtitle = element_text(size = 12, hjust = 0.5, vjust = 2),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face = "bold"),
        plot.margin = unit(c(2.5,2,2,2), "cm"))+
  labs(title = "Daily Ride Count as a Function of Temperature", 
       subtitle = "January 2021 - December 2021", 
       x = "Date", y = "Number of Rides", 
       color = "Average Outdoor Feel (°F)",
       caption = "Data obtained from https://divvy-tripdata.s3.amazonaws.com/index.html,
       https://mesonet.agron.iastate.edu/sites/hist.phtml?station=MDW&network=IL_ASOS&year=2021&month=3 " )

#Save Plots---
ggsave("avgduration.png", width = 40, height = 30, units = "cm")
ggsave("ridecount.png", width = 40, height = 30, units = "cm")
ggsave("biketype.png", width = 30, height = 30, units = "cm")
ggsave("dayweek.png", width = 30, height = 30, units = "cm")
