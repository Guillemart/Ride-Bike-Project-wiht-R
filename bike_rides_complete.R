#First I load the librarys that i will use for the project
library(tidyverse)
library(ggplot2)
library(lubridate)
library(dplyr)
library(readr)
library(janitor)
library(data.table)
library(tidyr)

#Now i load all the data that i Will use in the project, the 12 months that I will study(July 2021- to June 2022)

m1<-read.csv("C:\\Users\\17040\\Downloads\\data\\m1.csv")
m2<-read.csv("C:\\Users\\17040\\Downloads\\data\\m2.csv")
m3<-read.csv("C:\\Users\\17040\\Downloads\\data\\m3.csv")
m4<-read.csv("C:\\Users\\17040\\Downloads\\data\\m4.csv")
m5<-read.csv("C:\\Users\\17040\\Downloads\\data\\m5.csv")
m6<-read.csv("C:\\Users\\17040\\Downloads\\data\\m6.csv")
m7<-read.csv("C:\\Users\\17040\\Downloads\\data\\m7.csv")
m8<-read.csv("C:\\Users\\17040\\Downloads\\data\\m8.csv")
m9<-read.csv("C:\\Users\\17040\\Downloads\\data\\m9.csv")
m10<-read.csv("C:\\Users\\17040\\Downloads\\data\\m10.csv")
m11<-read.csv("C:\\Users\\17040\\Downloads\\data\\m11.csv")
m12<-read.csv("C:\\Users\\17040\\Downloads\\data\\m12.csv")


# Compare the column names 
colnames(m1)
colnames(m2)
colnames(m3)
colnames(m4)
colnames(m5)
colnames(m6)
colnames(m7)
colnames(m8)
colnames(m9)
colnames(m10)
colnames(m11)
colnames(m12)

#View data frames
str(m1)
str(m2)
str(m3)
str(m4)
str(m5)
str(m6)
str(m7)
str(m8)
str(m9)
str(m10)
str(m11)
str(m12)

# convert Data types
m1 <-  mutate(m1, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type))
m2 <-  mutate(m2, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m3 <-  mutate(m3, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m4 <-  mutate(m4, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m5 <-  mutate(m5, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m6 <-  mutate(m6, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m7 <-  mutate(m7, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m8 <-  mutate(m8, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m9 <-  mutate(m9, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m10<-  mutate(m10, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))
m11<-  mutate(m11, ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type))

m12<-  mutate(m12,ride_id = as.character(ride_id)
              ,rideable_type = as.character(rideable_type)) 

# combining all datasets into one dataframe
bike_rides <- bind_rows(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12)

# check structure of the new dataframe
str(bike_rides)

#Then I remove the columns that i wont use in the project, I have removed ride_id and starting and ending latitude and longitude.

bike_rides <- bike_rides %>%
  select(-c(start_lat:end_lng))
glimpse(bike_rides)

bike_rides <- bike_rides %>%
  select(-c(ride_id))
glimpse(bike_rides)

bike_rides <- bike_rides %>%
  select(-c(start_station_id))
glimpse(bike_rides)

bike_rides <- bike_rides %>%
  select(-c(end_station_id))
glimpse(bike_rides)



#Now I rename the columns for better understanding
bike_rides <- bike_rides %>%
  rename(ride_type = rideable_type, 
         start_time = started_at,
         end_time = ended_at,
         customer_type = member_casual)
glimpse(bike_rides)

#add columns that list the date, month,day,year of ride.
bike_rides$date <- as.Date(bike_rides$start_time) 
bike_rides$month <- format(as.Date(bike_rides$date), "%m")
bike_rides$day <- format(as.Date(bike_rides$date), "%d")
bike_rides$year <- format(as.Date(bike_rides$date), "%Y")
bike_rides$day_of_week <- format(as.Date(bike_rides$date), "%A")


# Add ride trip duration
bike_rides$ride_length <- difftime(bike_rides$end_time,bike_rides$start_time)


# convert ride_length to numeric

is.factor(bike_rides$ride_length)
bike_rides$ride_length <- as.numeric(as.character(bike_rides$ride_length))
is.numeric(bike_rides$ride_length)

# Remove data 
bike_rides_v2 <- bike_rides[!(bike_rides$start_station_name == "HQ QR" | bike_rides$ride_length<=0),]


# Some statistical analysis

mean(bike_rides_v2$ride_length) #straight average (total ride length / rides)
median(bike_rides_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(bike_rides_v2$ride_length) #longest ride
min(bike_rides_v2$ride_length) #shortest ride

summary(bike_rides_v2$ride_length)

#Compare members and casual users

aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type, FUN = mean)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type, FUN = median)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type, FUN = max)
aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type, FUN = min)

aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type + bike_rides_v2$day_of_week, FUN = mean)


# Order the days of the week

bike_rides_v2$day_of_week <- ordered(bike_rides_v2$day_of_week, levels=c("domingo", "lunes", "martes", "miércoles", "jueves", "viernes", "sábado"))

#Average ride time by each day group by different customers type

aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type + bike_rides_v2$day_of_week, FUN = mean)


#type of customer and weekday
bike_rides_v2 %>% 
  mutate(weekday = wday(start_time)) %>%  
  group_by(customer_type, weekday) %>%  
  summarise(number_of_rides = n()							 
            ,average_duration = mean(ride_length)) %>% 	
  arrange(customer_type, weekday)

#Visualization by number of rides and rider type
bike_rides_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = customer_type)) +
  geom_col(position = "dodge")

#Visualization for average duration
bike_rides_v2 %>% 
  mutate(weekday = wday(start_time)) %>% 
  group_by(customer_type, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(customer_type, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = customer_type)) +
  geom_col(position = "dodge")

#export the file for further investigation and analysis

counts <- aggregate(bike_rides_v2$ride_length ~ bike_rides_v2$customer_type + bike_rides_v2$day_of_week, FUN = mean)

# Open library to move the data frame to excel all united for further investigation
library(openxlsx)



write.csv(bike_rides_v2,"bike_rides_full.csv")





