# ---------------------------
#
# Script name:Transport Systems: Exercise 1     
# Purpose of script: Descriptive statistics, graphs, tables
# Author: Severin Planzer, Elisa Szalay, Philippe Wegmann
# Date Created: September 24th 2025
#
# ---------------------------

# load all necessary packages
library(tidyverse)
library(dplyr)
library(ggplot2)

# Exercise 1 --------------------------------------------------------------

## 1.1	Exploring descriptive statistics ----------------------------------

# load data (you can use the load() function since it is a RData file)

load("01_Data/smideBookingData.RData")

str(smideBookingData)
head(smideBookingData)

#library(lubridate)
# trip duration
smide_start=min(smideBookingData$startdat, na.rm = TRUE)
smide_end=max(smideBookingData$enddate, na.rm = TRUE)

cat("The time period of the recorded trips ranges from", 
    as.character(smide_start), "to", as.character(smide_end), "\n")

# mean trip duration and distance
smide_trip_duration_mean=mean(smideBookingData$tripDMIN)
smide_trip_distance_mean=mean(smideBookingData$tripDKM)
smide_trip_duration_sd=sd(smideBookingData$tripDMIN)
smide_trip_distance_sd=sd(smideBookingData$tripDKM)

cat("The mean trip duration of the recorded trips is", 
    round(smide_trip_duration_mean,2), "minutes and the standard deviation is", round(smide_trip_duration_sd,2), "minutes.")

cat("The mean trip distance of the recorded trips is", 
    round(smide_trip_distance_mean,2), "km and the standard deviation is", round(smide_trip_distance_sd,2), "km.")

## 1.2	Calculating statistics by group -------------------------------------

#by user
smideBookingData %>%
  count(userID) %>%
  summarise(mean_user = mean(n), sd_user = sd(n))

#by bike
smideBookingData %>%
  count(bikeID) %>%
  summarise(mean_bike = mean(n), sd_bike = sd(n))

## 1.3	Creating a bar chart (plotting a categorical and numerical variable) -------

mean_daily_bookings <- smideBookingData %>%
  group_by(startdat, dOfWeek) %>%        # trips per day of week
  summarise(daily_bookings = n(), .groups = "drop") %>%
  group_by(dOfWeek) %>%                  # average across all Mondays, Tuesdays, etc.
  summarise(mean_bookings = mean(daily_bookings))
mean_daily_bookings

ggplot(mean_daily_bookings, aes(x = dOfWeek, y = mean_bookings)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Day of the Week",
       y = "Mean Number of Daily Bookings") +
  theme_minimal()

## 1.4	Creating a line chart (plotting two numerical variables) ------------

mean_hourly_bookings <- smideBookingData %>%
  group_by(startdat, hOfDay) %>%
  summarise(hourly_bookings = n(), .groups = "drop") %>%
  group_by(hOfDay) %>%
  summarise(mean_bookings = mean(hourly_bookings))

ggplot(mean_hourly_bookings, aes(x = hOfDay, y = mean_bookings)) +
  geom_line(color = "steelblue", size = 1) +
  scale_x_continuous(breaks = 0:23) +   # 👈 show every hour tick
  labs(x = "Hour of the Day",
       y = "Mean Number of Bookings") +
  theme_minimal()



# Exercise 1: MOBIS SOCIOECONOMIC DATA -------------------------------------


## 2.1	Summary statistics of socioeconomic variables -----------------------

# load dataset (you will need to read the csv file)

# tidy dataset



##  2.2	Comparing socioeconomic characteristics between the MOBIS sample and the microcensus --------

## 2.3	Plotting the relationship between age and income ----------------------



## 2.4	Checking for a normal distribution -----------------------------------






# 3	MOBIS: TRAVEL BEHAVIOUR -----------------------------------------------

mobis_data = read.csv("01_Data/mobis_enriched.csv")

## 3.1	Summary trip statistics ---------------------------------------------

trip_stats = mobis_data %>% 
  group_by(participant_ID) %>% 
  summarise(n_trips=n_distinct(Trip_id), # in the sd calc sth is wrong sd > mean
            mean_duration=mean(duration)/60,
            sd_duration=sd(duration)/60,
            mean_length=mean(length)/1000,
            sd_length= sd(length)/1000,
            mean_speed=mean(length/duration)*3.6, # 3.6 = 60^2/1000
            sd_speed=sd(length/duration)*3.6,
            total_length = sum(length),
            total_duration = sum(duration)) %>%
  mutate(share_trips = n_trips / sum(n_trips)) %>% 
  mutate(share_duration = total_duration / sum(total_duration)) %>% 
  mutate(share_length = total_length / sum(total_length))



## 3.2	Calculating mode share ----------------------------------------------





## 3.3	Mode share vs income and age ----------------------------------------




## 3.4	Average daily passenger-kilometres travelled per person per per mode, by income and age group --------



