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


# Exercise 1 --------------------------------------------------------------

## 1.1	Exploring descriptive statistics ----------------------------------

# load data (you can use the load() function since it is a RData file)

load("01_Data/smideBookingData.RData")

## 1.2	Calculating statistics by group -------------------------------------




## 1.3	Creating a bar chart (plotting a categorical and numerical variable) -------





## 1.4	Creating a line chart (plotting two numerical variables) ------------




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



