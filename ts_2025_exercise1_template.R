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

# read data and make some manipulations on the global dataset
mobis_data = read.csv("01_Data/mobis_enriched.csv")
mobis_data$duration_min = mobis_data$duration / 60
mobis_data$length_km = mobis_data$length / 1000
mobis_data$speed = 60 * mobis_data$length_km / mobis_data$duration
mobis_data$mode = str_remove(mobis_data$mode, "^Mode::")
mobis_data$date = date(mobis_data$started_at_y)

qs = quantile(mobis_data$age, probs = seq(0, 1, 0.25), na.rm = TRUE)
mobis_data = mobis_data %>%
  mutate(
    age_cat = cut(
      age,
      breaks = qs,
      include.lowest = TRUE,
      labels = paste0(
        round(head(qs, -1)), "-", round(tail(qs, -1))
      )
    )
  )


## test


mobis_data %>%
  group_by(mode) %>%
  summarise(
    avg_length_km = mean(length_km),
    n_trips = n()
  )

mode_share_check <- mobis_data %>%
  group_by(mode) %>%
  summarise(
    trips = n(),
    pkm   = sum(length) / 1000,   # passenger-km
    .groups = "drop"
  ) %>%
  mutate(
    share_trips = trips / sum(trips),
    share_pkm   = pkm / sum(pkm)
  ) %>%
  arrange(desc(share_trips))   # sort for readability

print(mode_share_check)




## 3.1	Summary trip statistics ---------------------------------------------

trip_stats = mobis_data %>% 
  group_by(participant_ID) %>% 
  summarise(n_trips = n_distinct(Trip_id), # in the sd calc sth is wrong sd > mean
            mean_duration = mean(duration_min),
            sd_duration = sd(duration_min),
            mean_length = mean(length_km),
            sd_length = sd(length_km),
            mean_speed = mean(speed), # 3.6 = 60^2/1000
            sd_speed = sd(speed),
            total_length = sum(length),
            total_duration = sum(duration)) %>%
  mutate(share_trips = n_trips/sum(n_trips)) %>% 
  mutate(share_duration = total_duration/sum(total_duration)) %>% 
  mutate(share_length = total_length/sum(total_length))

## 3.2	Calculating mode share ----------------------------------------------
# => I have the feeling the cars should be the other way around with looking at the plot
mode_share = mobis_data %>%
  group_by(mode) %>% 
  summarize(
    trips = n(),
    pkm   = sum(length) / 1000,
    .groups = "drop"
  ) %>% 
  mutate(
    share_trips = trips / sum(trips),
    share_pkm   = pkm   / sum(pkm),
    mode = sub("^Mode::", "", mode)
  )

mode_share_long <- mode_share %>%
  pivot_longer(cols = c(share_trips, share_pkm),
               names_to = "measure", values_to = "share") %>%
  mutate(
    # Force correct order and names
    measure = factor(measure,
                     levels = c("share_trips", "share_pkm"),
                     labels = c("Trips", "Passenger-km"))
  )

ggplot(mode_share_long, aes(x = share, y = mode, fill = measure)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Trips" = "orange", "Passenger-km" = "skyblue")) +
  labs(title = "Mode Share: Trips vs Passenger-km",
       x = "Share", y = "Mode", fill = "Measure") +
  theme_minimal()


## 3.3	Mode share vs income and age ----------------------------------------

mobis_data %>%
  group_by(income, mode) %>%
  summarise(
    trips = n(),
    pkm   = sum(length) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    share_trips = trips / sum(trips),
    share_pkm   = pkm / sum(pkm)
  ) %>%
  pivot_longer(cols = c(share_trips, share_pkm),
               names_to = "measure", values_to = "share") %>% 
  ggplot(aes(x = share, y = income, fill = measure)) +
  geom_col(position = "dodge") +
  facet_wrap(~mode) +
  scale_fill_manual(values = c("share_trips" = "skyblue", "share_pkm" = "orange"),
                    labels = c("Trips", "Passenger-km")) +
  labs(title = "Mode Share by Income Group",
       x = "Mode share", y = "Income group", fill = "Definition") +
  theme_minimal()




mobis_data %>%
  group_by(age_cat, mode) %>%
  summarise(
    trips = n(),
    pkm   = sum(length) / 1000,
    .groups = "drop"
  ) %>%
  mutate(
    share_trips = trips / sum(trips),
    share_pkm   = pkm / sum(pkm)
  ) %>%
  pivot_longer(cols = c(share_trips, share_pkm),
               names_to = "measure", values_to = "share") %>% 
  ggplot(aes(x = share, y = age_cat, fill = measure)) +
  geom_col(position = "dodge") +
  facet_wrap(~mode) +
  scale_fill_manual(values = c("share_trips" = "skyblue", "share_pkm" = "orange"),
                    labels = c("Trips", "Passenger-km")) +
  labs(title = "Mode Share by Age Group",
       x = "Mode share", y = "Age share", fill = "Definition") +
  theme_minimal()

## 3.4	Average daily passenger-kilometres travelled per person per per mode, by income and age group --------

income_levels <- c(
  "More than 16 000 CHF",
  "12 001 - 16 000 CHF",
  "8 001 - 12 000 CHF",
  "4 001 - 8 000 CHF",
  "4 000 CHF or less",
  "Prefer not to say"
)

mobis_data %>%
  mutate(
    pkm = length / 1000,
    income = factor(income, levels = income_levels)
  ) %>%
  group_by(participant_ID, income, mode) %>%
  summarise(total_pkm = sum(pkm), .groups = "drop") %>%
  group_by(income, mode) %>%
  summarise(
    avg_pkm = mean(total_pkm),   # mean PKM per person
    .groups = "drop"
  ) %>% 
  ggplot(aes(x = avg_pkm, y = income, fill = mode)) +
  geom_col() +
  labs(
    title = "Passenger-kilometres per Person by Income Group",
    x = "Average PKM per person", y = "Income group", fill = "Mode"
  ) +
  theme_minimal()

## 3.5 Average passenger-kilometres travelled per person per mode, come and age group


days_per_person <- mobis_data %>%
  group_by(participant_ID) %>%
  summarise(n_days = n_distinct(date), .groups = "drop")

daily_pkm <- mobis_data %>%
  mutate(
    pkm  = length / 1000,
    income = factor(income, levels = income_levels)
  ) %>%
  left_join(days_per_person, by = "participant_ID") %>%
  group_by(participant_ID, income, age_cat, mode) %>%
  summarise(total_pkm = sum(pkm) / unique(n_days), .groups = "drop") %>%
  group_by(income, age_cat, mode) %>%
  summarise(avg_daily_pkm = mean(total_pkm), .groups = "drop")

ggplot(daily_pkm, aes(x = avg_daily_pkm, y = income, fill = mode)) +
  geom_col() +
  labs(
    title = "Daily Passenger-km per Person by Income Group",
    x = "Average daily PKM per person", y = "Income group", fill = "Mode"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

ggplot(daily_pkm, aes(x = avg_daily_pkm, y = age_cat, fill = mode)) +
  geom_col() +
  labs(
    title = "Daily Passenger-km per Person by Age Group",
    x = "Average daily PKM per person", y = "Age group (quartiles)", fill = "Mode"
  ) +
  theme_minimal()
