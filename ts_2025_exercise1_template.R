# ---------------------------
#
# Script name:Transport Systems: Exercise 1     
# Purpose of script: Descriptive statistics, graphs, tables
# Author: Severin Planzer, Elisa Szalay, Philippe Wegmann
# Date Created: September 24th 2025
#
# ---------------------------

# load all necessary packages
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("ggplot2")

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
  scale_x_continuous(breaks = 0:23) +  
  labs(x = "Hour of the Day",
       y = "Mean Number of Bookings") +
  theme_minimal()



# Exercise 2: MOBIS SOCIOECONOMIC DATA -------------------------------------


## 2.1	Summary statistics of socioeconomic variables -----------------------

# load dataset (you will need to read the csv file)
mobis_data <- read.csv("01_Data/mobis_enriched.csv")

# Select only socioeconomic variables and ensure one row per person
mobis_persons <- mobis_data %>%
  select(participant_ID, age, gender, income, education, main_employment,
         household_size, own_vehicles_car, own_vehicles_motorbike, 
         own_vehicles_bicycle, pt_pass_no_pass, pt_pass_type) %>%
  distinct(participant_ID, .keep_all = TRUE)  # ensures each person appears only once

mobis_persons <- mobis_persons %>%
  mutate(
    gender     = factor(gender, levels = c("Female", "Male")),
    income     = factor(income),
    education  = factor(education),
    employment = factor(main_employment),

    # ownerships: unify yes/no or TRUE/FALSE → 0/1
    car_ownership       = ifelse(own_vehicles_car %in% c("yes","Yes",TRUE,1), 1, 0),
    motorbike_ownership = ifelse(own_vehicles_motorbike %in% c("yes","Yes",TRUE,1), 1, 0),
    bicycle_ownership   = ifelse(own_vehicles_bicycle %in% c("yes","Yes",TRUE,1), 1, 0),
    pt_pass             = ifelse(pt_pass_no_pass %in% c("yes","Yes",TRUE,1), 1, 0)
  )

# Define age groups
mobis_persons <- mobis_persons %>%
  mutate(
    age_group = cut(
      age,
      breaks = c(-Inf, 17, 25, 45, 65, Inf),
      labels = c("Under 18", "18-25", "25-45", "45-65", "65+"),
      right = TRUE,
      include.lowest = TRUE
    )
  )

# Calculate shares and summary statistics
age_group_shares <- mobis_persons %>%
  count(age_group) %>%
  complete(age_group = factor(levels(mobis_persons$age_group), levels = levels(mobis_persons$age_group)), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

# Only keep "Female" and "Male" for gender shares
gender_shares <- mobis_persons %>%
  filter(gender %in% c("Female", "Male")) %>%
  count(gender) %>%
  complete(gender = factor(c("Female", "Male"), levels = c("Female", "Male")), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

# Adjust income groupings to match actual categories in the data
income_levels <- c(
  "More than 16 000 CHF",
  "12 001 - 16 000 CHF",
  "8 001 - 12 000 CHF",
  "4 001 - 8 000 CHF",
  "4 000 CHF or less",
  "Prefer not to say"
)

mobis_persons <- mobis_persons %>%
  mutate(
    income_group = case_when(
      is.na(income) | income %in% c("Prefer not to say", "NA", "") ~ "Not reported",
      income == "4 000 CHF or less" ~ "< 4000",
      income == "4 001 - 8 000 CHF" ~ "4000-8000",
      income == "8 001 - 12 000 CHF" ~ "8000-12000",
      income == "12 001 - 16 000 CHF" ~ "12000-16000",
      income == "More than 16 000 CHF" ~ "More than 16000",
      TRUE ~ "Other"
    ),
    income_group = factor(income_group, levels = c("Not reported", "< 4000", "4000-8000", "8000-12000", "12000-16000", "More than 16000"))
  )

income_group_shares <- mobis_persons %>%
  count(income_group) %>%
  complete(income_group = factor(levels(mobis_persons$income_group), levels = levels(mobis_persons$income_group)), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

library(stringr)
# Extract the lower bound of the income range for numeric calculation
income_numeric <- case_when(
  mobis_persons$income == "4 000 CHF or less" ~ 4000,
  mobis_persons$income == "4 001 - 8 000 CHF" ~ 8000,
  mobis_persons$income == "8 001 - 12 000 CHF" ~ 12000,
  mobis_persons$income == "12 001 - 16 000 CHF" ~ 16000,
  mobis_persons$income == "More than 16 000 CHF" ~ 20000,
  TRUE ~ NA_real_
)
mean_income_numeric <- mean(income_numeric, na.rm = TRUE)
sd_income_numeric <- sd(income_numeric, na.rm = TRUE)

mobis_persons <- mobis_persons %>%
  mutate(
    education_group = case_when(
      education == "Mandatory education" ~ "Mandatory",
      education == "Secondary education (e.g., apprenticeship or diploma)" ~ "Secondary",
      education == "Higher education (e.g., university)" ~ "Higher",
      TRUE ~ "Other"
    ),
    education_group = factor(education_group, levels = c("Mandatory", "Secondary", "Higher", "Other"))
  )

education_group_shares <- mobis_persons %>%
  count(education_group) %>%
  complete(education_group = factor(levels(mobis_persons$education_group), levels = levels(mobis_persons$education_group)), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

# Household size: collapse to 1, 2, 3+
mobis_persons <- mobis_persons %>%
  mutate(
    household_size_cat = case_when(
      household_size == 1 ~ "1",
      household_size == 2 ~ "2",
      household_size >= 3 ~ "3+",
      TRUE ~ NA_character_
    ),
    household_size_cat = factor(household_size_cat, levels = c("1", "2", "3+"))
  )

household_size_shares <- mobis_persons %>%
  count(household_size_cat) %>%
  complete(household_size_cat = factor(c("1", "2", "3+"), levels = c("1", "2", "3+")), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

# Mean and SD household size (only for non-missing values)
household_size_numeric <- as.numeric(mobis_persons$household_size)
mean_household_size <- mean(household_size_numeric, na.rm = TRUE)
sd_household_size <- sd(household_size_numeric, na.rm = TRUE)

employment_shares <- mobis_persons %>%
  count(employment) %>%
  complete(employment = factor(levels(mobis_persons$employment), levels = levels(mobis_persons$employment)), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

car_ownership_share <- mobis_persons %>%
  count(car_ownership) %>%
  mutate(
    ownership = ifelse(car_ownership == 1, "Owns car", "Does not own car"),
    share = n / sum(n)
  )

motorbike_ownership_share <- mobis_persons %>%
  count(motorbike_ownership) %>%
  mutate(
    ownership = ifelse(motorbike_ownership == 1, "Owns motorbike", "Does not own motorbike"),
    share = n / sum(n)
  )

bicycle_ownership_share <- mobis_persons %>%
  count(bicycle_ownership) %>%
  mutate(
    ownership = ifelse(bicycle_ownership == 1, "Owns bicycle", "Does not own bicycle"),
    share = n / sum(n)
  )

pt_pass_share <- mobis_persons %>%  
  count(pt_pass) %>%
  mutate(
    ownership = ifelse(pt_pass == 1, "Has PT pass", "Does not have PT pass"),
    share = n / sum(n)
  )

# Combine all calculated values into one summary table
summary_table <- tibble(
  Variable = c(
    paste0("Age group: ", age_group_shares$age_group),
    paste0("Gender: ", gender_shares$gender),
    paste0("Income group: ", income_group_shares$income_group),
    "Mean income (CHF, est.)",
    "SD income (CHF, est.)",
    paste0("Education group: ", education_group_shares$education_group),
    paste0("Household size: ", household_size_shares$household_size_cat),
    "Mean household size",
    "SD household size",
    paste0("Employment: ", employment_shares$employment),
    paste0("Car ownership: ", car_ownership_share$ownership),
    paste0("Motorbike ownership: ", motorbike_ownership_share$ownership),
    paste0("Bicycle ownership: ", bicycle_ownership_share$ownership),
    paste0("PT pass: ", pt_pass_share$ownership)
  ),
  Value = c(
    round(100 * age_group_shares$share, 1),
    round(100 * gender_shares$share, 1),
    round(100 * income_group_shares$share, 1),
    round(mean_income_numeric, 0),
    round(sd_income_numeric, 0),
    round(100 * education_group_shares$share, 1),
    round(100 * household_size_shares$share, 1),
    round(mean_household_size, 2),
    round(sd_household_size, 2),
    round(100 * employment_shares$share, 1),
    round(100 * car_ownership_share$share, 1),
    round(100 * motorbike_ownership_share$share, 1),
    round(100 * bicycle_ownership_share$share, 1),
    round(100 * pt_pass_share$share, 1)
  ),
  Unit = c(
    rep("%", nrow(age_group_shares)),
    rep("%", nrow(gender_shares)),
    rep("%", nrow(income_group_shares)),
    "CHF",
    "CHF",
    rep("%", nrow(education_group_shares)),
    rep("%", nrow(household_size_shares)),
    "Persons",
    "Persons",
    rep("%", nrow(employment_shares)),
    rep("%", nrow(car_ownership_share)),
    rep("%", nrow(motorbike_ownership_share)),
    rep("%", nrow(bicycle_ownership_share)),
    rep("%", nrow(pt_pass_share))
  )
)

print(summary_table, n = Inf)



##2.2 Comparing socioeconomic characteristics between the MOBIS sample and the microcensus

# Add BFS 2017 Microcensus summary as a data frame for comparison
bfs_microcensus <- tibble(
  Variable = c(
    "Age: under 18", "Age: 18-25", "Age: 25-45", "Age: 45-65", "Age: over 65",
    "Gender: Male", "Gender: Female",
    "Income: prefer not to say", "Income: Low (up to 4000 CHF)", "Income: Medium (4000-12000 CHF)", "Income: High (more than 12000 CHF)",
    "Education: Mandatory", "Education: Secondary", "Education: Higher",
    "Household size: 1", "Household size: 2", "Household size: 3 or more",
    "Employment: (self-) Employed", "Employment: Student / Apprentice", "Employment: Unemployed, Retired, Other"
  ),
  Value = c(
    13.2, 9.0, 29.6, 29.6, 18.5,
    49.3, 50.7,
    20.7, 17.8, 50.2, 11.3,
    19.3, 49.5, 31.2,
    34.0, 35.4, 30.6,
    55.4, 6.3, 38.3
  ),
  Unit = rep("%", 20)
)

# Prepare a comparison table between MOBIS and Microcensus
# First, align variable names and categories for comparison

# Extract comparable variables from summary_table
mobis_comp <- summary_table %>%
  filter(
    grepl("^Age group: ", Variable) |
    grepl("^Gender: ", Variable) |
    grepl("^Income group: ", Variable) |
    grepl("^Education group: ", Variable) |
    grepl("^Household size: ", Variable) |
    grepl("^Employment: ", Variable)
  ) %>%
  mutate(
    Category = str_replace(Variable, "^(Age group: |Gender: |Income group: |Education group: |Household size: |Employment: )", ""),
    Group = case_when(
      grepl("^Age group: ", Variable) ~ "Age",
      grepl("^Gender: ", Variable) ~ "Gender",
      grepl("^Income group: ", Variable) ~ "Income",
      grepl("^Education group: ", Variable) ~ "Education",
      grepl("^Household size: ", Variable) ~ "Household size",
      grepl("^Employment: ", Variable) ~ "Employment",
      TRUE ~ NA_character_
    )
  ) %>%
  select(Group, Category, MOBIS = Value, Unit)

# Prepare microcensus for join
bfs_comp <- bfs_microcensus %>%
  mutate(
    Group = case_when(
      grepl("^Age:", Variable) ~ "Age",
      grepl("^Gender:", Variable) ~ "Gender",
      grepl("^Income:", Variable) ~ "Income",
      grepl("^Education:", Variable) ~ "Education",
      grepl("^Household size:", Variable) ~ "Household size",
      grepl("^Employment:", Variable) ~ "Employment",
      TRUE ~ NA_character_
    ),
    Category = str_trim(str_replace(Variable, "^(Age: |Gender: |Income: |Education: |Household size: |Employment: )", ""))
  ) %>%
  select(Group, Category, Microcensus = Value, Unit)

# Join tables for side-by-side comparison
comparison_table <- full_join(mobis_comp, bfs_comp, by = c("Group", "Category", "Unit")) %>%
  arrange(Group, Category)

print(comparison_table, n = Inf)







##2.3 Plotting the relationship between age and income

# Calculate share of each income group by age
income_age_share <- mobis_persons %>%
  filter(!is.na(age), !is.na(income_group)) %>%
  group_by(age, income_group) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(age) %>%
  mutate(share = n / sum(n))

# Line plot: x = age, y = share, color = income group
ggplot(income_age_share, aes(x = age, y = share, color = income_group)) +
  geom_line(size = 1) +
  labs(
    title = "Share of Income Groups by Age",
    x = "Age",
    y = "Share",
    color = "Income Group"
  ) +
  theme_minimal()


  # Boxplot: income (numeric) by age group
  ggplot(mobis_persons, aes(x = age_group, y = income_numeric)) +
    geom_boxplot(fill = "skyblue") +
    labs(
      title = "Distribution of Estimated Income by Age Group",
      x = "Age Group",
      y = "Estimated Income (CHF)"
    ) +
    theme_minimal()







##2.4 Checking for a normal distribution

# Density plot 
ggplot(mobis_persons, aes(x = age)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  labs(
    title = "Density Plot of Participant Age",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()

# Q-Q plot 
qqnorm(mobis_persons$age, main = "Q-Q Plot of Participant Age")
qqline(mobis_persons$age, col = "red", lwd = 2)

# Gaussian curve for age distribution
age_mean <- mean(mobis_persons$age, na.rm = TRUE)
age_sd <- sd(mobis_persons$age, na.rm = TRUE)
age_range <- seq(min(mobis_persons$age, na.rm = TRUE), max(mobis_persons$age, na.rm = TRUE), length.out = 200)
gaussian_curve <- dnorm(age_range, mean = age_mean, sd = age_sd)
amount <- gaussian_curve * length(na.omit(mobis_persons$age)) * (age_range[2] - age_range[1])

ggplot() +
  geom_line(aes(x = age_range, y = amount), color = "red", size = 1) +
  labs(
    title = "Gaussian Curve of Participant Age",
    x = "Age",
    y = "Amount"



  ) +
  theme_minimal()









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
