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

# Calculate mean, median, and standard deviation for age
mean_age <- mean(mobis_persons$age, na.rm = TRUE)
median_age <- median(mobis_persons$age, na.rm = TRUE)
sd_age <- sd(mobis_persons$age, na.rm = TRUE)
cat("Mean age:", round(mean_age, 2), "\n")
cat("Median age:", round(median_age, 2), "\n")
cat("Standard deviation of age:", round(sd_age, 2), "\n")

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

# Combine 4000-8000 and 8000-12000 into one class "4000-12000"
mobis_persons <- mobis_persons %>%
  mutate(
    income_group = case_when(
      is.na(income) | income %in% c("Prefer not to say", "NA", "") ~ "Not reported",
      income %in% c("4 000 CHF or less") ~ "< 4000",
      income %in% c("4 001 - 8 000 CHF", "8 001 - 12 000 CHF") ~ "4000-12000",
      income %in% c("12 001 - 16 000 CHF", "More than 16 000 CHF") ~ "More than 12000",
      TRUE ~ "Other"
    ),
    income_group = factor(income_group, levels = c("Not reported", "< 4000", "4000-12000", "More than 12000"))
  )

income_group_shares <- mobis_persons %>%
  count(income_group) %>%
  complete(income_group = factor(levels(mobis_persons$income_group), levels = levels(mobis_persons$income_group)), fill = list(n = 0)) %>%
  mutate(share = n / sum(n))

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

mobis_persons <- mobis_persons %>%
  mutate(
    employment_group = case_when(
      employment %in% c("Employed", "Self_employed", "Self-employed") ~ "(self-) Employed",
      employment %in% c("Student", "Apprentice") ~ "Student / Apprentice",
      employment %in% c("Unemployed", "Retired", "Other") ~ "Unemployed, Retired, Other",
      TRUE ~ NA_character_
    ),
    employment_group = factor(employment_group, levels = c("(self-) Employed", "Student / Apprentice", "Unemployed, Retired, Other"))
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

# Only keep the three employment groups for summary table
employment_group_shares <- mobis_persons %>%
  count(employment_group) %>%
  filter(!is.na(employment_group)) %>%
  mutate(share = n / sum(n))

# Add mean, median, and sd of age to the summary table
summary_table <- tibble(
  Variable = c(
    paste0("Age group: ", age_group_shares$age_group),
    "Mean age", "Median age", "SD age",
    paste0("Gender: ", gender_shares$gender),
    paste0("Income group: ", income_group_shares$income_group),
    paste0("Education group: ", education_group_shares$education_group),
    paste0("Household size: ", household_size_shares$household_size_cat),
    "Mean household size",
    "SD household size",
    paste0("Employment group: ", employment_group_shares$employment_group),
    paste0("Car ownership: ", car_ownership_share$ownership),
    paste0("Motorbike ownership: ", motorbike_ownership_share$ownership),
    paste0("Bicycle ownership: ", bicycle_ownership_share$ownership),
    paste0("PT pass: ", pt_pass_share$ownership)
  ),
  Value = c(
    round(100 * age_group_shares$share, 1),
    round(mean_age, 2), round(median_age, 2), round(sd_age, 2),
    round(100 * gender_shares$share, 1),
    round(100 * income_group_shares$share, 1),
    round(100 * education_group_shares$share, 1),
    round(100 * household_size_shares$share, 1),
    round(mean_household_size, 2),
    round(sd_household_size, 2),
    round(100 * employment_group_shares$share, 1),
    round(100 * car_ownership_share$share, 1),
    round(100 * motorbike_ownership_share$share, 1),
    round(100 * bicycle_ownership_share$share, 1),
    round(100 * pt_pass_share$share, 1)
  ),
  Unit = c(
    rep("%", nrow(age_group_shares)),
    "Years", "Years", "Years",
    rep("%", nrow(gender_shares)),
    rep("%", nrow(income_group_shares)),
    rep("%", nrow(education_group_shares)),
    rep("%", nrow(household_size_shares)),
    "Persons",
    "Persons",
    rep("%", nrow(employment_group_shares)),
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
    "Age group: Under 18", "Age group: 18-25", "Age group: 25-45", "Age group: 45-65", "Age group: 65+",
    "Gender: Male", "Gender: Female",
    "Income group: Not reported", "Income group: < 4000", "Income group: 4000-12000", "Income group: More than 12000",
    "Education group: Mandatory", "Education group: Secondary", "Education group: Higher",
    "Household size: 1", "Household size: 2", "Household size: 3+",
    "Employment: (self-) Employed", "Employment: Student / Apprentice", "Employment: Unemployed, Retired, Other"
  ),
  Microcensus = c(
    13.2, 9.0, 29.6, 29.6, 18.5,
    49.3, 50.7,
    20.7, 17.8, 50.2, 11.3,
    19.3, 49.5, 31.2,
    34.0, 35.4, 30.6,
    55.4, 6.3, 38.3
  )
)

# Prepare MOBIS summary for matching variables
mobis_summary <- summary_table %>%
  mutate(
    Variable = case_when(
      str_detect(Variable, "^Employment group:") ~
        str_replace(Variable, "^Employment group:", "Employment:"),
      TRUE ~ Variable
    )
  ) %>%
  filter(
    Variable %in% bfs_microcensus$Variable
  ) %>%
  select(Variable, MOBIS = Value)

# Combine both tables, ensuring all variables from both sources are included
comparison_table <- full_join(
  bfs_microcensus,
  mobis_summary,
  by = "Variable"
) %>%
  arrange(factor(Variable, levels = unique(c(bfs_microcensus$Variable, mobis_summary$Variable))))

# Add a column for the difference between Microcensus and MOBIS (% points)
comparison_table <- comparison_table %>%
  mutate(
    Difference = round(MOBIS - Microcensus, 1)
  )

print(comparison_table, n = Inf)


# 2.2.1 Chi-square goodness-of-fit tests for representativity

# Helper function to run chi-square test for a variable
run_chisq_test <- function(mobis_counts, microcensus_props, n_mobis, var_name) {
  expected <- microcensus_props * n_mobis
  test <- chisq.test(x = mobis_counts, p = microcensus_props, rescale.p = TRUE, simulate.p.value = FALSE)
  tibble(
    Variable = var_name,
    Chi_sq_statistic = unname(test$statistic),
    df = unname(test$parameter),
    p_value = unname(test$p.value)
  )
}

# Age group
age_levels <- c("Under 18", "18-25", "25-45", "45-65", "65+")
mobis_age_counts <- age_group_shares %>% filter(age_group %in% age_levels) %>% arrange(factor(age_group, levels = age_levels)) %>% pull(n)
microcensus_age_props <- bfs_microcensus %>% filter(str_detect(Variable, "Age group:")) %>% arrange(match(Variable, paste0("Age group: ", age_levels))) %>% pull(Microcensus) / 100
n_mobis_age <- sum(mobis_age_counts)
chisq_age <- run_chisq_test(mobis_age_counts, microcensus_age_props, n_mobis_age, "Age group")

# Gender
gender_levels <- c("Male", "Female")
mobis_gender_counts <- gender_shares %>% arrange(factor(gender, levels = gender_levels)) %>% pull(n)
microcensus_gender_props <- bfs_microcensus %>% filter(str_detect(Variable, "Gender:")) %>% arrange(match(Variable, paste0("Gender: ", gender_levels))) %>% pull(Microcensus) / 100
n_mobis_gender <- sum(mobis_gender_counts)
chisq_gender <- run_chisq_test(mobis_gender_counts, microcensus_gender_props, n_mobis_gender, "Gender")

# Income group
income_levels <- c("Not reported", "< 4000", "4000-12000", "More than 12000")
mobis_income_counts <- income_group_shares %>% arrange(factor(income_group, levels = income_levels)) %>% pull(n)
microcensus_income_props <- bfs_microcensus %>% filter(str_detect(Variable, "Income group:")) %>% arrange(match(Variable, paste0("Income group: ", income_levels))) %>% pull(Microcensus) / 100
n_mobis_income <- sum(mobis_income_counts)
chisq_income <- run_chisq_test(mobis_income_counts, microcensus_income_props, n_mobis_income, "Income group")

# Education group
education_levels <- c("Mandatory", "Secondary", "Higher")
mobis_education_counts <- education_group_shares %>% filter(education_group %in% education_levels) %>% arrange(factor(education_group, levels = education_levels)) %>% pull(n)
microcensus_education_props <- bfs_microcensus %>% filter(str_detect(Variable, "Education group:")) %>% arrange(match(Variable, paste0("Education group: ", education_levels))) %>% pull(Microcensus) / 100
n_mobis_education <- sum(mobis_education_counts)
chisq_education <- run_chisq_test(mobis_education_counts, microcensus_education_props, n_mobis_education, "Education group")

# Household size
hh_levels <- c("1", "2", "3+")
mobis_hh_counts <- household_size_shares %>% arrange(factor(household_size_cat, levels = hh_levels)) %>% pull(n)
microcensus_hh_props <- bfs_microcensus %>% filter(str_detect(Variable, "Household size:")) %>% arrange(match(Variable, paste0("Household size: ", hh_levels))) %>% pull(Microcensus) / 100
n_mobis_hh <- sum(mobis_hh_counts)
chisq_hh <- run_chisq_test(mobis_hh_counts, microcensus_hh_props, n_mobis_hh, "Household size")

# Employment group
employment_levels <- c("(self-) Employed", "Student / Apprentice", "Unemployed, Retired, Other")
mobis_emp_counts <- employment_group_shares %>% arrange(factor(employment_group, levels = employment_levels)) %>% pull(n)
microcensus_emp_props <- bfs_microcensus %>% filter(str_detect(Variable, "Employment:")) %>% arrange(match(Variable, paste0("Employment: ", employment_levels))) %>% pull(Microcensus) / 100
n_mobis_emp <- sum(mobis_emp_counts)
chisq_emp <- run_chisq_test(mobis_emp_counts, microcensus_emp_props, n_mobis_emp, "Employment group")

# Combine results
chisq_results <- bind_rows(
  chisq_age,
  chisq_gender,
  chisq_income,
  chisq_education,
  chisq_hh,
  chisq_emp
)

print(chisq_results)






##2.3 Plotting the relationship between age and income

# Convert income_group to an ordered factor for plotting
mobis_persons <- mobis_persons %>%
  mutate(
    income_group = factor(
      income_group,
      levels = c("Not reported", "< 4000", "4000-12000", "More than 12000"),
      ordered = TRUE
    )
  )

# Assign numeric values to income groups for boxplot (midpoints or representative values)
income_numeric_map <- c(
  "Not reported" = NA,
  "< 4000" = 3000,
  "4000-12000" = 8000,
  "More than 12000" = 14000
)
mobis_persons <- mobis_persons %>%
  mutate(
    income_numeric = income_numeric_map[as.character(income_group)]
  )

# Boxplot: Age distribution by income group (no jitter)
ggplot(mobis_persons %>% filter(!is.na(income_group)), aes(x = income_group, y = age, fill = income_group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  labs(
    title = "Age Distribution by Income Group",
    x = "Income Group (CHF)",
    y = "Age"
  ) +
  scale_fill_brewer(palette = "Blues") +
  theme_minimal() +
  theme(legend.position = "none")

  # Calculate mean and standard deviation of age for each income group (boxplot values)
  age_by_income_stats <- mobis_persons %>%
    filter(!is.na(income_group)) %>%
    group_by(income_group) %>%
    summarise(
      mean_age = round(mean(age, na.rm = TRUE), 2),
      median_age = round(median(age, na.rm = TRUE), 2),
      sd_age = round(sd(age, na.rm = TRUE), 2),
      n = n()
    )

  print(age_by_income_stats)








##2.4 Checking for a normal distribution

# Density plot of participant age with overlaid normal curve
age_mean <- mean(mobis_persons$age, na.rm = TRUE)
age_sd <- sd(mobis_persons$age, na.rm = TRUE)

ggplot(mobis_persons, aes(x = age)) +
  geom_density(fill = "skyblue", alpha = 0.6, na.rm = TRUE) +
  stat_function(fun = dnorm, args = list(mean = age_mean, sd = age_sd),
                color = "red", linetype = "dashed", size = 1) +
  labs(
    title = "Density Plot of Participant Age with Normal Curve",
    x = "Age",
    y = "Density"
  ) +
  theme_minimal()

# Q-Q plot for participant age
qqnorm(mobis_persons$age, main = "Q-Q Plot of Participant Age")
qqline(mobis_persons$age, col = "red", lwd = 2)

# Note: The density plot shows the actual age distribution and overlays a normal curve for comparison.
# The Q-Q plot helps assess normality: strong deviations from the line indicate non-normality.








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

## 3.1	Summary trip statistics ---------------------------------------------

print("Numbers on per participant base:")
part_ntrips = mobis_data %>% 
  group_by(participant_ID,
           .groups = "drop") %>% 
  summarise(n_trips = n_distinct(Trip_id))
mean(part_ntrips$n_trips)
sd(part_ntrips$n_trips)

print("Numbers on global base:")
mobis_data %>% 
  group_by(Trip_id,
           .groups = "drop") %>% 
  summarize(trip_duration = sum(duration_min),
            trip_length = sum(length_km),
            trip_duration = sum(duration_min),
            trip_speed = 60*sum(length_km)/sum(duration_min)) %>% 
  summarise(
  mean_duration = mean(trip_duration, na.rm = TRUE),
  sd_duration   = sd(trip_duration, na.rm = TRUE),
  mean_length   = mean(trip_length, na.rm = TRUE),
  sd_length     = sd(trip_length, na.rm = TRUE),
  mean_speed    = mean(trip_speed, na.rm = TRUE),
  sd_speed      = sd(trip_speed, na.rm = TRUE)
  )

## 3.2	Calculating mode share ----------------------------------------------

mode_share_long = mobis_data %>%
  group_by(mode) %>% 
  summarize(
    trips = n(),
    pkm   = sum(length) / 1000,
    .groups = "drop") %>% 
  mutate(
    share_trips = trips / sum(trips),
    share_pkm   = pkm   / sum(pkm),
    mode = sub("^Mode::", "", mode)
  ) %>%
  pivot_longer(
    cols = c(share_trips, share_pkm),
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

# add as a share of trip legs

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
