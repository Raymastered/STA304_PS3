#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS website - usa_00001.dta.gz
# Author: Ruize Liu, Yi Lei Feng
# Data: 22 October 2020
# Contact: ruize.liu@mail.utoronto.ca, yilei.feng@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
raw_data <- read_dta("usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(
    stateicp,
    age, 
    race, 
    educd,
    labforce)

#### What's next? ####

## Here I am only splitting cells by age, but you 
## can use other variables to split by changing
## count(age) to count(age, sex, ....)

reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)") %>%
  filter(as.numeric(age) >= 18)

reduced_data <- reduced_data %>%
  mutate(educd = 
           case_when(
             educd == "no schooling completed" ~ "3rd Grade or less",
             educd == "nursery school, preschool" ~ "3rd Grade or less",
             educd == "kindergarten" ~ "3rd Grade or less",
             educd == "grade 1" ~ "3rd Grade or less",
             educd == "grade 2" ~ "3rd Grade or less",
             educd == "grade 3" ~ "3rd Grade or less",
             educd == "grade 4" ~ "Middle School - Grades 4 - 8",
             educd == "grade 5" ~ "Middle School - Grades 4 - 8",
             educd == "grade 6" ~ "Middle School - Grades 4 - 8",
             educd == "grade 7" ~ "Middle School - Grades 4 - 8",
             educd == "grade 8" ~ "Middle School - Grades 4 - 8",
             educd == "grade 9" ~ "High school graduate",
             educd == "grade 10" ~ "High school graduate",
             educd == "grade 11" ~ "High school graduate",
             educd == "12th grade, no diploma" ~ "High school graduate",
             educd == "regular high school diploma" ~ "Completed some high school",
             educd == "ged or alternative credential" ~ "Completed some high school",
             educd == "some college, but less than 1 year" ~ "High school graduate",
             educd == "1 or more years of college credit, no degree" ~ "Completed some college, but no degree",
             educd == "associate's degree, type not specified" ~ "Associate Degree",
             educd == "bachelor's degree" ~ "College Degree (such as B.A., B.S.)",
             educd == "master's degree" ~ "Masters degree",
             educd == "professional degree beyond a bachelor's degree" ~ "Masters degree",
             educd == "doctoral degree" ~ "Doctorate degree"
           )
  )

reduced_data <- 
  reduced_data %>%
  count(race, educd, labforce) %>%
  group_by(race, educd, labforce)

reduced_data$age <- as.integer(reduced_data$age)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")