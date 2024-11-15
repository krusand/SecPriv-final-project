# Setup
setwd("/Users/krusand/Documents/GitHub/SecPriv-final-project/")
library(tidyverse)
library(openxlsx)

dt_survey <- as.Date("2024-07-07")

survey_list <- read.csv("data/leaked/survey_listC.txt", sep = ".")
anonymized_data <- read.csv("data/anonymized/anonymised_dataC.csv") %>% 
  select(-education) %>% 
  mutate(zip = as.character(zip))

public_data_results <- read.xlsx("data/leaked/public_data_resultsC.xlsx")

public_data_register_raw <- read.xlsx("data/leaked/public_data_registerC.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30')) %>% 
  mutate(evote = last_voted) %>% 
  filter(name %in% survey_list$name) %>% 
  select(-last_voted)

pub_eu_country <- c("Belgium", "France", "Germany", "Hungary", "Iceland","Italy","Portugal","Slovakia","Spain", "Sweden")
pub_non_eu_country <- c("Afghanistan","China","Serbia", "Syria", "Thailand", "Turkey", "United Kingdom")
age_labs = c("18-24", "25-29","30-39","40-49","50-59", '60-69', "70+")

public_data_register <- public_data_register_raw %>% 
  mutate(age = year(as.period(interval(start=dob, end=dt_survey)))) %>% 
  mutate(age = cut(age, breaks = c(18, 24, 29, 39, 49, 59, 69, Inf), labels=age_labs)) %>% 
  mutate(marital_status = case_when(
    marital_status == 'Married' ~ 'Yes',
    T ~ 'No'
  )) %>% 
  mutate(citizenship = case_when(
    citizenship %in% pub_eu_country ~ "EU",
    citizenship %in% pub_non_eu_country ~ "Non-EU",
    citizenship == 'Denmark' ~ 'Denmark'
  )) %>% 
  select(name, sex,evote,age,zip,citizenship, marital_status)




anonymized_data %>% 
  left_join(public_data_register, 
            by = c("sex","evote","age","zip","citizenship", "marital_status")) %>% 
  filter(is.na(name)) %>% 
  View()



public_data_register_raw %>% 
  count(marital_status)








