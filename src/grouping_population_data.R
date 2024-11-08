library(readxl)
library(openxlsx)
library(tidyverse)
library(dplyr)

# grouping the date of birth attribute
raw_public_data <- read.xlsx("data/raw/public_data_registerU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30'))

dob_labs = c("<=1940s","1950s","1960s","1970s","1980s","1990s",">=2000s")

public_data <- raw_public_data %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs))


# grouping the citizenship column into 3 categories
country_mappings <- read.csv("src/country_mappings.csv", sep = ";")

public_data <- public_data %>%
    left_join(country_mappings, by = c("citizenship" = "Country"))

public_data <- public_data %>%
    mutate(m_region = case_when(m_region == "Non-EU" ~ "Other", 
                            m_region == "EU" ~ "Other", 
                            m_region == "Denmark" ~ "Denmark"))


# computing population frequencies
quasi_idfs <- c("sex", "last_voted", "m_dob", "zip", "m_region", "marital_status")
public_data <- public_data %>%
    group_by(across(all_of(quasi_idfs))) %>%
        mutate(F_k = n()) %>%
        ungroup()


# creating the final grouped version of the public data
public_data %>% 
  select(sex, last_voted, m_dob, zip, m_region, marital_status, F_k) %>% 

  write.xlsx("./data/modified/grouped_population_data.xlsx")