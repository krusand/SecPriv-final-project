# Setup
setwd("/Users/krusand/Documents/GitHub/SecPriv-final-project/")
library(tidyverse)
library(openxlsx)
library(rworldmap)
library(sdcMicro)

data(countryExData)

country_region <- countryExData %>% 
  select(Country, m_region = GEO_subregion)

# Some countries not available, add these:

missing_countries <- tibble(
  Country = c("Somalia", "Vietnam", "Yugoslavia"),
  m_region = c("Eastern Africa", 'South East Asia', 'Central Europe')
)

country_region <- bind_rows(list(country_region, missing_countries))

# Load data

raw_private_data <- read.xlsx("data/raw/private_dataU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30'))


# Identify and remove sensitive variables 

sensitive_variables <- c("name")

private_data <- raw_private_data %>% 
  select(-all_of(sensitive_variables))

# Global recoding + TopBottom Recoding - DOB
dob_labs = c("<=1940s","1950s","1960s","1970s","1980s","1990s",">=2000s")

private_data %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = 
           cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs)) %>% 
  View()

# PRAM - Marital status

possible_marital_status <- c("Never married", "Married/separated", "Divorced", "Widowed")
stay_prob <- 0.7
leave_prob <- 0.1


private_data %>% 
  mutate(m_marital_status = case_when(
    marital_status == 'Never married' ~ sample(possible_marital_status, size=nrow(.), replace = T, prob = c(stay_prob,leave_prob,leave_prob,leave_prob)),
    marital_status == 'Married/separated' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,stay_prob,leave_prob,leave_prob)),
    marital_status == 'Divorced' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,leave_prob,stay_prob,leave_prob)),
    marital_status == 'Widowed' ~ sample(possible_marital_status, size=nrow(.), replace = T,prob = c(leave_prob,leave_prob,leave_prob,stay_prob))
  )) %>% 
  add_count(m_marital_status) %>% 
  add_count(marital_status) %>% 
  View()



# Global recoding - Citizenship

private_data %>% 
  group_by(citizenship) %>% 
  left_join(country_region, by=c('citizenship' = 'Country')) %>%
  group_by(m_region) %>% 
  summarise(n=n()) %>% 
  View()



# PRAM - evote

possible_evote <- c(0,1)

stay_prob <- 1-sum(private_data$evote)/nrow(private_data)
leave_prob <- sum(private_data$evote)/nrow(private_data)

private_data %>% 
  mutate(m_evote = case_when(
    evote == 0 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob)),
    evote == 1 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob))
  )) %>%
  count(evote,m_evote) %>% 
  View()


# PRAM - party
possible_party <- c('Green','Red', 'Invalid vote')
stay_prob <- 0.9
leave_prob <- 0.05

private_data %>% 
  mutate(m_party = case_when(
    party == 'Green' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob, leave_prob)),
    party == 'Red' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob, leave_prob)),
    party == 'Invalid vote' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob, leave_prob,stay_prob))
  )) %>%
  count(m_party) %>% 
  View()

