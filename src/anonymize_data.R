# Setup
setwd("/Users/krusand/Documents/GitHub/SecPriv-final-project/")
library(tidyverse)
library(openxlsx)
library(sdcMicro)


country_region <- read_csv2("src/country_mappings.csv")

# Load data

raw_private_data <- read.xlsx("data/raw/private_dataU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30'))


# Identify and remove sensitive variables 

sensitive_variables <- c("name")

private_data <- raw_private_data %>% 
  select(-all_of(sensitive_variables))

# Global recoding + TopBottom Recoding - DOB
dob_labs = c("<=1940s","1950s","1960s","1970s","1980s","1990s",">=2000s")

private_data <- private_data %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs))

# PRAM - Marital status

possible_marital_status <- c("Never married", "Married/separated", "Divorced", "Widowed")
stay_prob <- 0.601
leave_prob <- 0.133


private_data <- private_data %>% 
  mutate(m_marital_status = case_when(
    marital_status == 'Never married' ~ sample(possible_marital_status, size=nrow(.), replace = T, prob = c(stay_prob,leave_prob,leave_prob,leave_prob)),
    marital_status == 'Married/separated' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,stay_prob,leave_prob,leave_prob)),
    marital_status == 'Divorced' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,leave_prob,stay_prob,leave_prob)),
    marital_status == 'Widowed' ~ sample(possible_marital_status, size=nrow(.), replace = T,prob = c(leave_prob,leave_prob,leave_prob,stay_prob))
  ))



# Global recoding - Citizenship

private_data <- private_data %>% 
  mutate(m_region = case_when(
    citizenship == 'Denmark' ~ 'Denmark',
    T ~ 'Other'
  ))


# PRAM - evote

possible_evote <- c(0, 1)

stay_prob <- 0.7
leave_prob <- 0.3

private_data <- private_data %>% 
  mutate(m_evote = case_when(
    evote == 0 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob)),
    evote == 1 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob))
  ))


# PRAM - party
possible_party <- c('Green','Red', 'Invalid vote')
stay_prob <- 0.7
leave_prob <- 0.3
leave_iv_prob <- 0

private_data <- private_data %>% 
  mutate(m_party = case_when(
    party == 'Green' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob, leave_iv_prob)),
    party == 'Red' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob, leave_iv_prob)),
    party == 'Invalid vote' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob, leave_prob,stay_prob))
  )) 


# PRAM - Sex

possible_sex <- c("Male", "Female")

stay_prob <- 0.7
leave_prob <- 0.3

private_data <- private_data %>% 
  mutate(m_sex = case_when(
    sex == "Male" ~ sample(possible_sex, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob)),
    sex == "Female" ~ sample(possible_sex, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob))
  ))

# Local suppression - zip

private_data <- private_data %>% 
  mutate(m_zip = case_when(
    m_region == 'Denmark' ~ zip,
    T ~ NA
  ))


private_data %>% 
  select(m_sex, m_evote, m_dob, m_zip, education, m_citizenship_region = m_region, m_marital_status, m_party) %>% 
  write.xlsx("./data/anonymized/anonymized_data.xlsx")

  
  
  
  
  
  