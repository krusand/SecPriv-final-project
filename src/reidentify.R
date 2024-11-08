# Setup
setwd("/Users/krusand/Documents/GitHub/SecPriv-final-project/")
library(tidyverse)
library(openxlsx)
library(rworldmap)
library(sdcMicro)


anonymized_data <- read.xlsx("data/anonymized/anonymized_data.xlsx")
private_data <- read.xlsx("data/raw/private_dataU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30'))

raw_public_data <- read.xlsx("data/raw/public_data_registerU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30')) %>% 
  mutate(evote = last_voted) %>% 
  filter(name %in% private_data$name)


dob_labs = c("<=1940s","1950s","1960s","1970s","1980s","1990s",">=2000s")

public_data <- raw_public_data %>% 
  mutate(m_region = case_when(
    citizenship == 'Denmark' ~ 'Denmark',
    T ~ 'Other'
  )) %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs)) %>% 
  select(name,m_dob, zip, m_region, marital_status, evote,sex)



anonymized_data %>% 
  select(m_dob,m_zip,m_citizenship_region, m_marital_status, m_evote, m_sex) %>% 
  inner_join(public_data, by = c('m_dob' = 'm_dob',
                                'm_zip' = 'zip',
                                'm_citizenship_region' = 'm_region',
                                'm_marital_status' = 'marital_status',
                                'm_evote' = 'evote',
                                'm_sex' = 'sex')) %>% 
  group_by(m_dob, m_zip, m_citizenship_region, m_marital_status,m_evote,m_sex) %>% 
  mutate(n=n()) %>% 
  View()








anonymized_data %>% 
  select(m_dob,m_zip,m_citizenship_region) %>% 
  left_join(public_data %>% 
               select(m_dob, zip, m_region, name), 
            by = c('m_dob' = 'm_dob',
                   'm_zip' = 'zip',
                   'm_citizenship_region' = 'm_region'), na_matches="na") %>% 
  group_by(m_dob, m_zip, m_citizenship_region) %>% 
  mutate(n=n()) %>% 
  View()





