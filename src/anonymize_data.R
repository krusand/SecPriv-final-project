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

private_data <- private_data %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs))

# PRAM - Marital status

possible_marital_status <- c("Never married", "Married/separated", "Divorced", "Widowed")
stay_prob <- 0.7
leave_prob <- 0.1


private_data <- private_data %>% 
  mutate(m_marital_status = case_when(
    marital_status == 'Never married' ~ sample(possible_marital_status, size=nrow(.), replace = T, prob = c(stay_prob,leave_prob,leave_prob,leave_prob)),
    marital_status == 'Married/separated' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,stay_prob,leave_prob,leave_prob)),
    marital_status == 'Divorced' ~ sample(possible_marital_status, size=nrow(.),replace = T, prob = c(leave_prob,leave_prob,stay_prob,leave_prob)),
    marital_status == 'Widowed' ~ sample(possible_marital_status, size=nrow(.), replace = T,prob = c(leave_prob,leave_prob,leave_prob,stay_prob))
  ))



# Global recoding - Citizenship

private_data <- private_data %>% 
  group_by(citizenship) %>% 
  left_join(country_region, by=c('citizenship' = 'Country')) %>% 
  ungroup()



# PRAM - evote

possible_evote <- c(0, 1)

stay_prob <- 0.9
leave_prob <- 0.1

private_data <- private_data %>% 
  mutate(m_evote = case_when(
    evote == 0 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob)),
    evote == 1 ~ sample(possible_evote, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob))
  ))


# PRAM - party
possible_party <- c('Green','Red', 'Invalid vote')
stay_prob <- 0.9
leave_prob <- 0.05

private_data <- private_data %>% 
  mutate(m_party = case_when(
    party == 'Green' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob, leave_prob)),
    party == 'Red' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob, leave_prob)),
    party == 'Invalid vote' ~ sample(possible_party, size=nrow(.), replace=T, prob=c(leave_prob, leave_prob,stay_prob))
  )) 


# PRAM - Sex

possible_sex <- c("Male", "Female")

stay_prob <- 0.9
leave_prob <- 0.1

private_data <- private_data %>% 
  mutate(m_sex = case_when(
    sex == "Male" ~ sample(possible_sex, size=nrow(.), replace=T, prob=c(stay_prob, leave_prob)),
    sex == "Female" ~ sample(possible_sex, size=nrow(.), replace=T, prob=c(leave_prob,stay_prob))
  ))


private_data %>% 
  select(m_sex, m_evote, m_dob, zip, education, m_citizenship_region = m_region, m_marital_status, m_party) %>% 
  write.xlsx("./data/anonymized/anonymized_data.xlsx")




# SDC micro

private_data_sdc_pram <- private_data %>% 
  mutate(zip = as.numeric(zip)) %>% 
  mutate(evote = as.factor(evote)) %>% 
  mutate(sex=as.factor(sex)) %>% 
  mutate(education=as.factor(education)) %>% 
  mutate(citizenship=as.factor(citizenship)) %>% 
  mutate(marital_status=as.factor(marital_status))

private_data_sdc_ls <- private_data %>% 
  mutate(zip = as.numeric(zip)) %>% 
  mutate(evote = as.character(evote)) %>% 
  mutate(sex=as.character(sex)) %>% 
  mutate(education=as.character(education)) %>% 
  mutate(citizenship=as.character(citizenship)) %>% 
  mutate(marital_status=as.character(marital_status))
selectedKeyVars = c('sex', 'evote', 'dob', 'zip', 'education', 'citizenship', 'marital_status')


# selected pram variables
selectedPramVars = c('evote', 'sex',  'education', 'citizenship', 'marital_status')

# sensitive variables for l-diversity computation
selectedSensibleVar = c('party')

sdcInitial <- createSdcObj(dat         = private_data_sdc_ls,
                           keyVars     = selectedKeyVars,
                           pramVars    = selectedPramVars,
                           sensibleVar = selectedSensibleVar)

sdcInitial


sdcInitial <- pram(obj = sdcInitial)

print(sdcInitial, type="pram")


localSuppression(obj = sdcInitial, k = 2)






