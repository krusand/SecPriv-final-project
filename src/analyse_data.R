#install.packages("tidyverse")
#install.packages("ggplot")
#install.packages("readxl")
#install.packages("here")
#install.packages("this.path")

library(tidyverse)
library(ggplot2)
library(readxl)
library(here)
library(this.path)



# Questions?
# Should we include invalid votes in the total?

#####################
###   Load data   ###
#####################

# Set working directory dynamically
setwd(dirname(this.path()))

private_survey <- read_excel("../data/raw/private_dataU.xlsx")
public_register <- read_excel("../data/raw/public_data_registerU.xlsx")
public_results_wide <- read_excel("../data/raw/public_data_resultsU.xlsx")

#view(private_survey)

##########################
###   Transform data   ###
##########################

# Remove invalid votes
private_survey <- private_survey %>% 
  filter(party!="Invalid vote")

# Change name of first column in public_data_resultsU
names(public_results_wide)[1] <- "Voting_method"
names(public_results_wide)[4] <- "Invalid vote"

# Remove row and columns with sum
public_results_wide <- public_results_wide[-nrow(public_results_wide), c(-4,-5)]

# Transform dataframe to long format
public_results <- public_results_wide %>% 
  pivot_longer(
    cols = `Red`:`Green`, 
    names_to = "party",
    values_to = "count"
  ) %>% 
  mutate(source = "Result")
#view(public_results)



#############
###   A   ###
#############

# Is there a significant difference between the political preferences as expressed in the survey 
# and the election results for both electronic and polling station votes?

public_results_evote <- public_results %>% 
  mutate(evote = case_when(
    Voting_method != "E-votes" ~ 0,
    T ~ 1,
  )) %>% 
  filter(evote == 1) %>% 
  group_by(source, party) %>% 
  summarise(count = sum(count))
#view(public_results_evote)

public_results_psvote <- public_results %>% 
  mutate(evote = case_when(
    Voting_method != "E-votes" ~ 0,
    T ~ 1,
  )) %>% 
  filter(evote == 0) %>% 
  group_by(source, party) %>% 
  summarise(count = sum(count))
#view(public_results_psvote)



private_survey_evote <- private_survey %>% 
  select(evote, party) %>% 
  filter(evote == 1) %>% 
  group_by(party) %>% 
  summarise(count = n()) %>% 
  mutate(source = "Survey")
#view(private_survey_evote)

private_survey_psvote <- private_survey %>% 
  select(evote, party) %>% 
  filter(evote == 0) %>% 
  group_by(party) %>% 
  summarise(count = n()) %>% 
  mutate(source = "Survey")
  
# Create contingency tables
contingency_table_evote <- rbind(public_results_evote, private_survey_evote) %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")

contingency_table_evote

contingency_table_psvote <- rbind(public_results_psvote, private_survey_psvote) %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")

# Chi-Square Test of Independence
# source, party, count
chisq_test_independence_evote <- chisq.test(contingency_table_evote)
chisq_test_independence_psvote <- chisq.test(contingency_table_psvote)

# Results
print(chisq_test_independence_evote)
print(chisq_test_independence_psvote)



#############
###   B   ###
#############

# Is there a significant difference between political preferences of the voters depending on their 
# demographic attributes recorded in the survey (that is, age, gender, education level…)?

# We use test of independence, because we want to test if the variables are independent of each other.

# Sex
contingency_table <- table(private_survey$party, private_survey$sex)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Flag

# DOB
contingency_table <- table(private_survey$party, private_survey$dob)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# ZIP
contingency_table <- table(private_survey$party, private_survey$zip)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Education
contingency_table <- table(private_survey$party, private_survey$education)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Flag

# Citizenship
contingency_table <- table(private_survey$party, private_survey$citizenship)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Marital status
contingency_table <- table(private_survey$party, private_survey$marital_status)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Flag

#############
###   C   ###
#############

# Is there a significant difference between voter’s choice of the voting channel (that is, if they 
# decide to vote either online or in person) depending on their demographic attributes recorded in 
# the survey?

# Sex
contingency_table <- table(private_survey$evote, private_survey$sex)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# DOB
contingency_table <- table(private_survey$evote, private_survey$dob)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# ZIP
contingency_table <- table(private_survey$evote, private_survey$zip)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Education
contingency_table <- table(private_survey$evote, private_survey$education)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Citizenship
contingency_table <- table(private_survey$evote, private_survey$citizenship)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Marital status
contingency_table <- table(private_survey$evote, private_survey$marital_status)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)





#####################
###   Load data   ###
#####################

# Set working directory dynamically
setwd(dirname(this.path()))

private_survey <- read_excel("../data/anonymized/anonymized_data.xlsx")
public_results_wide <- read_excel("../data/raw/public_data_resultsU.xlsx")

names(private_survey)[1] <- "sex"
names(private_survey)[2] <- "evote"
names(private_survey)[3] <- "dob"
names(private_survey)[4] <- "zip"
names(private_survey)[5] <- "education"
names(private_survey)[6] <- "citizenship_region"
names(private_survey)[7] <- "marital_status"
names(private_survey)[8] <- "party"


##########################
###   Transform data   ###
##########################

# Remove invalid votes
private_survey <- private_survey %>% 
  filter(party!="Invalid vote")

# Change name of first column in public_data_resultsU
names(public_results_wide)[1] <- "Voting_method"
names(public_results_wide)[4] <- "Invalid vote"

# Remove row and columns with sum
public_results_wide <- public_results_wide[-nrow(public_results_wide), c(-4,-5)]

# Transform dataframe to long format
public_results <- public_results_wide %>% 
  pivot_longer(
    cols = `Red`:`Green`, 
    names_to = "party",
    values_to = "count"
  ) %>% 
  mutate(source = "Result")
#view(public_results)



#############
###   A   ###
#############

# Is there a significant difference between the political preferences as expressed in the survey 
# and the election results for both electronic and polling station votes?

public_results_evote <- public_results %>% 
  mutate(evote = case_when(
    Voting_method != "E-votes" ~ 0,
    T ~ 1,
  )) %>% 
  filter(evote == 1) %>% 
  group_by(source, party) %>% 
  summarise(count = sum(count))
#view(public_results_evote)

public_results_psvote <- public_results %>% 
  mutate(evote = case_when(
    Voting_method != "E-votes" ~ 0,
    T ~ 1,
  )) %>% 
  filter(evote == 0) %>% 
  group_by(source, party) %>% 
  summarise(count = sum(count))
# view(public_results_psvote)

private_survey_evote <- private_survey %>% 
  select(evote, party) %>% 
  filter(evote == 1) %>% 
  group_by(party) %>% 
  summarise(count = n()) %>% 
  mutate(source = "Survey")

private_survey_psvote <- private_survey %>% 
  select(evote, party) %>% 
  filter(evote == 0) %>% 
  group_by(party) %>% 
  summarise(count = n()) %>% 
  mutate(source = "Survey")

# Create contingency tables
contingency_table_evote <- rbind(public_results_evote, private_survey_evote) %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")

contingency_table_evote

contingency_table_psvote <- rbind(public_results_psvote, private_survey_psvote) %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")

# Chi-Square Test of Independence
# source, party, count
chisq_test_independence_evote <- chisq.test(contingency_table_evote)
chisq_test_independence_psvote <- chisq.test(contingency_table_psvote)

# Results
print(chisq_test_independence_evote)
print(chisq_test_independence_psvote)

# For the electronic vote we reject the null hypothesis: We cannot reject that there is a difference 
# in preferences for the electronic votes.

# For the polling station votes, we cannot reject the null hypothesis.



#############
###   B   ###
#############

# Is there a significant difference between political preferences of the voters depending on their 
# demographic attributes recorded in the survey (that is, age, gender, education level…)?

# We use test of independence, because we want to test if the variables are independent of each other.

# Sex
contingency_table <- table(private_survey$party, private_survey$sex)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# DOB
contingency_table <- table(private_survey$party, private_survey$dob)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# ZIP
contingency_table <- table(private_survey$party, private_survey$zip)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Education
contingency_table <- table(private_survey$party, private_survey$education)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Citizenship
contingency_table <- table(private_survey$party, private_survey$citizenship_region)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Marital status
contingency_table <- table(private_survey$party, private_survey$marital_status)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)


#############
###   C   ###
#############

# Is there a significant difference between voter’s choice of the voting channel (that is, if they 
# decide to vote either online or in person) depending on their demographic attributes recorded in 
# the survey?

# Sex
contingency_table <- table(private_survey$evote, private_survey$sex)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# DOB
contingency_table <- table(private_survey$evote, private_survey$dob)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# ZIP
contingency_table <- table(private_survey$evote, private_survey$zip)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Education
contingency_table <- table(private_survey$evote, private_survey$education)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Citizenship
contingency_table <- table(private_survey$evote, private_survey$citizenship_region)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Marital status
contingency_table <- table(private_survey$evote, private_survey$marital_status)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)


#################
###   Stats   ###
#################
# Set working directory dynamically
setwd(dirname(this.path()))

private_survey <- read_excel("../data/raw/private_dataU.xlsx")
public_results_wide <- read_excel("../data/raw/public_data_resultsU.xlsx")

private_survey <- private_survey %>% 
  filter(party!="Invalid vote")
view(private_survey)

# Evote
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, evote) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(evote, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Sex
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, sex) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(sex, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()
  
# DOB
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, dob) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(dob, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# ZIP
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, zip) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(zip, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Education
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, education) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(education, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Citizenship
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, citizenship) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(citizenship, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Marital status
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, marital_status) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(marital_status, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()



#################
###   Stats   ###
#################
# Set working directory dynamically
setwd(dirname(this.path()))

private_survey <- read_excel("../data/anonymized/anonymized_data.xlsx")
public_results_wide <- read_excel("../data/raw/public_data_resultsU.xlsx")

names(private_survey)[1] <- "sex"
names(private_survey)[2] <- "evote"
names(private_survey)[3] <- "dob"
names(private_survey)[4] <- "zip"
names(private_survey)[5] <- "education"
names(private_survey)[6] <- "citizenship_region"
names(private_survey)[7] <- "marital_status"
names(private_survey)[8] <- "party"

private_survey <- private_survey %>% 
  filter(party!="Invalid vote")
view(private_survey)

# Sex
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, sex) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(sex, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# DOB
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, dob) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(dob, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# ZIP
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, zip) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(zip, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Education
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, education) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(education, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Citizenship
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, citizenship_region) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(citizenship_region, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()

# Marital status
private_survey %>% 
  group_by(party) %>% 
  mutate(n_voted_party=n()) %>% 
  group_by(party, marital_status) %>% 
  mutate(n=n()) %>% 
  mutate(n_freq=n/n_voted_party) %>% 
  distinct(marital_status, party, n_freq) %>% 
  pivot_wider(names_from=c('party'), values_from = n_freq) %>% 
  view()
