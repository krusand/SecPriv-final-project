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


########################
###   Example code   ###
########################

# Example data
observed_counts <- c(120, 80, 100)  # Replace with actual observed counts for each party
expected_counts <- c(100, 100, 100)  # Replace with expected counts or use proportions

# Chi-Square Goodness of Fit Test
chisq_test <- chisq.test(x = observed_counts, p = expected_counts / sum(expected_counts))

# Results
print(chisq_test)

# Example data frame
data <- data.frame(
  party = sample(c("Party1", "Party2", "Party3"), 100, replace = TRUE),
  gender = sample(c("Male", "Female"), 100, replace = TRUE)
)
view(data)

# Create contingency table
contingency_table <- table(data$party, data$gender)

# Chi-Square Test of Independence
chisq_test_independence <- chisq.test(contingency_table)

# Results
print(chisq_test_independence)




# Questions?
# Should we include invalid votes in the total?

#####################
###   Load data   ###
#####################

# Set working directory dynamically
setwd(dirname(this.path()))

private_survey <- read_excel("data/raw/private_dataU.xlsx")
public_register <- read_excel("data/raw/public_data_registerU.xlsx")
public_results_wide <- read_excel("data/raw/public_data_resultsU.xlsx")

##########################
###   Transform data   ###
##########################

# Change name of first column in public_data_resultsU
names(public_results_wide)[1] <- "Voting_method"
names(public_results_wide)[4] <- "Invalid vote"

# Remove row and columns with sum
public_results_wide <- public_results_wide[-nrow(public_results_wide), -ncol(public_results_wide)]
view(public_results_wide)

# Transform dataframe to long format
public_results <- public_results_wide %>% 
  pivot_longer(
    cols = `Red`:`Invalid vote`, 
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
view(public_results_evote)

public_results_psvote <- public_results %>% 
  mutate(evote = case_when(
    Voting_method != "E-votes" ~ 0,
    T ~ 1,
  )) %>% 
  filter(evote == 0) %>% 
  group_by(source, party) %>% 
  summarise(count = sum(count))
view(public_results_psvote)



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

view(private_survey)

# Sex
contingency_table <- table(private_survey$party, private_survey$sex)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Education
contingency_table <- table(private_survey$party, private_survey$education)
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

contingency_table <- table(private_survey$evote, private_survey$zip)
chisq_test_independence <- chisq.test(contingency_table)
print(chisq_test_independence)

# Yes








# Your data (replace this with your actual data)
data <- data.frame(
  source = c("Result", "Result", "Result", "Survey", "Survey", "Survey"),
  party = c("Green", "Invalid vote", "Red", "Green", "Invalid vote", "Red"),
  count = c(435, 18, 238, 70, 5, 66)
)

view(data)

# Reshape the data to a wide format (contingency table format)
contingency_table <- data %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")  # Move 'source' to row names for easier readability

contingency_table <- rbind(public_results_evote, private_survey_evote) %>%
  pivot_wider(names_from = party, values_from = count, values_fill = 0) %>%
  column_to_rownames("source")


view(contingency_table)

# Convert to a matrix (chisq.test requires a matrix format)
contingency_matrix <- as.matrix(contingency_table)
contingency_matrix

# Display the contingency matrix
print("Contingency Matrix:")
print(contingency_matrix)

chisq.test(contingency_matrix)
