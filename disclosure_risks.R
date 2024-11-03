library(readxl)
library(tidyverse)
library(dplyr)

# df <- read_xlsx(path = "data/raw/private_dataU.xlsx")
# print(df)
# quasi_idfs <- c("sex", "evote", "age", "zip", "education", "citizenship", "martial_status")
# sensitive <- c("party")

# test data frame
residence <- c("Urban", "Urban", "Urban", "Urban", "Rural", "Urban", "Urban", "Urban", "Urban", "Urban")
gender <- c("Female", "Female", "Female", "Male", "Female", "Male", "Female", "Male", "Female", "Female")
education <- c("secondary incomplete", 
                "secondary incomplete", 
                "primary imcomplete", 
                "secondary complete", 
                "secondary complete", 
                "secondary complete", 
                "primary complete", 
                "post-secondary",
                "secondary incomplete",
                "secondary incomplete")
labor <- c("employed", "employed", "non-lf", "employed", "unemployed", "employed", "non-lf", "unemployed", "non-lf", "non-lf")
health <- c("yes", "yes", "yes", "yes", "yes", "no", "no", "yes", "no", "yes")
test_df <- data.frame(residence, gender, education, labor, health)
print(test_df)

# function to compute sample frequencies f_k and return the k-anonymity
get_k_anonymity <- function(df, quasi_idfs) {
    table1 <- df %>% 
        group_by(across(all_of(quasi_idfs))) %>%
        mutate(f_k = n()) %>%
        ungroup()
        select(everything(), f_k)
    print(table1)
    k_anm <- min(table1$f_k)
    return(k_anm)
}

# function to compute l-diversity for each sample and return the minimum value
get_l_diversity <- function(df, quasi_idfs, sensitive_col) {
    table1 <- df %>% 
        group_by(across(all_of(quasi_idfs)), {{ sensitive_col }}) %>%
        group_by(across(all_of(quasi_idfs))) %>%
        mutate(l_diversity = n_distinct({{ sensitive_col }})) %>%
        ungroup() %>%
        select(everything(), l_diversity)
    print(table1)
    l_div <- min(table1$l_diversity)
    return(l_div)
}

# function to compute reidentification-risks and return average risk
get_reid_risk <- function(df, pop_df, quasi_idfs) {
    population_freqs <- pop_df %>%
        group_by(across(all_of(quasi_idfs))) %>%
        mutate(F_k = n()) %>%
        ungroup()
        select(everything(), f_k)
    
   table1 <- df %>%
        left_join(population_freqs, by = quasi_idfs)
        mutate(F_k = 1/F_k)
    print(table1)
    avg_reid_risk <- mean(table1$F_k)
    return(avg_reid_risk)
}

quasi_idfs <- c("residence", "gender", "education", "labor")
sensitive_col <- health
test_k_anm <- get_sample_freqs(test_df, quasi_idfs)
print(test_k_anm)
test_l_div <- get_l_diversity(test_df, quasi_idfs, sensitive_col)
print(test_l_div)
