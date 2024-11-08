library(readxl)
library(tidyverse)
library(dplyr)

df <- openxlsx::read.xlsx("data/anonymized/anonymized_data.xlsx")
print(df)
quasi_idfs <- c("m_sex", "m_evote", "m_dob", "zip", "education", "m_citizenship_region", "m_marital_status")
sensitive_col <- c("m_party")

# function to compute sample frequencies f_k and return the k-anonymity
get_k_anonymity <- function(df, quasi_idfs) {
    table1 <- df %>% 
        group_by(across(all_of(quasi_idfs))) %>%
        mutate(f_k = n()) %>%
        ungroup() %>%
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
    l_div <- min(table1$l_diversity)
    return(l_div)
}

# function to compute reidentification-risks and return average risk
get_reid_risk <- function(df, pop_df, quasi_idfs) {
    population_freqs <- pop_df %>%
        group_by(across(all_of(quasi_idfs))) %>%
        mutate(F_k = n()) %>%
        ungroup()
        select(everything(), F_k)
    
   table1 <- df %>%
        left_join(population_freqs, by = quasi_idfs)
        mutate(F_k = 1/F_k)
    avg_reid_risk <- mean(table1$F_k)
    return(avg_reid_risk)
}

k_anm <- get_k_anonymity(df, quasi_idfs)
print(paste0("The k-anonymity of the data frame is ", k_anm))
l_div <- get_l_diversity(df, quasi_idfs, sensitive_col)
print(paste0("The l-diversity of the data frame is ", l_div))
