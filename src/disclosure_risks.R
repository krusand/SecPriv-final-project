library(readxl)
library(tidyverse)
library(dplyr)

# loading the anonymized dataset as a data frame
df <- openxlsx::read.xlsx("data/anonymized/anonymized_data.xlsx")
print(colnames(df))

# loading the grouped population data as a data frame
pop_df <- openxlsx::read.xlsx("data/modified/grouped_population_data.xlsx")
print(colnames(pop_df))

# defining the quasi-identifiers and the sensitive column
quasi_idfs <- c("m_sex", "m_evote", "m_dob", "m_zip", "education", "m_citizenship_region", "m_marital_status")
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
        ungroup() %>%
        select(everything(), F_k) %>%
        distinct(across(all_of(quasi_idfs)), .keep_all = TRUE)

    table1 <- df %>%
        left_join(population_freqs, by = quasi_idfs, relationship = "many-to-one") %>%
        mutate(F_k = case_when(is.na(F_k) ~ 0, 
                                F_k > 0 ~ 1/F_k))
  
    avg_reid_risk <- mean(table1$F_k, na.rm = TRUE)
    avg_reid_risk_no_zeros <- mean(table1[table1$F_k > 0, "F_k"])
    return(list(avg_reid_risk = avg_reid_risk, avg_reid_risk_no_zeros = avg_reid_risk_no_zeros))
}


# computing and printing the k-anonymity of the anonymized dataset
k_anm <- get_k_anonymity(df, quasi_idfs)
print(paste0("The k-anonymity of the data frame is ", k_anm))

# computing and printing the l-diversity of the anonymized dataset
l_div <- get_l_diversity(df, quasi_idfs, sensitive_col)
print(paste0("The l-diversity of the data frame is ", l_div))

# computing and printing the average reidentification risk of the anonymized dataset
quasi_idfs_reid <- c("m_sex", "m_evote", "m_dob", "m_zip", "m_citizenship_region", "m_marital_status")
results <- get_reid_risk(df, pop_df, quasi_idfs_reid)
avg_reid_risk <- results$avg_reid_risk
avg_reid_risk_no_zeros <- results$avg_reid_risk_no_zeros
avg_reid_risk_rounded <- round(avg_reid_risk, 3)
avg_reid_risk_no_zeros_rounded <- round(avg_reid_risk_no_zeros, 3)
print(paste0("The average reidentification risk is ", avg_reid_risk_rounded))
print(paste0("The average reidentification risk is when ignoring population frequencies of 0 is ", avg_reid_risk_no_zeros_rounded))