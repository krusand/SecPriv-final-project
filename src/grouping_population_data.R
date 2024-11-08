library(readxl)
library(tidyverse)
library(dplyr)

df <- openxlsx::read.xlsx("data/anonymized/anonymized_data.xlsx")

df <- df %>% 
  mutate(year=year(dob)) %>% 
  mutate(m_dob = cut(year, breaks = c(0, 1949, 1959, 1969, 1979, 1989, 1999, Inf), labels=dob_labs))


