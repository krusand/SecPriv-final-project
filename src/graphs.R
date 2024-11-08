# Setup
setwd("/Users/krusand/Documents/GitHub/SecPriv-final-project/")
library(tidyverse)
library(openxlsx)
library(ggthemes)

raw_private_data <- read.xlsx("data/raw/private_dataU.xlsx") %>% 
  mutate(dob = as.Date(dob, origin = '1899-12-30'))


raw_private_data %>% 
  mutate(year = year(dob)) %>%
  
  ggplot(aes(x=year)) +
  geom_vline(xintercept = 1949, color = "firebrick", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = 1999, color = "firebrick", linetype = "dashed", size = 1) + 
  geom_rect(aes(xmin = 1920, xmax = 1949, ymin = 0, ymax = 19), fill='lightcoral', alpha=0.005) +
  geom_rect(aes(xmin = 1999, xmax = 2010, ymin = 0, ymax = 19), fill='lightcoral', alpha=0.005) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
  ylab("#persons with birth year") + 
  xlab("Birth year") +
  ggtitle("We do top and bottom recoding. Red lines signify where top and bottom recoding is performed") +
  theme_minimal()






    


