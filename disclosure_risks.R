library(tidyverse)
library(readxl)

df <- read_xlsx(path = "data/raw/private_dataU.xlsx")
print(df)
