library(dplyr)
library(caret)
library(randomForest)

setwd("~/Desktop/STAT 4996/Housing Vacancy Data /2021 Data")

all_units <- read.csv("allunits_puf_21.csv")
occupied_units <- read.csv("occupied_puf_21.csv")
person <- read.csv("person_puf_21.csv")
vacant_units <- read.csv("vacant_puf_21.csv")

#merging on CONTROL variable
merged_data <- all_units %>%
  left_join(occupied_units, by = "CONTROL") %>%
  left_join(person, by = "CONTROL") %>%
  left_join(vacant_units, by = "CONTROL")

#get rid of weights 
merged_data <- merged_data %>% 
  select(-starts_with("FW"))
merged_data <- merged_data[, !grepl("^FW", names(merged_data))]


merged_data[is.na(merged_data)] <- -999 

merged_data$BORO <- as.factor(merged_data$BORO)
merged_data

vacant_units <- merged_data %>% 
  mutate(CONTROL = as.character(CONTROL)) 
merged_data <- vacant_units %>% 
  mutate(
    vacancy_status_all = ifelse(CONTROL %in% vacant_units$CONTROL, 1, 0)
  )
merged_data$vacancy_status_all <- as.factor(merged_data$vacancy_status_all)