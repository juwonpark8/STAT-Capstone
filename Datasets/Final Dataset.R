library(dplyr)
library(caret)
library(randomForest)
setwd("/Users/juwonpark/Downloads")
all_units <- read.csv("allunits_puf_21.csv")
occupied_units <- read.csv("occupied_puf_21.csv")
person <- read.csv("person_puf_21.csv")
vacant_units <- read.csv("vacant_puf_21.csv")

# not using person dataset 
#merging on CONTROL variable
merged_data <- all_units %>%
  left_join(occupied_units, by = "CONTROL") %>%
  #left_join(person, by = "CONTROL") %>%
  left_join(vacant_units, by = "CONTROL")

#get rid of weights 
merged_data <- merged_data %>% 
  dplyr::select(-starts_with("FW"))
merged_data <- merged_data[, !grepl("^FW", names(merged_data))]
merged_data <- merged_data %>% 
  dplyr::select(-starts_with("PW"))
merged_data <- merged_data[, !grepl("PW", names(merged_data))]
merged_data <- merged_data %>% 
  dplyr::select(-starts_with("I_"))
merged_data <- merged_data[, !grepl("I_", names(merged_data))]

# na values 
merged_data[is.na(merged_data)] <- -999 

merged_data$BORO <- as.factor(merged_data$BORO)
merged_data

# categorize as vacant or not vacant 
# 1 = vacant 0 = not vacant
vacant_units$CONTROL<- as.character(vacant_units$CONTROL)
merged_data <- merged_data %>% 
  mutate(
    vacancy_status_all = ifelse(CONTROL %in% vacant_units$CONTROL, 1, 0))
unique(merged_data$vacancy_status_all)

# add values to the data 
mortgage_multiplier<-(1+(6.73/100/12))^(30*12)*((6.73/100/12))/((1+(6.73/100/12))^(30*12)-1)
merged_data <- merged_data %>%
  mutate(MonthlyCost = case_when(
    ASKINGRENT == 1 ~ 1100,
    ASKINGRENT == 2 ~ 1900,
    ASKINGRENT == 3 ~ 2700,
    ASKINGRENT == 4 ~ 3500,
    ASKINGRENT == 5 ~ 4300,
    ASKINGPRICE == 1 ~ 150000 * mortgage_multiplier,
    ASKINGPRICE == 2 ~ 450000 * mortgage_multiplier,
    ASKINGPRICE == 3 ~ 750000 * mortgage_multiplier,
    ASKINGPRICE == 4 ~ 1050000 * mortgage_multiplier,
    !(GRENT %in% c(-1, -2,-999)) ~ GRENT,
    !(MOWNERCOST %in% c(-1, -2, -3,-999)) ~ MOWNERCOST,
    TRUE ~ NA_real_  # Default case for missing values
  ))
merged_data <- merged_data %>% 
  mutate(MonthlyCost = case_when(
    is.na(MonthlyCost) & !(MFIRSTMORT %in% c(-1, -2, -3, -999)) ~ MFIRSTMORT,
    TRUE ~ MonthlyCost  # Keeps existing values if the condition is not met
  ))
merged_data <- merged_data %>%
  mutate(Occupancy_Change = case_when(
    VAC_LENGTH == 1 ~ 0.5,
    VAC_LENGTH == 2 ~ 1.5,
    VAC_LENGTH == 3 ~ 2.5,
    VAC_LENGTH == 4 ~ 4.5,
    VAC_LENGTH == 5 ~ 9,
    VAC_LENGTH == 6 ~ 15,
    !(HHFIRSTMOVEIN %in% c(-999)) ~ (2021-HHFIRSTMOVEIN)*12,
    TRUE ~ NA_real_  # Default case for missing values
  ))

#status_counts <- table(merged_data$vacancy_status_all)
#status_df <- as.data.frame(status_counts)
#colnames(status_df) <- c("occupancy_status", "Freq")

#ggplot(status_df, aes(x = occupancy_status, y = Freq, fill = occupancy_status)) +
#  geom_bar(stat = "identity") +
#  labs(title = "Distribution of Occupancy Status", 
#       x = "Occupancy Status", 
#       y = "Count") +
#  theme_minimal()

# colnames(merged_data)
table(merged_data$OCC) # frequency table 

# bootstrapping
total_target <- sum(merged_data$OCC == 1)

subset_data <- merged_data %>% filter(OCC %in% c(2, 3, 4))

bootstrapped_data <- subset_data %>% sample_n(size = total_target, replace = TRUE)

final_data <- bind_rows(merged_data %>% filter(OCC == 1), bootstrapped_data)

table(final_data$OCC)

# plot distribution
library(ggplot2)

status_counts <- table(final_data$vacancy_status_all)
status_df <- as.data.frame(status_counts)
colnames(status_df) <- c("occupancy_status", "Freq")

ggplot(status_df, aes(x = occupancy_status, y = Freq, fill = occupancy_status)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribution of Occupancy Status", 
       x = "Occupancy Status", 
       y = "Count") +
  theme_minimal()





