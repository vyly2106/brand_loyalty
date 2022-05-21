### Install packages
install.packages("haven")
install.packages("dplyr")
install.packages("car")
install.packages("emmeans")
install.packages("effectsize")

library(haven)
library(dplyr)
library(car)
library(emmeans)
library(effectsize)

#########################
##### Data cleaning #####
#########################

# Import data
data <- read_sav("Exp+Res+2022+Idea+3+Manipulate+&+loyalty_May+9,+2022_14.59.sav")
final_data <- data %>%
  filter(Status==0) %>% 
  select(Prolific_ID:loyalty_DO_control, -comment)

# Recode likert scales
## Create function to recode likert scale to 1-7
recode_val <- function(col) {
  new_col <- case_when(
    col %in% -3 ~ 1,
    col %in% -2 ~ 2,
    col %in% -1 ~ 3,
    col %in% 0 ~ 4,
    col %in% 1 ~ 5,
    col %in% 2 ~ 6,
    col %in% 3 ~ 7,
  )}

## Reverse value function
reverse_val <- function(col) {
  new_col <- case_when(
    col %in% 1 ~ 7,
    col %in% 2 ~ 6,
    col %in% 3 ~ 5,
    col %in% 4 ~ 4,
    col %in% 5 ~ 3,
    col %in% 6 ~ 2,
    col %in% 7 ~ 1,
  )}

final_data <- final_data %>%
  mutate(
    loyalty_1 = recode_val(loyalty_1),
    loyalty_2 = recode_val(loyalty_2),
    reversed_loyalty_2 = reverse_val(loyalty_2),
    NFU_similarity_1 = recode_val(NFU_similarity_1),
    NFU_similarity_2 = recode_val(NFU_similarity_2),
    NFU_similarity_3 = recode_val(NFU_similarity_3),
    NFU_similarity_4 = recode_val(NFU_similarity_4)
  )

# Recode manipulation condition
final_data$loyalty_DO_higher[is.na(final_data$loyalty_DO_higher)] <- 0

# Recode gender
final_data$gender <- as.factor(final_data$gender)
final_data$gender <- factor(final_data$gender, levels = c(1,2,3,4),
                            labels = c("Male", "Female", 
                                       "Non-binary/Third gender", "Prefer not to say"))

# Categorize perceived relative rank 
#(Need to check again since the levels don't make sense)

# Create summated scale
final_data <- final_data %>%
  mutate(
    brand_loyalty = (loyalty_1 + reversed_loyalty_2) / 2,
    NFU = (
      NFU_similarity_1 + NFU_similarity_2 + NFU_similarity_3 + NFU_similarity_4
    ) / 4
  )

#####################
##### Analysis #####
####################








