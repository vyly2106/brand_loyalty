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
library(tidyverse)
library(cowplot)
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

# # Hypothesis 1: The effect of consumption on brand loyalty is mediated by perceived relative rank
# Whether relative rank can affect brand loyalty by increasing people's rank by 20 points

## Path A: Consumption/purchase on perceived relative rank
## assumptions checking:
### Assumption 1: Homoskedasticity
ggplot(final_data, aes(x=purchase, y=relative_rank_1)) +
  geom_boxplot(outlier.colour="black",
               outlier.size=2, notch=FALSE)

#### Levene's test for homogeneity
leveneTest(relative_rank_1 ~ purchase, final_data, center=mean)

### Assumption 2: Normality
#### Shapiro-Wilk test for normality
shapiro.test(final_data$relative_rank_1)
qqnorm(final_data$relative_rank_1, pch=1, frame=FALSE, xlim=c(-3,3))
qqline(final_data$relative_rank_1, col="steelblue", lwd=2)

### Anova test
purchase_pcr_model <- lm(relative_rank_1 ~ purchase, data = final_data)
Anova(purchase_pcr_model, type=3)

final_data %>%
  group_by(purchase) %>%
  summarize(mean_ranks = mean(relative_rank_1)) %>%
  ggplot(aes(x=purchase, y=mean_ranks)) +
  geom_col(fill = c("dodgerblue4", "dodgerblue2", "cornflowerblue")) +
  ylim(0,100) +
  geom_hline(yintercept=50, linetype="dashed", 
             color = "red", size=1)

emmeans(purchase_pcr_model, pairwise ~ purchase, adjust="bonferroni")

## Path B: Perceived relative rank on brand loyalty if people are told to increase their ranks by 20 points
plot_grid(ggplot(data = final_data, aes(x=loyalty_DO_higher, y=jitter(brand_loyalty))) + geom_point(), 
          ggplot(data = final_data, aes(x=relative_rank_1,y=brand_loyalty)) + geom_point())

treated_group <- final_data %>%
  filter(loyalty_DO_higher == 1) %>%
  summarize(mean_loyalty = mean(brand_loyalty))

control_group <- final_data %>%
  filter(loyalty_DO_higher == 0) %>%
  summarize(mean_loyalty = mean(brand_loyalty))

## Average treatment effect
ATE <- treated_group - control_group
ATE
### Not significantly different in perceived brand loyalty between the two groups
### Need to perform a t-test to be sure

## 2 sample t-test
st.test(final_data$brand_loyalty[final_data$loyalty_DO_higher==1],
       final_data$brand_loyalty[final_data$loyalty_DO_higher==0])
### Cannot reject H0

## Invest correlational relationship between relative rank and brand loyalty
## Check LR assumptions:
### Assumption 1: linearity
ggplot(final_data, aes(x=relative_rank_1, y=scale(brand_loyalty))) +
  geom_point() +
  geom_jitter() +
  geom_smooth(method = "lm", formula = y~x)

### Assumption 2: Homoskedasticity
plot(linear_model_1)

## Regression of brand loyalty on relative rank
linear_model_1 <- lm(scale(brand_loyalty) ~ relative_rank_1, final_data)
summary(linear_model_1)

linear_model_2 <- lm(scale(brand_loyalty) ~ relative_rank_1 + gender, final_data)
summary(linear_model_2)


# Part 2: Purchase types and NFU on Perceived relative rank
final_data$product_choice <- as.factor(final_data$product_choice)
final_data$product_choice <- factor(final_data$product_choice, levels = c(4,6,7),
                            labels = c("Coffee", "Cola", "Tea"))
final_data %>%
  count(product_choice)

final_data <- final_data %>%
  mutate(
    purchase = case_when(
      product_choice == "Coffee" ~ "High_freq_purchased",
      product_choice == "Cola" ~ "Low_freq_purchased",
      product_choice == "Tea" ~ "Med_freq_purchased"
    ),
    purchase = as.factor(purchase)
  )


final_data %>%
  group_by(purchase) %>%
  summarize(PRR = mean(relative_rank_1))




model_3 <- lm(relative_rank_1 ~ purchase+NFU, data = final_data)
summary(model_3)

model_4 <- lm(relative_rank_1 ~ NFU, data = final_data)
summary(model_4)

ggplot(final_data, aes(x=relative_rank_1)) +
  geom_histogram(binwidth = 10)














