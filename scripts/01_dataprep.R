# Prepare the simulated data for analysis

library(tidyverse)

load("./data/simdata.RData")

# 114013 unique IDs (i.e., participants)
length(unique(sim_data$id))


#   Checking descriptive statistics and distribution for each variable.
##  6 months
summary(sim_data$height_6mo)
hist(sim_data$height_6mo)

##  18 months
summary(sim_data$height_18m)
hist(sim_data$height_18m)

##  3 years
summary(sim_data$height_3yr)
hist(sim_data$height_3yr)

##  5 years
summary(sim_data$height_5yr)
hist(sim_data$height_5yr)

##  7 years
summary(sim_data$height_7yr)
hist(sim_data$height_7yr)

##  8 years
summary(sim_data$height_8yr)
hist(sim_data$height_8yr)

##  Sex
summary(factor(sim_data$sex))

##  Birth length
summary(sim_data$birth_length)
hist(sim_data$birth_length)

##  Breastfeeding duration
summary(sim_data$breastfeed_dur)
hist(sim_data$breastfeed_dur)


### preprocessing

#   Everything looks normally distributed, except for breastfeeding duration.
#   Instead of treating breastfeeding duration as a continuous variable, we 
#   will recode it as a categorical variable, with non-/early cessation (0-1 months),
#   up to 6 months (between 2 and 6 months), and extended duration (> 6 months).

sim_data <- sim_data %>% 
  mutate(
    breastfeed_dur_factor=factor(case_when(
      breastfeed_dur == 0 | breastfeed_dur == 1 ~ 0,
      breastfeed_dur >= 2 & breastfeed_dur <= 6 ~ 1,
      breastfeed_dur > 6 ~ 2)),
    breastfeed_dur_factor_s1=factor(case_when(
      breastfeed_dur < 6 ~ 0,
      breastfeed_dur == 6 ~ 1,
      breastfeed_dur > 6 ~ 2))
    )

#   Reshape the data into long format
long_data <- sim_data %>%
  gather(key = "age", value = "height", starts_with("height_"), -id, -birth_length,-sex,-fam_id, -breastfeed_dur, -breastfeed_dur_factor, -breastfeed_dur_factor_s1)

#   Arrange by ID
long_data <- long_data %>% 
  arrange(id)

#   Make numeric sex variable 
long_data$sex_num <- ifelse(long_data$sex == "Male", 0, 1)

#   Mutate age column so each measurement reflects age in years
long_data <- long_data %>% 
  mutate(age=case_when(
    age == "height_6mo" ~ 0.5,
    age == "height_18m" ~ 1.5,
    age == "height_3yr" ~ 3,
    age == "height_5yr" ~ 5,
    age == "height_7yr" ~ 7,
    age == "height_8yr" ~ 8,
    TRUE ~ as.numeric(gsub("height_","",age))
    )
  )


#   We will also remove all outliers above 3 SD for our main analysis, as well
#   as all outliers above 2 SD for our sensitivity analysis.

long_data <- long_data %>% 
  group_by(age) %>% 
  mutate(mean_height = mean(height, na.rm = TRUE),
         sd_height = sd(height, na.rm = TRUE),
         height = ifelse(
           abs(height - mean_height) <= 3 * sd_height, 
           height, 
           NA),
         height_s2 = ifelse(
           abs(height - mean_height) <= 2 * sd_height, 
           height, 
           NA)
  ) %>% 
  select(-mean_height, -sd_height) %>% 
  ungroup()

#   Save the processed data.
save(long_data, file = "./data/longdata.RData")

