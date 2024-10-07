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


#   There seem to be some outliers in the height variables. We'll remove all
#   values that are more than 3 SD away from the mean.

#   Creating a list of all height variables.
height_variables <- grep("height", names(sim_data), value = TRUE)

#   Removing all outliers
remove_outliers <- function() {
  for (var in height_variables) {
    # Calculate the mean and standard deviation for the variable
    mean_value <- mean(sim_data[[var]], na.rm = TRUE)
    sd_value <- sd(sim_data[[var]], na.rm = TRUE)
    
    # Define the lower and upper bounds (3 SD from the mean)
    lower_bound <- mean_value - 3 * sd_value
    upper_bound <- mean_value + 3 * sd_value
    
    # Filter the data to remove outliers for the specific variable
    sim_data[[var]][sim_data[[var]] < lower_bound | sim_data[[var]] > upper_bound] <<- NA
  }
}

remove_outliers()

#   Everything looks normally distributed, except for breastfeeding duration.
#   Instead of treating breastfeeding duration as a continuous variable, we 
#   will recode it as a categorical variable, with non-/early cessation (0-1 months),
#   up to 6 months (between 2 and 6 months), and extended duration (> 6 months).

sim_data <- sim_data %>% mutate(breastfeed_dur_factor=factor(case_when(
  breastfeed_dur == 0 | breastfeed_dur == 1 ~ 0,
  breastfeed_dur >= 2 & breastfeed_dur <= 6 ~ 1,
  breastfeed_dur > 6 ~ 2), 
  ordered=TRUE))



#   Reshape the data into long format
long_data <- sim_data %>%
  gather(key = "timepoint", value = "height", starts_with("height_"), -id, -birth_length,-sex,-fam_id, -breastfeed_dur, -breastfeed_dur_factor)

#   Arrange by ID
long_data <- long_data %>% arrange(id)

#   Make numeric sex variable 
long_data$sex_num <- ifelse(long_data$sex == "Male", 0, 1)

#   Rename "timepoint" column to "age" 
long_data <- long_data %>% rename(age=timepoint)

#   Mutate age column so each measurement reflects age in years
long_data <- long_data %>% mutate(age=case_when(
  age == "height_6mo" ~ 0.5,
  age == "height_18m" ~ 1.5,
  age == "height_3yr" ~ 3,
  age == "height_5yr" ~ 5,
  age == "height_7yr" ~ 7,
  age == "height_8yr" ~ 8,
  TRUE ~ as.numeric(gsub("height_","",age))
))


#   Save the processed data.
save(long_data, file = "./data/longdata.RData")
