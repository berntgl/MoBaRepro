#Prepare the synthetic data for validation analyses

library(tidyverse)

prep_syndata = function(x){
  syn_data <- x %>% 
    mutate(fam_id = seq(1, nrow(x)),
           id_s1 = NA, 
           id_s2 = NA) %>% 
    pivot_longer(-fam_id, names_sep = "_s", names_to = c(".value","sib")) %>% 
    drop_na(sex) 
  
  syn_data <- syn_data %>% 
    mutate(id= seq(1, nrow(syn_data)))
  
  
  # 114013 unique IDs (i.e., participants)
  length(unique(syn_data$fam_id))
  
  #   Reshape the data into long format
  long_data <- syn_data %>%
    gather(key = "age", value = "height", starts_with("height"), -id, -birth_length,-sex,-fam_id,  -breastfeed_dur_factor)
  
  #   Arrange by ID
  long_data <- long_data %>% 
    arrange(id)
  
  #   Make numeric sex variable 
  long_data$sex_num <- ifelse(long_data$sex == "Male", 0, 1)
  
  #   Mutate age column so each measurement reflects age in years
  long_data <- long_data %>% 
    mutate(age=as.numeric(str_remove_all(age, "height")))
    

  
  return(long_data)
}
  

prep_syndata_raw = function(x){
  #First go back from widest format to the raw format (1 row per individual)
  
  syn_data <- x %>% 
    mutate(fam_id = seq(1, nrow(x)),
           id_s1 = NA, 
           id_s2 = NA) %>% 
    pivot_longer(-fam_id, names_sep = "_s", names_to = c(".value","sib")) %>% 
    drop_na(sex) 
  
  syn_data <- syn_data %>% 
    mutate(id= seq(1, nrow(syn_data)))
  
  
  # 114013 unique IDs (i.e., participants)
  length(unique(syn_data$id))
  
  
  #   Checking descriptive statistics and distribution for each variable.
  ##  6 months
  summary(syn_data$height_6mo)
  hist(syn_data$height_6mo)
  
  ##  18 months
  summary(syn_data$height_18m)
  hist(syn_data$height_18m)
  
  ##  3 years
  summary(syn_data$height_3yr)
  hist(syn_data$height_3yr)
  
  ##  5 years
  summary(syn_data$height_5yr)
  hist(syn_data$height_5yr)
  
  ##  7 years
  summary(syn_data$height_7yr)
  hist(syn_data$height_7yr)
  
  ##  8 years
  summary(syn_data$height_8yr)
  hist(syn_data$height_8yr)
  
  ##  Sex
  summary(factor(syn_data$sex))
  
  ##  Birth length
  summary(syn_data$birth_length)
  hist(syn_data$birth_length)
  
  ##  Breastfeeding duration
  summary(syn_data$breastfeed_dur)
  hist(syn_data$breastfeed_dur)
  
  
  ### preprocessing
  
  #   Everything looks normally distributed, except for breastfeeding duration.
  #   Instead of treating breastfeeding duration as a continuous variable, we 
  #   will recode it as a categorical variable, with non-/early cessation (0-1 months),
  #   up to 6 months (between 2 and 6 months), and extended duration (> 6 months).
  
  syn_data <- syn_data %>% 
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
  long_data <- syn_data %>%
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
  
  syn_long_data <- long_data %>% 
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
    dplyr::select(-mean_height, -sd_height) %>% 
    ungroup()
  
  return(syn_long_data)
}