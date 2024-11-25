## This script is used to analyse the simulated data, after it has been prepared in script 01

library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)
library(broom)

load("./data/longdata.RData")

#   Main analysis
#   Defining model
mod <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod)


#   Sensitivity 1: different binning of the breastfeeding variable

#   Defining model
mod_s1 <- lmerTest::lmer(height ~ age + breastfeed_dur_factor_s1 + sex_num + birth_length + age*sex_num + breastfeed_dur_factor_s1*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod_s1)


#   Sensitivity 2: Different threshold criteria for outliers in outcome variable

#   Defining model
mod_s2 <- lmerTest::lmer(height_s2 ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod_s2)


#   Collating results

#   Load helper function
source("./scripts/helper_function.R")

# Extract summaries from each model
results_main <- extract_summary(mod, "Main")
results_s1 <- extract_summary(mod_s1, "Sensitivity 1")
results_s2 <- extract_summary(mod_s2, "Sensitivity 2")

# Combine results into a single table
combined_results <- bind_rows(results_main, results_s1, results_s2) %>%
  dplyr::select(analysis, term, Estimate = "Estimate", Std.Error = "Std. Error", t.value = "t value", p.value = "p.value")

save(combined_results, file="./data/res.RData")

# Print combined table
print(combined_results)
