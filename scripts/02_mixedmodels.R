## This script is used to analyse the simulated data, after it has been prepared in script 01

library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)
library(broom)

load("./data/longdata.RData")

#   Main analysis
#   Defining baseline model to facilitate LRT of breastfeeding
mod1 <- lmerTest::lmer(height ~ age + sex_num + birth_length + age*sex_num + (1 | id) + (1 | fam_id), data = long_data)
summary(mod1)

#   Defining model with breastfeeding duration variable
mod2 <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod2)

#   Comparing baseline and breastfeeding models
lrt_main <- anova(mod1, mod2, test = "LRT")

#   Sensitivity 1: different binning of the breastfeeding variable

#   Defining baseline model
mod1_s1 <- lmerTest::lmer(height ~ age + sex_num + birth_length + age*sex_num + (1 | id) + (1 | fam_id), data = long_data)
summary(mod1_s1)

#   Defining model with alternative breastfeeding duration variable
mod2_s1 <- lmerTest::lmer(height ~ age + breastfeed_dur_factor_s1 + sex_num + birth_length + age*sex_num + breastfeed_dur_factor_s1*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod2_s1)

#   Comparing baseline and breastfeeding duration models
lrt_s1 <- anova(mod1_s1, mod2_s1, test = "LRT")

#   Sensitivity 2: Different threshold criteria for outliers in outcome variable

#   Defining baseline model
mod1_s2 <- lmerTest::lmer(height_s2 ~ age + sex_num + birth_length + age*sex_num + (1 | id) + (1 | fam_id), data = long_data)
summary(mod1_s2)

#   Defining model with breastfeeding duration variable
mod2_s2 <- lmerTest::lmer(height_s2 ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod2_s2)

#   Comparing baseline and breastfeeding duration models. 
lrt_s2 <- anova(mod1_s2, mod2_s2, test = "LRT")

#   Collating results

#   Load helper function
source("./scripts/helper_function.R")

# Extract summaries from each model
results_main1 <- extract_summary(mod1, "Main", "Baseline")
results_main2 <- extract_summary(mod2, "Main", "Breastfeeding")
results_s1_1 <- extract_summary(mod1_s1, "S1", "Baseline")
results_s1_2 <- extract_summary(mod2_s1, "S1", "Breastfeeding")
results_s2_1 <- extract_summary(mod1_s2, "S2", "Baseline")
results_s2_2 <- extract_summary(mod2_s2, "S2", "Breastfeeding")

# Combine results into a single table
combined_results <- bind_rows(results_main1, results_main2, results_s1_1, results_s1_2, results_s2_1, results_s2_2) %>%
  dplyr::select(analysis, model, term, Estimate = "Estimate", Std.Error = "Std. Error", t.value = "t value", p.value = "p.value")

save(combined_results, file="./data/res.RData")

# Combine LRT results into a single table
combined_results_lrt <- bind_rows(lrt_main, lrt_s1, lrt_s2)

save(combined_results_lrt, file="./data/res_lrt.RData")

# Print combined table
print(combined_results)
print(combined_results_lrt)
