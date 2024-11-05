library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)

load("./data/syndata.RData")

#   Main analysis
#   Defining model
mod_syn <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = syn_long$syn)
summary(mod_syn)


#   Sensitivity 1: different binning of the breastfeeding variable

#   Defining model
mod_s1_syn <- lmerTest::lmer(height ~ age + breastfeed_dur_factor_s1 + sex_num + birth_length + age*sex_num + breastfeed_dur_factor_s1*sex_num+ (1 | id) + (1 | fam_id), data = syn_long$syn)
summary(mod_s1_syn)


#   Sensitivity 2: Different threshold criteria for outliers in outcome variable

#   Defining model
mod_s2_syn <- lmerTest::lmer(height_s2 ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = syn_long$syn)
summary(mod_s2_syn)


#   Collating results

#   Load helper function
source("./scripts/helper_function.R")

# Extract summaries from each model
results_main_syn <- extract_summary(mod_syn, "Main syn")
results_s1_syn <- extract_summary(mod_s1_syn, "Sensitivity 1 syn")
results_s2_syn <- extract_summary(mod_s2_syn, "Sensitivity 2 syn")

# Combine results into a single table
combined_results_syn <- bind_rows(results_main_syn, results_s1_syn, results_s2_syn) %>%
  select(analysis, term, Estimate = "Estimate", Std.Error = "Std. Error", t.value = "t value", p.value = "p.value")


# Print combined table
print(combined_results_syn)
