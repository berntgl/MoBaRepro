## This script is used to analyse the simulated data

library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)

load("./data/longdata.RData")

#   Defining model
mod <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = long_data)
summary(mod)

#   Add the Bonferroni correction to the factored variable (breastfeeding duration)
p_values <- summary(mod)$coefficients[, "Pr(>|t|)"]
breastfeeding_p_values <- p_values[grep("breastfeed_dur_factor", names(p_values))]

#   Adjusted p-values resulting from Bonferroni correction
adjusted_p_values <- p.adjust(breastfeeding_p_values, method = "bonferroni")

#   Compare unadjusted and adjusted p-values
results <- data.frame(
  Comparison = names(breastfeeding_p_values),
  Original_P = breastfeeding_p_values,
  Bonferroni_Adjusted_P = adjusted_p_values
)

print(results)

