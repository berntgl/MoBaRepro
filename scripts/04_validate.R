library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)

load("./data/syndata.RData")

#   Defining model using synthetic data.
mod_syn <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = syn_long$syn)
summary(mod_syn)

#   Add the Bonferroni correction.
p_values_syn <- summary(mod_syn)$coefficients[, "Pr(>|t|)"]

breastfeeding_p_values_syn <- p_values_syn[grep("breastfeed_dur_factor", names(p_values_syn))]

#   Adjusted p-values resulting from Bonferroni correction
adjusted_p_values_syn <- p.adjust(breastfeeding_p_values_syn, method = "bonferroni")

results_syn <- data.frame(
  Comparison = names(breastfeeding_p_values_syn),
  Original_P = breastfeeding_p_values_syn,
  Bonferroni_Adjusted_P = adjusted_p_values_syn
)

print(results_syn)
