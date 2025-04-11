# Validate that consisent results are produced using synthetic data (vs original simulated data)

library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)

load("./data/syndata.RData")

source("./scripts/04.1_syn_dataprep.R")

all <- map(syn_wide$syn, prep_syndata) 
#all <- map(syn_wide$syn, prep_syndata_raw)  # Use if the synthesis was done based on raw rather than processed data


#######

# Define a function that fits both models and returns them in a list
fit_models <- function(data) {
  
  # Filter out all rows with incomplete data
  relevant_vars <- c("height", "age", "breastfeed_dur_factor", "sex_num", "birth_length", "id", "fam_id")
  
  filtered_data <- data %>% 
    dplyr::select(all_of(relevant_vars)) %>% 
    na.omit()
  
  # Defining baseline model without breastfeeding duratoin variable
  all_mod1 <- lmerTest::lmer(height ~ age + sex_num + birth_length + age*sex_num + (1 | id) + (1 | fam_id), data = filtered_data)
  
  # Defining model with breastfeeding duration variable
  all_mod2 <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = filtered_data)
  
  return(list(all_mod1 = all_mod1, all_mod2 = all_mod2))
}

# Fit both models for each dataset
all_models <- map(all, fit_models)

# Run ANOVA on both models
results_syn_lrt <- map(all_models, ~ anova(.x$all_mod1, .x$all_mod2, test = "LRT"))


#   Collating results
#   Load helper function
source("./scripts/helper_function.R")

# Extract summaries from each model
results_main1_syn <- map(all_models, ~ extract_summary(.x$all_mod1, "Syn", "Baseline"))
results_main2_syn <- map(all_models, ~ extract_summary(.x$all_mod2, "Syn", "Breastfeeding"))

# Combine results into a single table
combined_results_syn <- bind_rows(
  results_main1_syn %>% 
    bind_rows() %>% 
    mutate(analysis = rep(paste0("Syn_",seq(1,10)), each=5), type = "synthetic"),
  results_main2_syn %>% 
    bind_rows() %>% 
    mutate(analysis = rep(paste0("Syn_",seq(1,10)), each=9), type = "synthetic")) %>%
  dplyr::select(analysis, model, type, term, Estimate = "Estimate", Std.Error = "Std. Error", t.value = "t value", p.value = "p.value")

# Print combined table
print(combined_results_syn)

# Compare with results from original data

load("./data/res.RData")

combined_results_all <- combined_results %>%
  mutate(type="original") %>% 
  bind_rows(combined_results_syn) %>% 
  mutate(analysis=factor(analysis))

save(combined_results_all, file = "./data/res_all.RData")


# Combine LRT results

combined_results_syn_lrt <- bind_rows(results_syn_lrt)

print(combined_results_syn_lrt)

load("./data/res_lrt.RData")

combined_results_all_lrt <- combined_results_lrt %>%
  mutate(type="original") %>% 
  bind_rows(combined_results_syn_lrt)

print(combined_results_all_lrt)

save(combined_results_all_lrt, file = "./data/res_all_lrt.RData")


# Compare synthetic data to original data

# Baseline
ggplot(combined_results_all %>% 
         filter(str_detect(analysis,"Main|Syn"),
                term!="(Intercept)",
                model  == "Baseline"),
       aes(x=Estimate, y= term, fill= analysis, colour = analysis,alpha=type))+
  geom_errorbarh(aes(xmin = Estimate -1.96*Std.Error, xmax = Estimate + 1.96*Std.Error), height=0, linewidth=1.5, position=position_dodge(0.5))+
  geom_point(shape=21, size= 2, stroke= 1, colour="grey10", position= position_dodge(0.5))+
  scale_alpha_manual(values=c(1,0.2))+
  theme_classic()

# Breastfeeding
ggplot(combined_results_all %>% 
         filter(str_detect(analysis,"Main|Syn"),
                term!="(Intercept)",
                model  == "Breastfeeding"),
       aes(x=Estimate, y= term, fill= analysis, colour = analysis,alpha=type))+
  geom_errorbarh(aes(xmin = Estimate -1.96*Std.Error, xmax = Estimate + 1.96*Std.Error), height=0, linewidth=1.5, position=position_dodge(0.5))+
  geom_point(shape=21, size= 2, stroke= 1, colour="grey10", position= position_dodge(0.5))+
  scale_alpha_manual(values=c(1,0.2))+
  theme_classic()


# Overall the synthetic data broadly replicates the main results (age, sex age), with a very small
# bias towards underestimation in both cases
