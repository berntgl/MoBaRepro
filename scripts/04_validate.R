# Validate that consisent results are produced using synthetic data (vs original simulated data)

library(lme4)
library(tidyverse)
library(lmerTest)
library(multcomp)

load("./data/syndata.RData")

source("./scripts/04.1_syn_dataprep.R")

all = map(syn_wide$syn, prep_syndata) 
#all= map(syn_wide$syn, prep_syndata_raw)  # Use if the synthesis was done based on raw rather than processed data

#   Main analysis
#   Defining model
mod_syn <- function(x){
  mod <- lmerTest::lmer(height ~ age + breastfeed_dur_factor + sex_num + birth_length + age*sex_num + breastfeed_dur_factor*sex_num+ (1 | id) + (1 | fam_id), data = x)
  summary(mod)}

all_mod = map(all, mod_syn)


#   Collating results

#   Load helper function
source("./scripts/helper_function.R")

# Extract summaries from each model
results_main_syn <- map(all_mod, extract_summary, "Main syn")

# Combine results into a single table
combined_results_syn <- bind_rows(results_main_syn) %>% 
  mutate(analysis= rep(paste0("Syn",seq(1,10)), each=9 ),
         type="synthetic")  %>%
  dplyr::select(analysis,type, term, Estimate = "Estimate", Std.Error = "Std. Error", t.value = "t value", p.value = "p.value") 

# Print combined table
print(combined_results_syn)

# Compare with results from original data

load("./data/res.RData")

combined_results_all = combined_results %>%
  mutate(type="original") %>% 
  bind_rows(combined_results_syn) %>% 
  mutate(analysis=factor(analysis))

ggplot(combined_results_all %>% 
         filter(str_detect(analysis,"Main|Syn"),
                term!="(Intercept)"),
       aes(x=Estimate, y= term, fill= analysis, colour = analysis,alpha=type))+
  geom_errorbarh(aes(xmin = Estimate -1.96*Std.Error, xmax = Estimate + 1.96*Std.Error), height=0, linewidth=1.5, position=position_dodge(0.5))+
  geom_point(shape=21, size= 2, stroke= 1, colour="grey10", position= position_dodge(0.5))+
  scale_alpha_manual(values=c(1,0.2))+
  theme_classic()

# Overall the synthetic data broadly replicates the main results (age, sex age), with a very small
# bias towards underestimation in both cases
