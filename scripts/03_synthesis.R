# Create synthetic data based on simulated MoBa data

# Load required packages

# This is a function that will check to see if CRAN packages are installed.
# If they are not, they will be installed.
# After checking, they will be loaded into the R session
# Source: https://gist.github.com/stevenworthington/3178163

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

packages <- c("synthpop", "tidyverse", "cowplot", "car",
              "simstudy", "mice", "StatMeasures")
ipak(packages)

# Load data

load("./data/longdata.RData")

# Go wide to allow within-person and within-sibship correlations to be preserved in synthetic data

wide_data = long_data %>% 
  dplyr::select(id, fam_id, sex, sib, birth_length, breastfeed_dur_factor,age, height ) %>% 
  pivot_wider(names_from=age, names_prefix = "height", values_from=height) %>% 
  pivot_wider(id_cols=fam_id, names_from = sib, names_glue="{.value}_{sib}", values_from = c(id, sex, birth_length, breastfeed_dur_factor, matches("height")))

#to do the synthesising from raw data:
#load("./data/simdata.RData")
#wide_data = sim_data %>% 
#  pivot_wider(id_cols=fam_id, names_from = sib, names_glue="{.value}_{sib}", values_from = c(id, sex, birth_length, breastfeed_dur, matches("height")))

# Create synthetic data
syn_wide1 <- syn(wide_data, seed = 1234, m=1)

# Remove rows that are too similar to rows in the original data (i.e., 
# potentially disclosing data).
syn_wide1 <- sdc(syn_wide1, wide_data, rm.replicated.uniques = TRUE)


# Perform a comparison (NB this comparison only seems to work when m=1 )
sym_com_wide <- compare(
  syn_wide1, # The synthetic dataset
  wide_data, # The original dataset
  vars = c("sex_s1", "sex_s2", "birth_length_s1","birth_length_s2", "height1.5_s1","height1.5_s2",
           "breastfeed_dur_factor_s1","breastfeed_dur_factor_s2"), # The variables for comparison
  print.coef = TRUE, # Print tables of estimates for original and synthetic data
  ncol = 3, # The number of columns in the plot
  breaks = 16, # Gaps between columns 
  stat = "counts", # Present the raw counts for each variable
  cols = c("#62B6CB", "#1B4965") # Setting the colours in the plot
) # Visual comparison of o

sym_com_wide

# Create multiple synthetic datasets to test sensitivity
syn_wide <- syn(wide_data, seed = 1234, m=10)
# Remove rows that are too similar to rows in the original data (i.e., 
# potentially disclosing data).
syn_wide <- sdc(syn_wide, wide_data, rm.replicated.uniques = TRUE)

# Check inter-correlations (cross-trait within-sib, within-trait cross-sib, and cross-trait cross-sib):

corrplot::corrplot(wide_data %>% 
                     dplyr::select(-c(matches("sex|id|breast"))) %>% 
                     dplyr::select(matches("_s1"),matches("_s2")) %>% 
                     cor(., use="pairwise.complete.obs"))

corrplot::corrplot(syn_wide$syn[[1]] %>% 
                     dplyr::select(-c(matches("sex|id|breast"))) %>% 
                     dplyr::select(matches("_s1"),matches("_s2")) %>% 
                     cor(., use="pairwise.complete.obs"))
corrplot::corrplot(syn_wide$syn[[9]] %>% 
                     dplyr::select(-c(matches("sex|id|breast"))) %>% 
                     dplyr::select(matches("_s1"),matches("_s2")) %>% 
                     cor(., use="pairwise.complete.obs"))

# Cross-sib corrs are somewhat attenuated at later waves, otherwise looks ok

# Save out for validation
save(syn_wide, file= "./data/syndata.RData")
