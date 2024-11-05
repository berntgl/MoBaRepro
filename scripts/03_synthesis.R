# Synthetic data


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


load("./data/longdata.RData")

syn_long <- syn(long_data, seed = 1234)

save(syn_long, file= "./data/syndata.RData")

sim_com_long <- compare(
  syn_long, # The synthetic dataset
  long_data, # The original dataset
  vars = c("fam_id", "sex",
           "birth_length", "age",
           "height", "sex_num", "breastfeed_dur_factor", 
           "breastfeed_dur_factor_s1", "height_s2"), # The variables for comparison
  print.coef = TRUE, # Print tables of estimates for original and synthetic data
  ncol = 3, # The number of columns in the plot
  breaks = 16, # Gaps between columns 
  stat = "counts", # Present the raw counts for each variable
  cols = c("#62B6CB", "#1B4965") # Setting the colours in the plot
) # Visual comparison of o

sim_com_long

s1 <- lm.synds(height ~ age + sex_num + birth_length + 
           (1 | id) + (1 | fam_id), data = syn_long)

s1a <- lm.synds(height ~ sex_num + birth_length + 
                 (1 | id) + (1 | fam_id), data = syn_long)

t1 <- compare(
  s1, # Results from the synthetic linear model
  long_data, # The original dataset
  lwd = 1.5, # The type of line in the plot
  lty = 1, # The width of line in the plot
  point.size = 4, # The size of the symbols used in the plot
  lcol = c("#62B6CB", "#1B4965") # Set the colours
) # A comparison of the linear models

t1

t1a <- compare(
  s1a, # Results from the synthetic linear model
  long_data, # The original dataset
  lwd = 1.5, # The type of line in the plot
  lty = 1, # The width of line in the plot
  point.size = 4, # The size of the symbols used in the plot
  lcol = c("#62B6CB", "#1B4965") # Set the colours
) 

