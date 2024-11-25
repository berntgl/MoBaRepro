## Script to simulate a "MoBa" dataset based on real sample characteristics for MoBa repro project demo analyses

# install.packages("devtools")
# devtools::install_github("debruine/faux")

library(tidyverse)
library(faux)

## Section 1: simulate the raw data

set.seed(43142) 

# Set the number of rows
total_rows <- 114013
# Removing 16% of the total, who are siblings that we simulate later
reduced_rows <- total_rows-round(total_rows*0.16, 0)

# Generate a tibble with a unique ID variable - 
sim_data <- tibble(id = 1:reduced_rows)

# Create a "sex" variable with the desired Male:Female ratio
male_ratio <- 0.523  # Desired Male:Female ratio
sim_data <- sim_data %>%
  mutate(sex = sample(c("Male", "Female"), reduced_rows, replace = TRUE, prob = c(male_ratio, 1 - male_ratio)))


# Generate the "birth_length" variable with the specified mean and standard deviation
sim_data <- sim_data %>%
  mutate(birth_length = rnorm(n(), mean = 50.18, sd = 2.87))


# Generate the height variables, with simulated correlations to birth_length and each other 

sim_data <- sim_data %>%
  mutate(height_6mo = rnorm_pre(birth_length, mu = 67.74, sd = 2.71, r=0.3),
         height_18m = rnorm_pre(height_6mo, mu = 80.63,  sd = 3.71, r =rnorm(1,0.6,0.05)),
         height_3yr = rnorm_pre(height_18m, mu = 96.59,  sd = 6.12, r =rnorm(1,0.6,0.05)),
         height_5yr = rnorm_pre(height_3yr, mu = 113.07, sd = 6.25, r =rnorm(1,0.8,0.05)),
         height_7yr = rnorm_pre(height_5yr, mu = 125.84, sd = 6.87, r =rnorm(1,0.8,0.05)),
         height_8yr = rnorm_pre(height_7yr, mu = 132.02, sd = 6.66, r =rnorm(1,0.8,0.05))
  )


# Generate the breastfeeding variable 
sim_data <- sim_data %>% 
  mutate(breastfeed_dur = round(rnorm_pre(height_6mo, mu = 6, sd = 3.4, r =.12),0))


# Add siblings

sim_data <- sim_data %>% 
  rename_with(~ paste0(.,"__s1")) %>% 
  mutate(id__s2 = seq(reduced_rows+1,reduced_rows*2),
         sex__s2 = sample(c("Male", "Female"), reduced_rows, replace = TRUE, prob = c(male_ratio, 1 - male_ratio)),
         birth_length__s2 = rnorm_pre(birth_length__s1, mu = 50.18, sd = 2.87, r=0.3))

# We have to proceed in separate chunks, as rnorm_pre only takes data.frames or single vectors, 
# and we want variables to be correlated within individuals and across sibships

sim_data <- sim_data %>% 
  mutate(height_6mo__s2 = rnorm_pre(sim_data %>% select(height_6mo__s1, birth_length__s2), mu = 67.74, sd = 2.71, r=c(0.2,0.3) ))

sim_data <- sim_data %>% 
  mutate(height_18m__s2 = rnorm_pre(sim_data %>% select(height_18m__s1, height_6mo__s2), mu = 80.63,  sd = 3.71, r=c(0.3,0.6) ))

sim_data <- sim_data %>% 
  mutate(height_3yr__s2 = rnorm_pre(sim_data %>% select(height_3yr__s1, height_18m__s2), mu = 96.59,  sd = 6.12,  r=c(0.4,0.6) ))

sim_data <- sim_data %>% 
  mutate(height_5yr__s2 = rnorm_pre(sim_data %>% select(height_5yr__s1, height_3yr__s2), mu = 113.07, sd = 6.25, r=c(0.4,0.8) ))

sim_data <- sim_data %>% 
  mutate(height_7yr__s2 = rnorm_pre(sim_data %>% select(height_7yr__s1, height_5yr__s2), mu = 125.84, sd = 6.87, r=c(0.5,0.8) ))

sim_data <- sim_data %>% 
  mutate(height_8yr__s2 = rnorm_pre(sim_data %>% select(height_8yr__s1, height_7yr__s2), mu = 132.02, sd = 6.66, r=c(0.5,0.8) ))

sim_data <- sim_data %>% 
  mutate(breastfeed_dur__s2 = rnorm_pre(sim_data %>% select(breastfeed_dur__s1, height_6mo__s2), mu = 6, sd = 3.4, r=c(0.8,0.12) )) %>% 
  pivot_longer(everything(), names_sep = "__", names_to = c(".value","sib")) %>% 
  mutate(fam_id = rep(seq(1,reduced_rows), each=2)) %>% 
  filter(!(fam_id %in% sample(rep(seq(1,reduced_rows)), reduced_rows-round(total_rows*0.16, 0)) & sib=="s2")) %>%  #drop a random subset of simulated siblings to return us to the correct sample size
  mutate(id = seq(1, total_rows)) %>% 
  select(id,fam_id, sex,everything())

###########

## Section 2: add in sex effects, missingness, and mess up the simulated data in various ways

# Specify the missingness patterns for height, conditional on missingness on previous waves

nmissing_6mo =
  slice_sample(sim_data, n= 84056)
nmissing_18m =
  slice_sample(nmissing_6mo, n= 64086)
nmissing_3yr =
  slice_sample(nmissing_18m, n= 48447)
nmissing_5yr =
  slice_sample(nmissing_18m, n= 39336)
nmissing_7yr =
  slice_sample(nmissing_18m, n= 47252)
nmissing_8yr =
  slice_sample(nmissing_7yr, n= 37433)

sim_data <- sim_data %>% 
  mutate(height_6mo = ifelse(id%in%nmissing_6mo$id, height_6mo, NA),
         height_18m = ifelse(id%in%nmissing_18m$id, height_18m, NA),
         height_3yr = ifelse(id%in%nmissing_3yr$id, height_3yr, NA),
         height_5yr = ifelse(id%in%nmissing_5yr$id, height_5yr, NA),
         height_7yr = ifelse(id%in%nmissing_7yr$id, height_7yr, NA),
         height_8yr = ifelse(id%in%nmissing_8yr$id, height_8yr, NA))

# Add some measurement/data entry error into the height variables

mess_up=function(x,y){
  case_when(y>0.9997 ~ round(x+rnorm(1,70,5),0),
            y<0.0005 ~ round(x-rnorm(1,70,5),0),
            TRUE ~ x)
}

sim_data <- sim_data %>% 
  pivot_longer(cols=matches("height")) %>% 
  mutate(rand = runif(total_rows*6),
         value = mess_up(value,rand) ) %>% 
  select(-rand) %>% 
  pivot_wider(names_from=name, values_from = value)

## Adjust the height variables based on sex

# This is a constant describing, in terms of half of the mean difference, how different males and females should be
avg_diff = 0.8

# This parameterises the interaction between age and sex 
# It is defined as a per-year change in the sex difference
age_sex = 0.15


sim_data <- sim_data %>%
  mutate(birth_length = ifelse(sex == "Male", 
                               birth_length + rnorm(total_rows, avg_diff, 0.05),
                               birth_length - rnorm(total_rows, avg_diff, 0.05)),
         height_6mo = ifelse(sex == "Male", 
                             height_6mo + rnorm(total_rows, 5*age_sex*avg_diff, 0.05*2.71),
                             height_6mo - rnorm(total_rows, .5*age_sex*avg_diff, 0.05*2.71)),
         height_18m = ifelse(sex == "Male", 
                             height_18m + rnorm(total_rows, 1.5*age_sex*avg_diff, 0.05*3.71),
                             height_18m - rnorm(total_rows, 1.5*age_sex*avg_diff, 0.05*3.71)),
         height_3yr = ifelse(sex == "Male", 
                             height_3yr + rnorm(total_rows, 3*age_sex*avg_diff, 0.05*6.12),
                             height_3yr - rnorm(total_rows, 3*age_sex*avg_diff, 0.05*6.12)),
         height_5yr = ifelse(sex == "Male", 
                             height_5yr + rnorm(total_rows, 5*age_sex*avg_diff, 0.05*6.25),
                             height_5yr - rnorm(total_rows, 5*age_sex*avg_diff, 0.05*6.25)),
         height_7yr = ifelse(sex == "Male", 
                             height_7yr + rnorm(total_rows, 7*age_sex*avg_diff, 0.05*6.87),
                             height_7yr - rnorm(total_rows, 7*age_sex*avg_diff, 0.05*6.87)),
         height_8yr = ifelse(sex == "Male", 
                             height_8yr + rnorm(total_rows, 8*age_sex*avg_diff, 0.05*6.66),
                             height_8yr - rnorm(total_rows, 8*age_sex*avg_diff, 0.05*6.66))) 

# Verify

psych::describe.by(sim_data, "sex")

# Adjust the breastfeeding variable - NB, this is deliberately simulated to be non-normally distributed, in order
# that it is necessary to deviate from our example preregistration and categorise the variable, as opposed to treating
# it as continuous as planned, thus representing a realistic scenario and allowing us to demonstrate how to handle such
# a situation

# To do this, we simulated an underlying distribution with a small relationship to height at 6mo, then reflect both
# limtations of the measurement of breastfeeding in MoBa (monthwise until 6 months, then every 2 months until 18months
# and not again after) and the (hypothetically unanticipated) higher prevalence of 0 or short duration breastfeeding

sim_data <- sim_data %>% 
  mutate(breastfeed_dur = round(rnorm_pre(height_6mo, mu = 6, sd = 3.4, r =.12),0),
         rand = runif(total_rows)) %>% 
  mutate(breastfeed_dur = case_when(breastfeed_dur<0 ~ 0,
                                    (breastfeed_dur>1&breastfeed_dur<6)&rand<0.25 ~ 1,
                                    (breastfeed_dur>1&breastfeed_dur<6)&rand>0.5 ~ 2,
                                    breastfeed_dur>18 ~ 18,
                                    breastfeed_dur>6&breastfeed_dur<8 ~7,
                                    breastfeed_dur>8&breastfeed_dur<10 ~9,
                                    breastfeed_dur>10&breastfeed_dur<12 ~11,
                                    breastfeed_dur>12&breastfeed_dur<14 ~13,
                                    breastfeed_dur>14&breastfeed_dur<16 ~15,
                                    breastfeed_dur>16&breastfeed_dur<18 ~17,
                                    TRUE ~ breastfeed_dur)) %>% 
  select(-rand)

#Finally check overall descriptives

psych::describe(sim_data)


# View and save the resulting data frame
head(sim_data)

save(sim_data, file= "./data/simdata.RData")
