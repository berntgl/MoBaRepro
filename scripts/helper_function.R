library(tidyverse)

# Define a function to extract summary coefficients and convert to a dataframe
extract_summary <- function(model, analysis_label, model_label) {
  coef_summary <- summary(model)$coefficients
  coef_summary <- as.data.frame(coef_summary) %>%
    rownames_to_column("term") %>%
    mutate(analysis = analysis_label,
           model = model_label,
           p.value = `Pr(>|t|)`) %>%
    dplyr::select(-`Pr(>|t|)`)
  
  
  return(coef_summary)
}