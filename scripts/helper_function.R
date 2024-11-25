library(tidyverse)

# Define a function to extract summary coefficients and convert to a dataframe
extract_summary <- function(model, analysis_label) {
  coef_summary <- summary(model)$coefficients
  coef_summary <- as.data.frame(coef_summary) %>%
    rownames_to_column("term") %>%
    mutate(analysis = analysis_label,
           p.value = `Pr(>|t|)`,
           p.value = ifelse(grepl("breastfeed_dur_factor", term),
                            p.adjust(p.value, method = "bonferroni"),
                            p.value)) %>%
    dplyr::select(-`Pr(>|t|)`)
  
  
  return(coef_summary)
}