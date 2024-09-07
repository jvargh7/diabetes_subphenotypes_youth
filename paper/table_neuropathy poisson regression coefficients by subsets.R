rm(list=ls());gc();source(".Rprofile")


library(readr)


datasets <- list(
  etiologic_main = "etiologic/paper/table_neuropathy cross-sectional poisson regression coefficients main clusters.csv",
  etiologic_cca = "etiologic/sensitivity analysis/complete cases/table_cross-sectional poisson regression coefficients complete cases.csv",
  etiologic_seonly = "etiologic/sensitivity analysis/search only/table_cross-sectional poisson regression coefficients search only.csv",
  
  factorial_main = "factorial/paper/table_neuropathy cross-sectional poisson regression coefficients main clusters.csv",
  factorial_cca = "factorial/sensitivity analysis/complete cases/table_cross-sectional poisson regression coefficients complete cases.csv",
  factorial_seonly = "factorial/sensitivity analysis/search only/table_cross-sectional poisson regression coefficients search only.csv",
  
  prov_main = "prov/paper/table_neuropathy cross-sectional poisson regression coefficients main clusters.csv",
  prov_cca = "prov/sensitivity analysis/complete cases/table_cross-sectional poisson regression coefficients complete cases.csv",
  prov_seonly = "prov/sensitivity analysis/search only/table_cross-sectional poisson regression coefficients search only.csv"
)


labels <- c(
  "etiologic main clusters", "etiologic complete cases", "etiologic search only",
  "factorial main clusters", "factorial complete cases", "factorial search only",
  "prov main clusters", "prov complete cases", "prov search only"
)


terms_to_select <- c("clusterySIDD", "clusterySIRD")

combined_tab <- bind_rows(
  lapply(seq_along(datasets), function(i) {
    read_csv(datasets[[i]]) %>% 
      filter(term %in% terms_to_select) %>% 
      mutate(subset = labels[i])
  })
)

write_csv(combined_tab,"paper/table_neuropathy cross-sectional poisson regression coefficients by subsets.csv")