rm(list=ls());gc();source(".Rprofile")


library(readr)


datasets <- list(
  etiologic_seonly = "etiologic/paper/table_nephropathy cross-sectional poisson regression coefficients main clusters.csv",
  factorial_seonly = "factorial/paper/table_nephropathy cross-sectional poisson regression coefficients main clusters.csv",
  prov_seonly = "prov/paper/table_nephropathy cross-sectional poisson regression coefficients main clusters.csv"
)


labels <- c("etiologic search only", "factorial search only", "prov search only")


terms_to_select <- c("clusteryIDD", "clusteryIRD")

combined_tab <- bind_rows(
  lapply(seq_along(datasets), function(i) {
    read_csv(datasets[[i]]) %>% 
      filter(term %in% terms_to_select) %>% 
      mutate(subset = labels[i])
  })
)

write_csv(combined_tab,"paper/table_nephropathy cross-sectional poisson regression coefficients by subsets.csv")