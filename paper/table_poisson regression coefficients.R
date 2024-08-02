rm(list=ls());gc();source(".Rprofile")


(table_df <- read_csv("analysis/dsy04_cross-sectional coefficients.csv") %>% 
  dplyr::select(iv,outcome,OR) %>% 
  pivot_wider(names_from = c(outcome),values_from=OR)) %>% 
  write_csv(.,"paper/table_cross-sectional poisson regression coefficients.csv")



(table_longitudinal_df <- read_csv("analysis/dsy05_longitudinal coefficients.csv") %>% 
    dplyr::select(iv,outcome,OR) %>% 
    pivot_wider(names_from = c(outcome),values_from=OR)) %>% 
  write_csv(.,"paper/table_longitudinal poisson regression coefficients.csv")