rm(list=ls());gc();source(".Rprofile")


(table_df <- read_csv("analysis/dsy04a_cross-sectional adding weights coefficients.csv") %>% 
  dplyr::select(iv,outcome,OR) %>% 
  pivot_wider(names_from = c(outcome),values_from=OR)) %>% 
  write_csv(.,"paper/table_cross-sectional poisson regression coefficients.csv")
