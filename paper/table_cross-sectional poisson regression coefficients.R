rm(list=ls());gc();source(".Rprofile")


read_csv("analysis/dsy03b_cross-sectional poisson regression coefficients.csv") %>% 
  mutate(coef_ci = paste0(round(exp(estimate),2)," (",
                          round(exp(estimate - 1.96*robust_se),2),", ",
                          round(exp(estimate + 1.96*robust_se),2),")")) %>% 
  dplyr::select(term,outcome,coef_ci) %>% 
  pivot_wider(names_from = c(outcome),values_from=coef_ci) %>% 
  write_csv(.,"paper/table_cross-sectional poisson regression coefficients.csv")
