rm(list=ls());gc();source(".Rprofile")

(table_df <- read_csv("etiologic/preprocessing/setdy03_age category coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(estimate,2)," (",
                          round(lci,2),", ",
                          round(uci,2),")")) %>% 
    dplyr::select(term,OR_CI,outcome) %>%
    pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"etiologic/paper/table_age category linear regression coefficients main clusters.csv")


(table_df <- read_csv("etiologic/analysis/dsy04_neuropathy cross-sectional coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(exp(estimate),2)," (",
                          round(exp(lci),2),", ",
                          round(exp(uci),2),")")) %>% 
  dplyr::select(term,OR_CI,outcome) %>%
  pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"etiologic/paper/table_neuropathy cross-sectional poisson regression coefficients main clusters.csv")


(table_df <- read_csv("etiologic/analysis/dsy07_nephropathy cross-sectional coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(exp(estimate),2)," (",
                          round(exp(lci),2),", ",
                          round(exp(uci),2),")")) %>% 
    dplyr::select(term,OR_CI,outcome) %>%
    pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"etiologic/paper/table_nephropathy cross-sectional poisson regression coefficients main clusters.csv")

