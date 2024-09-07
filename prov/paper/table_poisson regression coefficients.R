rm(list=ls());gc();source(".Rprofile")


(table_df <- read_csv("prov/analysis/dsy04_neuropathy cross-sectional coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(exp(estimate),2)," (",
                          round(exp(lci),2),", ",
                          round(exp(uci),2),")")) %>% 
  dplyr::select(term,OR_CI,outcome) %>%
  pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"prov/paper/table_neuropathy cross-sectional poisson regression coefficients main clusters.csv")


(table_df <- read_csv("prov/analysis/dsy07_nephropathy cross-sectional coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(exp(estimate),2)," (",
                          round(exp(lci),2),", ",
                          round(exp(uci),2),")")) %>% 
    dplyr::select(term,OR_CI,outcome) %>%
    pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"prov/paper/table_nephropathy cross-sectional poisson regression coefficients main clusters.csv")

