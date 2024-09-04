rm(list=ls());gc();source(".Rprofile")


(table_df <- read_csv("factorial/analysis/dsy04_cross-sectional coefficients.csv") %>% 
    mutate(lci = estimate - 1.96*std.error,
           uci = estimate + 1.96*std.error) %>% 
    mutate(OR_CI = paste0(round(exp(estimate),2)," (",
                          round(exp(lci),2),", ",
                          round(exp(uci),2),")")) %>% 
    dplyr::select(term,OR_CI,outcome) %>%
    pivot_wider(names_from = c(outcome),values_from=OR_CI)) %>% 
  write_csv(.,"factorial/paper/table_cross-sectional poisson regression coefficients main clusters.csv")
