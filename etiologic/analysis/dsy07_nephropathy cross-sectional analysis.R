rm(list=ls());gc();source(".Rprofile")

library(geepack)
library(stringr)

completed_data <- readRDS("etiologic/analysis/dsy06_nephropathy mi_dfs.RDS") %>%
  mutate(cluster = factor(cluster,levels=c("yMOD","ySIDD","ySIRD")),
         race_eth = factor(race_eth,levels=c("NH White","NH Black","Hispanic","NH Other"))) 


neph_mod <- geeglm(nephropathy_diag ~ cluster + age_category + female + race_eth, 
                   data = completed_data, 
                   family = poisson(link = "log"), 
                   id = study_id,
                   corstr = "independence")

neph_mod_out = broom::tidy(neph_mod) %>% 
  mutate(outcome = "nephropathy_abnormal") %>% 
  write_csv(.,"etiologic/analysis/dsy07_nephropathy cross-sectional coefficients.csv")