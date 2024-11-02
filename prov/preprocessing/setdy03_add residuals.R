rm(list=ls()); gc(); source(".Rprofile")

library(geepack)
library(stringr)

analytic_df = read_csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/prov/setdy02_knn imputation.csv'))

bmi_mod <-lm(bmi ~ age_category, data = analytic_df)
hba1c_mod <-lm(hba1c ~ age_category, data = analytic_df)
cpeptidef_mod <-lm(cpeptidef ~ age_category, data = analytic_df)
sbp_mod <-lm(sbp ~ age_category, data = analytic_df)
dbp_mod <-lm(dbp ~ age_category, data = analytic_df)
ldlc_mod <-lm(ldlc ~ age_category, data = analytic_df)
hdlc_mod <-lm(hdlc ~ age_category, data = analytic_df)


analytic_dataset <- analytic_df %>%
  mutate(bmi_residual = residuals(bmi_mod),
         hba1c_residual = residuals(hba1c_mod),
         cpeptidef_residual = residuals(cpeptidef_mod),
         sbp_residual = residuals(sbp_mod),
         dbp_residual = residuals(dbp_mod),
         ldlc_residual = residuals(ldlc_mod),
         hdlc_residual = residuals(hdlc_mod))

write.csv(analytic_dataset, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/setdy03_knn imputation add residuals.csv"))


bmi_mod_out = broom::tidy(bmi_mod)  
hba1c_mod_out <- broom::tidy(hba1c_mod)
cpeptidef_mod_out <- broom::tidy(cpeptidef_mod)
sbp_mod_out <- broom::tidy(sbp_mod)
dbp_mod_out <- broom::tidy(dbp_mod)
ldlc_mod_out <- broom::tidy(ldlc_mod)
hdlc_mod_out <- broom::tidy(hdlc_mod)

bind_rows(
  bmi_mod_out %>% mutate(outcome = "BMI"),
  hba1c_mod_out %>% mutate(outcome = "HbA1c"),
  cpeptidef_mod_out %>% mutate(outcome = "Fasting C-peptide"),
  sbp_mod_out %>% mutate(outcome = "SBP"),
  dbp_mod_out %>% mutate(outcome = "DBP"),
  ldlc_mod_out %>% mutate(outcome = "LDL Cholesterol"),
  hdlc_mod_out %>% mutate(outcome = "HDL Cholesterol")
) %>% 
  dplyr::filter(term != "(Intercept)") %>%
  select(outcome, term, estimate, std.error) %>% 
  write_csv(.,"prov/preprocessing/setdy03_age category coefficients.csv")


