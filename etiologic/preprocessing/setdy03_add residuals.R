rm(list=ls()); gc(); source(".Rprofile")

library(haven)

analytic_df = read_csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/etiologic/setdy02_knn imputation.csv'))

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

write.csv(analytic_dataset, paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/setdy03_knn imputation add residuals.csv"))
