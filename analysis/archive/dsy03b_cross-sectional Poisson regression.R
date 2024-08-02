rm(list=ls());gc();source(".Rprofile")

library(geepack)
library(sandwich)
library(nlme)

cross_weight <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS")) %>% 
  dplyr::filter(combined_available == 1) %>% 
  mutate(cluster = factor(cluster, levels = c("OB", "ID", "IR")),  # Including 'relevel' by setting the levels directly
         cluster = relevel(cluster, ref = "OB"))
  

# 1. Cross-sectional associations (n ~ 722)
# a) Prevalence ratios using Poisson regression based on cutoff

### Questionnaire
survey_mod <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth, 
                     data = cross_weight, 
                     family = poisson(link = "log"), 
                     id = cross_weight$cluster,
                     weights = cross_weight$weight, 
                     corstr = "independence")
summary(survey_mod)
survey_robust_se <- sqrt(diag(vcov(survey_mod)))


# Transforming the dependent variable
cross_weight$survey_abnormal_transformed <- sqrt(cross_weight$survey_abnormal + 0.5)  # Adding 0.5 to avoid taking sqrt of 0

# Fit the GLS model
survey_mod1 <- gls(survey_abnormal_transformed ~ cluster + age_category + female + race_eth,
                  data = cross_weight,
                  correlation = corCompSymm(form = ~ 1 | cluster),  # This assumes compound symmetry of errors within clusters
                  weights = varIdent(form = ~ 1 | cluster))  # Allows different variances per cluster

# Summary of the model
summary(survey_mod1)
survey_robust_se1 <- sqrt(diag(vcov(survey_mod1)))

### Examination
exam_mod <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth, 
                     data = cross_weight, 
                     family = poisson(link = "log"), 
                     id = cross_weight$cluster,
                     weights = cross_weight$weight, 
                     corstr = "independence")
summary(exam_mod)
exam_robust_se <- sqrt(diag(vcov(exam_mod)))

### Combined
combined_mod <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth, 
                     data = cross_weight, 
                     family = poisson(link = "log"), 
                     id = cross_weight$cluster,
                     weights = cross_weight$weight, 
                     corstr = "independence")
summary(combined_mod)
combined_robust_se <- sqrt(diag(vcov(combined_mod)))



bind_rows(tidy(survey_mod, exp = FALSE) %>% mutate(outcome = "survey_abnormal",robust_se = survey_robust_se),
          tidy(exam_mod, exp = FALSE) %>% mutate(outcome = "exam_abnormal",robust_se = exam_robust_se),
          tidy(combined_mod, exp = FALSE) %>% mutate(outcome = "combined_abnormal",robust_se = combined_robust_se)) %>% 
  write_csv(.,"analysis/dsy03b_cross-sectional poisson regression coefficients.csv")
