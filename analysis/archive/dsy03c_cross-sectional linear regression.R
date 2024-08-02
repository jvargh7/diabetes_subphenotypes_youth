rm(list=ls());gc();source(".Rprofile")

library(geepack)

cross_weight <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS")) %>% 
  dplyr::filter(combined_available == 1) %>% 
  mutate(cluster = factor(cluster, levels = c("OB", "ID", "IR")),  # Including 'relevel' by setting the levels directly
         cluster = relevel(cluster, ref = "OB"))

### cross-sectional linear regression with inverse probability weights for not having MNSI data (geeglm, gaussian)

survey_mod <- geeglm(survey_score ~ cluster + age_category + female + race_eth, 
                     data = cross_weight, 
                     family = gaussian(link = "identity"),  # Using Gaussian family with identity link
                     id = cross_weight$cluster,
                     weights = cross_weight$weight, 
                     corstr = "independence") 

summary(survey_mod)

exam_mod <- geeglm(exam_score ~ cluster + age_category + female + race_eth, 
                     data = cross_weight, 
                     family = gaussian(link = "identity"),  # Using Gaussian family with identity link
                     id = cross_weight$cluster,
                     weights = cross_weight$weight, 
                     corstr = "independence") 

summary(exam_mod)
