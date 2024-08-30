rm(list=ls());gc();source(".Rprofile")


# Loss to follow-up weights
crosssec_df_cca <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/complete cases/cca01_cross sectional complete cases df.RDS")) %>% 
  dplyr::select(-"nonna_selfmnsi") %>% 
  mutate(cs_available = case_when(is.na(combined_abnormal) ~ 0,
                                  TRUE ~ 1)) %>% 
  mutate(cluster = factor(cluster,levels=c("yMOD","ySIDD","ySIRD")),
         race_eth = factor(race_eth,levels=c("NH White","NH Black","Hispanic","NH Other"))) %>% 
  # convert to numberic
  mutate(study_id = case_when(study == "TODAY" ~ str_replace_all(study_id, "-", ""),
                              TRUE ~ study_id)) %>% 
  mutate(study_id = as.numeric(study_id))


cs_available_mod <- glm(cs_available ~ cluster + age_category + female + race_eth + study, 
                     data = crosssec_df_cca, 
                     family = poisson(link = "log"))
crosssec_df_cca$ltfu_prob = predict(cs_available_mod,newdata=crosssec_df_cca,type="response")
crosssec_df_cca$ltfu_weight = 1/crosssec_df_cca$ltfu_prob

library(stringr)
 
  
  survey_mod <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth, 
                            data = crosssec_df_cca, 
                            family = poisson(link = "log"), 
                            # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                            id = study_id,
                            weights = ltfu_weight, 
                            corstr = "independence")
  exam_mod <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth, 
                          data = crosssec_df_cca, 
                          family = poisson(link = "log"), 
                          # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                          id = study_id,
                          weights = ltfu_weight, 
                          corstr = "independence")
  combined_mod <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth, 
                              data = crosssec_df_cca, 
                              family = poisson(link = "log"), 
                              # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                              id = study_id,
                              weights = ltfu_weight, 
                              corstr = "independence")



survey_mod_out = broom::tidy(survey_mod)  
exam_mod_out = broom::tidy(exam_mod)  
combined_mod_out = broom::tidy(combined_mod)  
  

bind_rows(
 
  survey_mod_out %>% mutate(outcome = "survey_abnormal"),
  exam_mod_out %>% mutate(outcome = "exam_abnormal"),
  combined_mod_out %>% mutate(outcome = "combined_abnormal")
   
) %>% 
  write_csv(.,"etiologic/sensitivity analysis/complete cases/cca02_cross-sectional coefficients.csv")
