rm(list=ls());gc();source(".Rprofile")

mi_dfs <- readRDS("factorial/analysis/dsy03_mi_dfs.RDS") 

# Loss to follow-up weights
crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/factorial/dsy01_cross sectional df.RDS")) %>% 
  dplyr::select(-"nonna_selfmnsi") %>% 
  mutate(cs_available = case_when(is.na(combined_abnormal) ~ 0,
                                  TRUE ~ 1))

cs_available_mod <- glm(cs_available ~ cluster + age_category + female + race_eth + study, 
                     data = crosssec_df, 
                     family = poisson(link = "log"))
crosssec_df$ltfu_prob = predict(cs_available_mod,newdata=crosssec_df,type="response")
crosssec_df$ltfu_weight = 1/crosssec_df$ltfu_prob


library(geepack)
library(stringr)
  
  completed_data <- complete(mi_dfs, action = 1) %>%
    # dplyr::filter(earliest == 1) %>% 
    mutate(cluster = factor(cluster,levels=c("yMOD","ySIDD","ySIRD")),
           race_eth = factor(race_eth,levels=c("NH White","NH Black","Hispanic","NH Other"))) %>% 
    ### Examination
    mutate(Deformities = case_when(obsmnsir1 == 1 | obsmnsil1 == 1 ~ 1,
                                   obsmnsir1 == 0 & obsmnsil1 == 0 ~ 0,
                                   TRUE ~ NA_real_),
           Dryskin = case_when(obsmnsir2 == 1 | obsmnsil2 == 1 ~ 1,
                               obsmnsir2 == 0 & obsmnsil2 == 0 ~ 0,
                               TRUE ~ NA_real_),
           Infection = case_when(obsmnsir3 == 1 | obsmnsil3 == 1 ~ 1,
                                 obsmnsir3 == 0 & obsmnsil3 == 0 ~ 0,
                                 TRUE ~ NA_real_),
           Fissure = case_when(obsmnsir4 == 1 | obsmnsil4 == 1 ~ 1,
                               obsmnsir4 == 0 & obsmnsil4 == 0 ~ 0,
                               TRUE ~ NA_real_),
           Ulceration = case_when(obsmnsir_ulcer == 1 | obsmnsil_ulcer == 1 ~ 1,
                                  obsmnsir_ulcer == 0 & obsmnsil_ulcer == 0 ~ 0,
                                  TRUE ~ NA_real_)
    ) %>% 
    mutate(R1_abnormalormal = case_when(obsmnsir1 == 1 | obsmnsir2 == 1 | obsmnsir3 == 1 | obsmnsir4 == 1 ~ 1,
                                        TRUE ~ 0),
           R2_Ulceration = obsmnsir_ulcer,
           
           R3_Reflex = case_when(obsmnsir_reflex1 == 1 & obsmnsir_reflex2 == 0 ~ 1,
                                 obsmnsir_reflex1 == 0 & obsmnsir_reflex2 == 1 ~ 0.5,
                                 obsmnsir_reflex1 == 0 & obsmnsir_reflex2 == 0 ~ 0,
                                 TRUE ~ NA_real_),
           
           R4_Perception = case_when(obsmnsir_perception1 == 1 & obsmnsir_perception2 == 0 ~ 1,
                                     obsmnsir_perception1 == 0 & obsmnsir_perception2 == 1 ~ 0.5,
                                     obsmnsir_perception1 == 0 & obsmnsir_perception2 == 0 ~ 0,
                                     TRUE ~ NA_real_),
           
           L1_abnormalormal = case_when(obsmnsil1 == 1 | obsmnsil2 == 1 | obsmnsil3 == 1 | obsmnsil4 == 1 ~ 1,
                                        TRUE ~ 0),
           L2_Ulceration = obsmnsil_ulcer,
           
           L3_Reflex = case_when(obsmnsil_reflex1 == 1 & obsmnsil_reflex2 == 0 ~ 1,
                                 obsmnsil_reflex1 == 0 & obsmnsil_reflex2 == 1 ~ 0.5,
                                 obsmnsil_reflex1 == 0 & obsmnsil_reflex2 == 0 ~ 0,
                                 TRUE ~ NA_real_),
           
           L4_Perception = case_when(obsmnsil_perception1 == 1 & obsmnsil_perception2 == 0 ~ 1,
                                     obsmnsil_perception1 == 0 & obsmnsil_perception2 == 1 ~ 0.5,
                                     obsmnsil_perception1 == 0 & obsmnsil_perception2 == 0 ~ 0,
                                     TRUE ~ NA_real_)
           
    )  %>%
    rowwise() %>%
    mutate(survey_score = sum(c_across(selfmnsi1:selfmnsi15), na.rm = TRUE)) %>%
    mutate(survey_abnormal = case_when(survey_score >= 4 ~ 1,
                                       TRUE ~ 0)) %>% 
    mutate(exam_score = sum(c(R1_abnormalormal, R2_Ulceration, R3_Reflex, R4_Perception,
                              L1_abnormalormal, L2_Ulceration, L3_Reflex, L4_Perception), na.rm = TRUE)) %>%
    mutate(exam_abnormal = case_when(exam_score >= 2.5 ~ 1,
                                     TRUE ~ 0)) %>% 
    ungroup() %>% 
    ### Combination
    mutate(combined_abnormal = case_when(survey_abnormal == 1 | exam_abnormal == 1 ~ 1,
                                         TRUE ~ 0)) %>% 
    left_join(crosssec_df %>% 
                dplyr::select(study,study_id,ltfu_prob,ltfu_weight),
              by = c("study","study_id")) %>% 
    # convert to numberic
    mutate(study_id = case_when(study == "TODAY" ~ str_replace_all(study_id, "-", ""),
                                TRUE ~ study_id)) %>% 
    mutate(study_id = as.numeric(study_id))
    
  
  survey_mod <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth, 
                            data = completed_data, 
                            family = poisson(link = "log"), 
                            # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                            id = study_id,
                            weights = ltfu_weight, 
                            corstr = "independence")
  exam_mod <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth, 
                          data = completed_data, 
                          family = poisson(link = "log"), 
                          # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                          id = study_id,
                          weights = ltfu_weight, 
                          corstr = "independence")
  combined_mod <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth, 
                              data = completed_data, 
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
  write_csv(.,"factorial/analysis/dsy04_cross-sectional coefficients.csv")
