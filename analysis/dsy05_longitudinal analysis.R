
rm(list=ls());gc();source(".Rprofile")

longitudinal_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02b_longitudinal df.RDS"))

mi_dfs <- readRDS("analysis/dsy03_mi_dfs.RDS")

library(geepack)

survey_mod <- exam_mod <- combined_mod <- list()

# Apply the function to each imputed dataset
for (i in 1:mi_dfs$m) {
  
  complete_data <- complete(mi_dfs, action = i) %>%
    mutate(cluster = factor(cluster,levels=c("OB","ID","IR")),
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
           
           R3_Reflex = case_when(obsmnsir_reflex == "Absent" ~ 1,
                                 obsmnsir_reflex == "Reduced" ~ 0.5,
                                 obsmnsir_reflex == "Present" ~ 0,
                                 TRUE ~ NA_real_),
           
           R4_Perception = case_when(obsmnsir_perception == "Absent"  ~ 1,
                                     obsmnsir_perception == "Reduced"  ~ 0.5,
                                     obsmnsir_perception == "Present" ~ 0,
                                     TRUE ~ NA_real_),
           
           L1_abnormalormal = case_when(obsmnsil1 == 1 | obsmnsil2 == 1 | obsmnsil3 == 1 | obsmnsil4 == 1 ~ 1,
                                        TRUE ~ 0),
           L2_Ulceration = obsmnsil_ulcer,
           
           L3_Reflex = case_when(obsmnsil_reflex == "Absent" ~ 1,
                                 obsmnsil_reflex == "Reduced" ~ 0.5,
                                 obsmnsil_reflex == "Present" ~ 0,
                                 TRUE ~ NA_real_),
           
           L4_Perception = case_when(obsmnsil_perception == "Absent"  ~ 1,
                                     obsmnsil_perception == "Reduced"  ~ 0.5,
                                     obsmnsil_perception == "Present" ~ 0,
                                     TRUE ~ NA_real_),
           
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
                                         TRUE ~ 0))

  
     
  
  
      survey_followup_data = complete_data %>% 
        dplyr::filter(earliest == 0) %>% 
        dplyr::select(study,study_id,survey_score,survey_abnormal,randdays) %>% 
        right_join(
          complete_data %>% 
            dplyr::filter(earliest == 1,survey_abnormal == 0) %>%
            dplyr::select(study,study_id,cluster,cluster_age_category,female,race_eth),
          by = c("study","study_id")
          
        ) %>% 
        mutate(followup_available = case_when(is.na(survey_abnormal) ~ 0,
                                              TRUE ~ 1))
      
      exam_followup_data = complete_data %>% 
        dplyr::filter(earliest == 0) %>% 
        dplyr::select(study,study_id,exam_score,exam_abnormal,randdays) %>% 
        right_join(
          complete_data %>% 
            dplyr::filter(earliest == 1,exam_abnormal == 0) %>%
            dplyr::select(study,study_id,cluster,cluster_age_category,female,race_eth),
          by = c("study","study_id")
          
        ) %>% 
        mutate(followup_available = case_when(is.na(exam_abnormal) ~ 0,
                                              TRUE ~ 1))
      
      combined_followup_data = complete_data %>% 
        dplyr::filter(earliest == 0) %>% 
        dplyr::select(study,study_id,combined_abnormal,randdays) %>% 
        right_join(
          complete_data %>% 
            dplyr::filter(earliest == 1,combined_abnormal == 0) %>%
            dplyr::select(study,study_id,cluster,cluster_age_category,female,race_eth),
          by = c("study","study_id")
          
        )
      

      
      
      # mi_dfs$imp[[i]] <- transformed_data
      
      survey_mod[[i]] <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth + randdays, 
                                data = survey_followup_data, 
                                family = poisson(link = "log"), 
                                # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                                id = study_id,
                                weights = ltfu_weight, 
                                corstr = "independence")
      exam_mod[[i]] <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth + randdays, 
                              data = exam_followup_data, 
                              family = poisson(link = "log"), 
                              # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                              id = study_id,
                              weights = ltfu_weight, 
                              corstr = "independence")
      combined_mod[[i]] <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth + randdays, 
                                  data = combined_followup_data, 
                                  family = poisson(link = "log"), 
                                  # This is incorrect - we are not trying to cluster on the cluster! This is the clustering for repeated measures!
                                  id = study_id,
                                  weights = ltfu_weight, 
                                  corstr = "independence")
        

    
  
  
}
  