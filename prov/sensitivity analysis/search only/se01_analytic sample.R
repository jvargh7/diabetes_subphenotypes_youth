rm(list=ls());gc();source(".Rprofile")

#--------------------------------------------------------------------------------------
# Outcome: 3 scores
# survey score: >= 4 
# exam score: >= 2.5
# combined score: survey >=4 / exam >= 2.5
#--------------------------------------------------------------------------------------

mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_mnsi.RDS")) %>% 
  mutate(study_id = as.character(study_id), study = "SEARCH") %>%
  dplyr::select(-obsmnsir5,-obsmnsil5) %>% 
  ### Questionnaire
  mutate(across(c(selfmnsi1:selfmnsi3, selfmnsi5:selfmnsi6, selfmnsi8:selfmnsi9, selfmnsi11:selfmnsi12, selfmnsi14:selfmnsi15), 
                ~ifelse(. == 1, 1, 0))) %>%
  mutate(across(c(selfmnsi7, selfmnsi13), ~ifelse(. == 0, 1, 0))) %>%
  rowwise() %>%
  mutate(survey_score = sum(c_across(selfmnsi1:selfmnsi15), na.rm = TRUE)) %>%
  mutate(survey_abnormal = case_when(survey_score >= 4 ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() %>%
  ### Examination
  mutate(obsmnsir = case_when(obsmnsir == "Normal" ~ 0,
                              obsmnsir == "Abnormal" ~ 1,
                              TRUE ~ NA_real_),
         obsmnsil = case_when(obsmnsil == "Normal" ~ 0,
                              obsmnsil == "Abnormal" ~ 1,
                              TRUE ~ NA_real_),
         obsmnsir_reflex1 = case_when(obsmnsir_reflex == "Present" ~ 0,
                                      obsmnsir_reflex == "Reduced" ~ 0,
                                      obsmnsir_reflex == "Absent" ~ 1,
                                      TRUE ~ NA_real_),
         obsmnsir_reflex2 = case_when(obsmnsir_reflex == "Present" ~ 0,
                                      obsmnsir_reflex == "Reduced" ~ 1,
                                      obsmnsir_reflex == "Absent" ~ 0,
                                      TRUE ~ NA_real_),
         obsmnsil_reflex1 = case_when(obsmnsil_reflex == "Present" ~ 0,
                                      obsmnsil_reflex == "Reduced" ~ 0,
                                      obsmnsil_reflex == "Absent" ~ 1,
                                      TRUE ~ NA_real_),
         obsmnsil_reflex2 = case_when(obsmnsil_reflex == "Present" ~ 0,
                                      obsmnsil_reflex == "Reduced" ~ 1,
                                      obsmnsil_reflex == "Absent" ~ 0,
                                      TRUE ~ NA_real_),
         obsmnsir_perception1 = case_when(obsmnsir_perception == "Present" ~ 0,
                                          obsmnsir_perception == "Reduced" ~ 0,
                                          obsmnsir_perception == "Absent" ~ 1,
                                          TRUE ~ NA_real_),
         obsmnsir_perception2 = case_when(obsmnsir_perception == "Present" ~ 0,
                                          obsmnsir_perception == "Reduced" ~ 1,
                                          obsmnsir_perception == "Absent" ~ 0,
                                          TRUE ~ NA_real_),
         obsmnsil_perception1 = case_when(obsmnsil_perception == "Present" ~ 0,
                                          obsmnsil_perception == "Reduced" ~ 0,
                                          obsmnsil_perception == "Absent" ~ 1,
                                          TRUE ~ NA_real_),
         obsmnsil_perception2 = case_when(obsmnsil_perception == "Present" ~ 0,
                                          obsmnsil_perception == "Reduced" ~ 1,
                                          obsmnsil_perception == "Absent" ~ 0,
                                          TRUE ~ NA_real_),
         obsmnsir_filament1 = case_when(obsmnsir_filament == "Present" ~ 0,
                                        obsmnsir_filament == "Reduced" ~ 0,
                                        obsmnsir_filament == "Absent" ~ 1,
                                        TRUE ~ NA_real_),
         obsmnsir_filament2 = case_when(obsmnsir_filament == "Present" ~ 0,
                                        obsmnsir_filament == "Reduced" ~ 1,
                                        obsmnsir_filament == "Absent" ~ 0,
                                        TRUE ~ NA_real_),
         obsmnsil_filament1 = case_when(obsmnsil_filament == "Present" ~ 0,
                                        obsmnsil_filament == "Reduced" ~ 0,
                                        obsmnsil_filament == "Absent" ~ 1,
                                        TRUE ~ NA_real_),
         obsmnsil_filament2 = case_when(obsmnsil_filament == "Present" ~ 0,
                                        obsmnsil_filament == "Reduced" ~ 1,
                                        obsmnsil_filament == "Absent" ~ 0,
                                        TRUE ~ NA_real_)
  ) %>% 
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
         
  ) %>% 
  rowwise() %>%
  mutate(exam_score = sum(c(R1_abnormalormal, R2_Ulceration, R3_Reflex, R4_Perception,
                            L1_abnormalormal, L2_Ulceration, L3_Reflex, L4_Perception), na.rm = TRUE)) %>%
  mutate(exam_abnormal = case_when(exam_score >= 2.5 ~ 1,
                                        TRUE ~ 0)) %>% 
  ungroup() %>% 
  ### Combination
  mutate(combined_abnormal = case_when(survey_abnormal == 1 | exam_abnormal == 1 ~ 1,
                                  TRUE ~ 0))



analytic_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/prov/setdy04_kmeans clustering.csv')) %>% 
  dplyr::filter(study == "SEARCH")


crosssec_df <-  analytic_df %>% 
  mutate(study_id = as.character(study_id)) %>% 
  left_join(
    mnsi %>% 
      group_by(study,study_id) %>%
      mutate(include = case_when(study == "SEARCH" & age == min(age) ~ 1),
             age_diff = (age - min(age))*365) %>% 
      ungroup() %>% 
      dplyr::filter(include == 1),
    by = c("study","study_id")
    
  )

saveRDS(crosssec_df,paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/search only/se01_cross sectional df.RDS"))

