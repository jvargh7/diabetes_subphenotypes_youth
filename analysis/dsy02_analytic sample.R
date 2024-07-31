rm(list=ls());gc();source(".Rprofile")

#--------------------------------------------------------------------------------------
# Outcome: 3 scores
# survey score: >= 4 
# exam score: >= 2.5
# combined score: survey >=4 / exam >= 2.5
#--------------------------------------------------------------------------------------

se_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/search/search_mnsi.RDS")) %>% 
  mutate(study_id = as.character(study_id), study = "SEARCH") %>%
  ### Questionnaire
  mutate(across(c(selfmnsi1:selfmnsi3, selfmnsi5:selfmnsi6, selfmnsi8:selfmnsi9, selfmnsi11:selfmnsi12, selfmnsi14:selfmnsi15), 
                ~ifelse(. == 1, 1, 0))) %>%
  mutate(across(c(selfmnsi7, selfmnsi13), ~ifelse(. == 0, 1, 0))) %>%
  rowwise() %>%
  mutate(survey_score = sum(c_across(selfmnsi1:selfmnsi15), na.rm = TRUE)) %>%
  mutate(survey_abn = case_when(survey_score >= 4 ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() %>%
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
  mutate(E1_Abnormal = case_when(Deformities == 1 | Dryskin == 1 | Infection == 1 | Fissure == 1 ~ 1,
                                 TRUE ~ 0),
         E2_Ulceration = Ulceration,
         
         E3_Reflex = case_when(obsmnsir_reflex == "Absent" | obsmnsil_reflex == "Absent" ~ 1,
                               obsmnsir_reflex == "Reduced" | obsmnsil_reflex == "Reduced" ~ 0.5,
                               obsmnsir_reflex == "Present" & obsmnsil_reflex == "Present" ~ 0,
                               TRUE ~ NA_real_),
         
         E4_Perception = case_when(obsmnsir_perception == "Absent" | obsmnsir_perception == "Absent" ~ 1,
                                   obsmnsir_perception == "Reduced" | obsmnsir_perception == "Reduced" ~ 0.5,
                                   obsmnsir_perception == "Present" & obsmnsir_perception == "Present" ~ 0,
                                   TRUE ~ NA_real_)
  ) %>% 
  rowwise() %>%
  mutate(exam_score = sum(c(E1_Abnormal, E2_Ulceration, E3_Reflex, E4_Perception), na.rm = TRUE)) %>%
  mutate(exam_abn = case_when(exam_score >= 2.5 ~ 1,
                              TRUE ~ 0)) %>% 
  ungroup() %>% 
  ### Combination
  mutate(combined_abn = case_when(survey_abn == 1 | exam_abn == 1 ~ 1,
                                  TRUE ~ 0))


tdy_mnsi <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/today/today_mnsi.RDS")) %>% 
  mutate(study = "TODAY") %>% 
  rename(wave = dataset) %>%
  ### Questionnaire
  mutate(across(c(selfmnsi1:selfmnsi3, selfmnsi5:selfmnsi6, selfmnsi8:selfmnsi9, selfmnsi11:selfmnsi12, selfmnsi14), 
                ~ifelse(. == 1, 1, 0))) %>%
  mutate(across(c(selfmnsi7, selfmnsi13), ~ifelse(. == 0, 1, 0))) %>%
  rowwise() %>%
  mutate(survey_score = sum(c_across(selfmnsi1:selfmnsi14), na.rm = TRUE)) %>%
  mutate(survey_abn = case_when(survey_score >= 4 ~ 1,
                                TRUE ~ 0)) %>% 
  ungroup() %>%
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
  mutate(E1_Abnormal = case_when(Deformities == 1 | Dryskin == 1 | Infection == 1 | Fissure == 1 ~ 1,
                                 TRUE ~ 0),
         E2_Ulceration = Ulceration,
         
         E3_Reflex = case_when(obsmnsir_reflex == "Absent" | obsmnsil_reflex == "Absent" ~ 1,
                               obsmnsir_reflex == "Reduced" | obsmnsil_reflex == "Reduced" ~ 0.5,
                               obsmnsir_reflex == "Present" & obsmnsil_reflex == "Present" ~ 0,
                               TRUE ~ NA_real_),
         
         E4_Perception = case_when(obsmnsir_perception == "Absent" | obsmnsir_perception == "Absent" ~ 1,
                                   obsmnsir_perception == "Reduced" | obsmnsir_perception == "Reduced" ~ 0.5,
                                   obsmnsir_perception == "Present" & obsmnsir_perception == "Present" ~ 0,
                                   TRUE ~ NA_real_)
  ) %>% 
  rowwise() %>%
  mutate(exam_score = sum(c(E1_Abnormal, E2_Ulceration, E3_Reflex, E4_Perception), na.rm = TRUE)) %>%
  mutate(exam_abn = case_when(exam_score >= 2.5 ~ 1,
                              TRUE ~ 0)) %>% 
  ungroup() %>% 
  ### Combination
  mutate(combined_abn = case_when(survey_abn == 1 | exam_abn == 1 ~ 1,
                                  TRUE ~ 0))

# -------------------------------------------------------------------------------------
#          | repeated, early wave | non-repeated | Total |
# -------------------------------------------------------------------------------------
# SEARCH   |           150        |     235      |  385  |
# TODAY    |           324        |      13      |  337  |
# -------------------------------------------------------------------------------------
# cross-sectional      722
# longitudinal         474
#--------------------------------------------------------------------------------------
analytic_sedf <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv')) %>% 
  dplyr::filter(study == "SEARCH") %>% 
  left_join(se_mnsi)

longitudinal_sedf <- analytic_sedf %>%
  group_by(study_id,study) %>%
  dplyr::filter(n() > 1) %>%
  ungroup() # N=150

unique_sedf <- analytic_sedf %>% 
  dplyr::filter(!study_id %in% longitudinal_sedf$study_id) # N=235

crosssec_se <- bind_rows(longitudinal_sedf %>% 
                           dplyr::filter(wave == "SEARCH 1 TO 3"),
                         unique_sedf) # 385

#--------------------------------------------------------
analytic_tdydf <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/setdy03_kmeans clustering.csv')) %>% 
  dplyr::filter(study == "TODAY") %>% 
  left_join(tdy_mnsi)

longitudinal_tdydf <- analytic_tdydf %>%
  group_by(study_id,study) %>%
  dplyr::filter(n() > 1) %>%
  ungroup() # N=324
tdy_mnsi
unique_tdydf <- analytic_tdydf %>% 
  dplyr::filter(!study_id %in% longitudinal_tdydf$study_id) # N=13

crosssec_tdy <- bind_rows(longitudinal_tdydf %>% 
                            group_by(study_id,study) %>%
                            dplyr::filter(randdays == min(randdays)) %>% # Days since randomization
                            ungroup(),
                          unique_tdydf) # 337

#--------------------------------------------------------
crosssec_df <- full_join(crosssec_se, crosssec_tdy) 

longitudinal_df <- full_join(longitudinal_sedf, longitudinal_tdydf) # N = 474

saveRDS(crosssec_df,paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02a_cross sectional df.RDS"))
saveRDS(longitudinal_df,paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02b_longitudinal df.RDS"))















