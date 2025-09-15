rm(list=ls());gc();source(".Rprofile")

mi_dfs <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/dsy03_neuropathy mi_dfs.RDS")) 

# Loss to follow-up weights
crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/etiologic/dsy01_cross sectional df.RDS")) %>% 
  dplyr::select(-"nonna_selfmnsi") %>% 
  mutate(cs_available = case_when(is.na(combined_abnormal) ~ 0,
                                  TRUE ~ 1))

# ---- 1) Sanity checks on variables used in the model ----
vars <- c("cs_available","cluster","age_category","female","race_eth","study")
stopifnot(all(vars %in% names(crosssec_df)))

# Look at levels / missingness quickly
str(crosssec_df[vars])
sapply(crosssec_df[vars], \(x) sum(is.na(x)))

# ---- 2) Clean types, drop NAs used by the model, drop empty levels ----
df_mod <- crosssec_df %>% 
  dplyr::mutate(
    cs_available = as.integer(cs_available),   # 0/1
    cluster      = as.factor(cluster),
    age_category = as.factor(age_category),
    female       = as.integer(female),         # 0/1
    race_eth     = as.factor(race_eth),
    study        = as.factor(study)
  ) %>% 
  dplyr::filter(if_all(all_of(vars), ~ !is.na(.))) %>% 
  droplevels()

# Outcome must vary
table(df_mod$cs_available)
stopifnot(dplyr::n_distinct(df_mod$cs_available) > 1)

# ---- 3) Fit (binary outcome with log link via Poisson) ----
cs_available_mod <- glm(
  cs_available ~ cluster + age_category + female + race_eth + study,
  data   = df_mod,
  family = poisson(link = "log")
)
summary(cs_available_mod)


df_mod$ltfu_prob = predict(cs_available_mod,newdata=df_mod,type="response")
df_mod$ltfu_weight = 1/df_mod$ltfu_prob

library(mice)
library(geepack)
library(stringr)
  
  completed_data <- complete(mi_dfs, action = 1) %>%
    # dplyr::filter(earliest == 1) %>% 
    mutate(cluster = factor(cluster,levels=c("yOD","yIDD","yIRD")),
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
    left_join(df_mod %>% 
                dplyr::select(study,study_id,ltfu_prob,ltfu_weight,bmi),
              by = c("study","study_id")) %>% 
    # convert to numberic
    mutate(study_id = case_when(study == "TODAY" ~ str_replace_all(study_id, "-", ""),
                                TRUE ~ study_id)) %>% 
    mutate(study_id = as.numeric(study_id))
    

# --- Clean & validate ---
vars <- c("survey_abnormal","cluster","age_category","female","race_eth","study_id","ltfu_weight")
  
df <- completed_data %>%
  mutate(
    survey_abnormal = as.integer(survey_abnormal),      # 0/1
    cluster         = factor(cluster),
    age_category    = factor(age_category),
    female          = as.integer(female),               # 0/1
    race_eth        = factor(race_eth),
    study_id        = factor(study_id),
    ltfu_weight     = as.numeric(ltfu_weight)
  ) %>%
  dplyr::filter(if_all(all_of(vars), ~ !is.na(.))) %>%
  dplyr::filter(is.finite(ltfu_weight), ltfu_weight > 0) %>%
  droplevels()
  
# Adjusted -----------------------------------------------------------------------------------------------------
  
  survey_mod <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth, 
                            data = df, 
                            family = poisson(link = "log"), 
                            id = study_id,
                            weights = ltfu_weight, 
                            corstr = "independence")
  exam_mod <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth, 
                          data = df, 
                          family = poisson(link = "log"), 
                          id = study_id,
                          weights = ltfu_weight, 
                          corstr = "independence")
  combined_mod <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth, 
                              data = df, 
                              family = poisson(link = "log"), 
                              id = study_id,
                              weights = ltfu_weight, 
                              corstr = "independence")



survey_mod_out = broom::tidy(survey_mod)  
exam_mod_out = broom::tidy(exam_mod)  
combined_mod_out = broom::tidy(combined_mod)  

# Adjusted for BMI -----------------------------------------------------------------------------------------------------

survey_bmod <- geeglm(survey_abnormal ~ cluster + age_category + female + race_eth + bmi, 
                     data = df, 
                     family = poisson(link = "log"), 
                     id = study_id,
                     weights = ltfu_weight, 
                     corstr = "independence")
exam_bmod <- geeglm(exam_abnormal ~ cluster + age_category + female + race_eth + bmi, 
                   data = df, 
                   family = poisson(link = "log"), 
                   id = study_id,
                   weights = ltfu_weight, 
                   corstr = "independence")
combined_bmod <- geeglm(combined_abnormal ~ cluster + age_category + female + race_eth + bmi, 
                       data = df, 
                       family = poisson(link = "log"), 
                       id = study_id,
                       weights = ltfu_weight, 
                       corstr = "independence")



survey_bmod_out = broom::tidy(survey_bmod)  
exam_bmod_out = broom::tidy(exam_bmod)  
combined_bmod_out = broom::tidy(combined_bmod)  

# Unadjusted -----------------------------------------------------------------------------------------------------

survey_unmod <- geeglm(survey_abnormal ~ cluster, 
                     data = completed_data, 
                     family = poisson(link = "log"), 
                     id = study_id,
                     weights = ltfu_weight, 
                     corstr = "independence")
exam_unmod <- geeglm(exam_abnormal ~ cluster, 
                   data = completed_data, 
                   family = poisson(link = "log"), 
                   id = study_id,
                   weights = ltfu_weight, 
                   corstr = "independence")
combined_unmod <- geeglm(combined_abnormal ~ cluster, 
                       data = completed_data, 
                       family = poisson(link = "log"), 
                       id = study_id,
                       weights = ltfu_weight, 
                       corstr = "independence")



survey_unmod_out = broom::tidy(survey_unmod)  
exam_unmod_out = broom::tidy(exam_unmod)  
combined_unmod_out = broom::tidy(combined_unmod)  




bind_rows(
  survey_mod_out %>% mutate(outcome = "survey_abnormal"),
  exam_mod_out %>% mutate(outcome = "exam_abnormal"),
  combined_mod_out %>% mutate(outcome = "combined_abnormal"),
  
  survey_bmod_out %>% mutate(outcome = "survey_abnormal_bmi"),
  exam_bmod_out %>% mutate(outcome = "exam_abnormal_bmi"),
  combined_bmod_out %>% mutate(outcome = "combined_abnormal_bmi"),
  
  survey_unmod_out %>% mutate(outcome = "unadjusted survey_abnormal"),
  exam_unmod_out %>% mutate(outcome = "unadjusted exam_abnormal"),
  combined_unmod_out %>% mutate(outcome = "unadjusted combined_abnormal")
  
) %>% 
  write_csv(.,"etiologic/analysis/dsy04_neuropathy cross-sectional coefficients.csv")
