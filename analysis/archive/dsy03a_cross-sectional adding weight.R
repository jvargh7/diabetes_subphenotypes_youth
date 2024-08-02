rm(list=ls());gc();source(".Rprofile")

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02a_cross sectional df.RDS"))

## available data indicator, 1~562, 0~160
cross_weight <- crosssec_df %>%
# mutate(survey_available = if_else(rowSums(!is.na(select(., contains("selfmnsi")))) > 0, 1, 0)) %>%
# mutate(exam_available = if_else(rowSums(!is.na(select(., contains("obsmnsi")))) > 0, 1, 0))
mutate(survey_available = case_when(!is.na(survey_score) ~ 1,
                                      TRUE ~ 0),
         exam_available = case_when(!is.na(exam_score) ~ 1,
                                      TRUE ~ 0),
         combined_available = case_when(survey_available == 1 & exam_available == 1 ~ 1,
                                      TRUE ~ 0))

## logistic regression 
glm_mod <- glm(combined_available ~ age_category + female + race_eth + cluster, data = cross_weight)
summary(glm_mod)

## estimate probability
cross_weight$prob <- predict(glm_mod, data = cross_weight, type = "response")

## weight = 1/prob
cross_weight$weight <- 1/(cross_weight$prob)



saveRDS(cross_weight,paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS"))
