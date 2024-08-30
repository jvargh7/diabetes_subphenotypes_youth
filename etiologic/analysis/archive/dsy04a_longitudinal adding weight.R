rm(list=ls());gc();source(".Rprofile")



search_data_wide <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02b_longitudinal df.RDS")) %>%
  dplyr::filter(study == "SEARCH") %>%
  dplyr::select("study_id","wave","study","age") %>% 
  pivot_wider(
    names_from = wave,
    values_from = age,
    names_prefix = "age_"
  ) %>%
  mutate(age_diff = `age_SEARCH 4` - `age_SEARCH 1 TO 3`)


longitudinal_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02b_longitudinal df.RDS")) %>%
  left_join(search_data_wide %>% dplyr::select(study_id, age_diff), by = "study_id") %>% 
  mutate(age_diff = case_when(study == "SEARCH" ~ age_diff,
                              TRUE ~ randdays/365.25))

ggplot(data=longitudinal_df,aes(x=age_diff,fill=study,group=study)) +
  geom_histogram(position =  position_dodge(width=0.9))



















## available data indicator, 1~562, 0~160
long_weight <- longitudinal_df %>%
  # mutate(survey_available = if_else(rowSums(!is.na(select(., contains("selfmnsi")))) > 0, 1, 0)) %>%
  # mutate(exam_available = if_else(rowSums(!is.na(select(., contains("obsmnsi")))) > 0, 1, 0))
  mutate(survey_available = case_when(!is.na(survey_score) ~ 1,
                                      TRUE ~ 0),
         exam_available = case_when(!is.na(exam_score) ~ 1,
                                    TRUE ~ 0),
         combined_available = case_when(survey_available == 1 & exam_available == 1 ~ 1,
                                        TRUE ~ 0)) 
# N = 333
subset_mnsi_na <- longitudinal_df %>%
   dplyr::filter_if(grepl("mnsi", names(.)), any_vars(is.na(.)))



## logistic regression 
glm_mod <- glm(combined_available ~ age_category + female + race_eth + cluster, data = cross_weight)
summary(glm_mod)

## estimate probability
cross_weight$prob <- predict(glm_mod, data = cross_weight, type = "response")

## weight = 1/prob
cross_weight$weight <- 1/(cross_weight$prob)

saveRDS(cross_weight,paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy03a_add weight cross-sectional.RDS"))
