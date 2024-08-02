rm(list=ls());gc();source(".Rprofile")

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02a_cross sectional df.RDS")) %>% 
  dplyr::select(-"nonna_selfmnsi")

### MNSI scores all missing, N = 160
all_na <- crosssec_df %>% 
  dplyr::filter(is.na(combined_abnormal))

### partially missing MNSI scores, N = 562
part_na <- crosssec_df %>% 
  dplyr::filter(!is.na(combined_abnormal)) %>% 
  dplyr::select("study_id","study","cluster","female","age_category","race_eth",contains("mnsi"))

#-----------------------------------------------------------------------------------------------------------------------
## Multiple Imputation for MNSI measurements
colnames(part_na)

selfmnsi_columns <- names(part_na)[grep("selfmnsi", names(part_na))]
proportion_vars <- c(selfmnsi_columns, "obsmnsir1","obsmnsir2","obsmnsir3","obsmnsir4", 
                     "obsmnsir5","obsmnsil","obsmnsil1","obsmnsil2","obsmnsil3","obsmnsil4",       
                     "obsmnsil5","obsmnsir_ulcer","obsmnsil_ulcer","female")

grouped_vars <- c("age_category","race_eth","obsmnsir","obsmnsil", "obsmnsir_reflex",    
                  "obsmnsil_reflex","obsmnsir_perception","obsmnsil_perception", 
                  "obsmnsir_filament","obsmnsil_filament")

id_vars = c("study_id","study","cluster")
#--------------------------------------------------------------------------------
require(survey)
require(mice)

impute_df <- part_na %>% 
  dplyr::select(one_of(id_vars),one_of(proportion_vars), one_of(grouped_vars))

mi_null <- mice(impute_df, maxit = 0)

method = mi_null$method
pred = mi_null$predictorMatrix

pred[c("study_id","study","cluster"),] <- 0
pred[,c("study_id","study","cluster")] <- 0


mi_dfs <- mice(impute_df,
               method = method,
               pred = pred,
               m=10,maxit=50,seed=500)
#--------------------------------------------------------------------------------

source("functions/add_variable_mi.R")

completed_dfs <- mice::complete(mi_dfs, "long") %>%
  group_by(.imp) %>%
  nest() %>%
  mutate(data = map(data, add_variable_mi)) %>%
  unnest(data)

saveRDS(mi_dfs, "analysis/dsy02b_mi_dfs.RDS")
