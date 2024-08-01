rm(list=ls());gc();source(".Rprofile")

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/dsy02a_cross sectional df.RDS"))

### MNSI scores all missing, N = 160
all_na <- crosssec_df %>% 
  dplyr::filter(is.na(combined_abnormal))

### partially missing MNSI scores, N = 562
part_na <- crosssec_df %>% 
  dplyr::filter(!is.na(combined_abnormal)) %>% 
  dplyr::select("study_id","study","cluster","female","age_category","race_eth",contains("mnsi"))


#-----------------------------------------------------------------------------------------------------------------------
## Multiple Imputation for MNSI measurements

