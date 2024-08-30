rm(list=ls());gc();source(".Rprofile")

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/factorial/dsy01_cross sectional df.RDS")) %>% 
  dplyr::select(-"nonna_selfmnsi")


# MNSI scores all missing, N = 140
all_na <- crosssec_df %>% 
  dplyr::filter(is.na(combined_abnormal))

### partially missing MNSI scores, N = 501
part_na <- crosssec_df %>% 
  dplyr::filter(!is.na(combined_abnormal)) 

#-----------------------------------------------------------------------------------------------------------------------
## Multiple Imputation for MNSI measurements
colnames(part_na)

selfmnsi_columns <- names(part_na)[grep("selfmnsi", names(part_na))]
proportion_vars <- c("female",selfmnsi_columns, "obsmnsir","obsmnsil",
                     "obsmnsir1","obsmnsir2","obsmnsir3","obsmnsir4", 
                     "obsmnsil1","obsmnsil2","obsmnsil3","obsmnsil4",       
                     "obsmnsir_ulcer","obsmnsil_ulcer",
                     "obsmnsir_reflex1", "obsmnsir_reflex2","obsmnsil_reflex1", "obsmnsil_reflex2",
                     "obsmnsir_perception1","obsmnsir_perception2","obsmnsil_perception1","obsmnsil_perception2",
                     "obsmnsir_filament1","obsmnsir_filament2","obsmnsil_filament1","obsmnsil_filament2")

grouped_vars <- c("age_category","race_eth")

id_vars = c("study_id","study","cluster","wave")
#------------------------------------------------------------------------------------------------------------------
require(survey)
require(mice)

impute_df <- part_na %>% 
  dplyr::select(one_of(id_vars),one_of(proportion_vars), one_of(grouped_vars))

imputation_method <- rep("rf", length(impute_df))
imputation_method[names(impute_df) %in% proportion_vars] <- "logreg"
imputation_method[names(impute_df) %in% grouped_vars] <- ""
imputation_method[names(impute_df) %in% id_vars] <- ""

mi_dfs <- mice(impute_df, method = imputation_method, m = 1, maxit = 50, seed = 500)

#mi_analytic_df <- complete(mi_dfs, 1)

saveRDS(mi_dfs, "factorial/analysis/dsy03_mi_dfs.RDS")

