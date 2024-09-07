rm(list=ls());gc();source(".Rprofile")

analytic_df <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/factorial/setdy04_kmeans clustering.csv')) %>% 
  dplyr::filter(study == "SEARCH") %>% 
  dplyr::select(cluster,study,study_id,age_category,female,race_eth,dkd,nephropathy_prescription,nephropathy_tx,nephropathy_diag)

#-----------------------------------------------------------------------------------------------------------------------
## Multiple Imputation for nephropathy indicators
colnames(analytic_df)

proportion_vars <- c("female","nephropathy_diag")

grouped_vars <- c("age_category","race_eth")

id_vars = c("study_id","study","cluster")

#------------------------------------------------------------------------------------------------------------------
require(survey)
require(mice)

impute_df <- analytic_df %>% 
  dplyr::select(one_of(id_vars),one_of(proportion_vars), one_of(grouped_vars))

imputation_method <- rep("rf", length(impute_df))
imputation_method[names(impute_df) %in% proportion_vars] <- "logreg"
imputation_method[names(impute_df) %in% grouped_vars] <- ""
imputation_method[names(impute_df) %in% id_vars] <- ""

mi_dfs <- mice(impute_df, method = imputation_method, m = 1, maxit = 50, seed = 500)

mi_analytic_df <- complete(mi_dfs, 1)

saveRDS(mi_analytic_df, "factorial/analysis/dsy06_nephropathy mi_dfs.RDS")
