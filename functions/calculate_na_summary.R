calculate_na_summary <- function(data, study_name = NULL) {
  if (!is.null(study_name)) {
    data <- data %>% dplyr::filter(study == study_name)
  }
  
  na_counts <- sapply(data[vars_of_interest], function(x) sum(is.na(x)))
  na_percentages <- sapply(data[vars_of_interest], function(x) mean(is.na(x)) * 100)
  
  # Create summary data frame
  na_summary <- data.frame(
    Variable = vars_of_interest,
    NA_Count = na_counts,
    NA_Percentage = na_percentages
  )
  
  return(na_summary)
}