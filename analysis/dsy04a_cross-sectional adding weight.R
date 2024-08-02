rm(list=ls());gc();source(".Rprofile")

# Define the function to fit GLM and calculate weights
calculate_weights <- function(data) {
  glm_mod <- glm(combined_abnormal ~ age_category + female + race_eth + cluster, data = data, family = binomial)
  data$prob <- predict(glm_mod, newdata = data, type = "response")
  data$weight <- 1 / data$prob
  return(data)
}

# Apply the function to each imputed dataset
for (i in 1:mi_dfs$m) {
  completed_data <- complete(mi_dfs, action = i)
  transformed_data <- calculate_weights(completed_data)
  mi_dfs$imp[[i]] <- transformed_data
}
