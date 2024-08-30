rm(list=ls());gc();source(".Rprofile")


### Elbow Plot
library(ggplot2)

imputed_data <- read.csv(paste0(path_diabetes_subphenotypes_youth_folder, '/working/cleaned/etiologic/setdy02_knn imputation.csv')) %>% 
  dplyr::select(bmi, hba1c, cpeptidef, sbp, dbp, ldlc, hdlc)

# Scaling the data can often yield better clustering results
data_scaled <- scale(imputed_data)

# Function to compute total within-cluster sum of square
wss <- function(k) {
  kmeans(data_scaled, centers=k, nstart=10)$tot.withinss
}

# Compute and plot WSS for different numbers of clusters
k_values <- 1:11  
wss_values <- sapply(k_values, wss)

elbow_plot <- ggplot(data.frame(k = k_values, wss = wss_values), aes(x = k, y = wss)) +
  geom_point(size = 3, color = "blue") +  # Increase point size and change color
  geom_line(color = "blue") +  # Change line color to match points
  scale_x_continuous(breaks = 1:11) +  # Explicit x-axis breaks for each integer from 1 to 11
  labs(title = "Elbow Plot for Optimal Number of Clusters",
       x = "Number of clusters (k)",
       y = "Total within-cluster sum of squares")
theme_minimal() +  # Use a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # Center and bold the title
    axis.title = element_text(face = "bold"),  # Bold axis titles
    axis.text = element_text(color = "black"),  # Black color for axis texts
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.background = element_rect(fill = "white", colour = "white")
  )

ggsave(elbow_plot,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/etiologic/elbow plot for optimal number of clusters.png"),width=6, height = 4)

