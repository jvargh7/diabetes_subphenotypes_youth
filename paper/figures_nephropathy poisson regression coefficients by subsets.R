rm(list=ls());gc();source(".Rprofile")

library(stringr)


combined_tab <- read_csv("paper/table_nephropathy cross-sectional poisson regression coefficients by subsets.csv") %>% 
  mutate(term = str_replace(term, "cluster", ""))

combined_long <- combined_tab %>%
  pivot_longer(cols = c(nephropathy_abnormal),
               names_to = "abnormal_type",
               values_to = "estimate_ci") %>%
  separate(estimate_ci, into = c("estimate", "ci_low", "ci_high"), sep = "\\(|, |\\)") %>%
  mutate(across(c(estimate, ci_low, ci_high), as.numeric)) %>% 
  mutate(subset = factor(subset, levels = c("etiologic search only", "factorial search only", "prov search only")))



forest_plot <- ggplot(combined_long, aes(x = estimate, y = subset, color = term, shape = term)) +
  geom_point(position = position_dodge(width = 0.5), size = 2) +
  geom_errorbarh(aes(xmin = ci_low, xmax = ci_high), 
                 position = position_dodge(width = 0.5), height = 0.2) +
  facet_grid(. ~ abnormal_type, scales = "free_x", switch = "x") +
  theme_bw() +
  labs(x = "Prevalence Ratio (95% CI)", y = "Subset", color = "Subphenotype", shape = "Subphenotype") +
  scale_x_continuous(limits = c(0, 14), breaks = 0:14) +  # Set x-axis limits and breaks
  theme(
    strip.text.x = element_text(angle = 0),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) 

ggsave(forest_plot,filename=paste0(path_diabetes_subphenotypes_youth_folder,"/figures/forest plot of nephropathy prevalence ratio by subset.jpg"),width=12,height =5.5)

