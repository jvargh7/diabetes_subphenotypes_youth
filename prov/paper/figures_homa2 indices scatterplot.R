rm(list=ls());gc();source(".Rprofile")

library(ggplot2)
library(readxl)
library(ggpubr)

homa2_df <- read_excel("prov/analysis/HOMA2Calculator.xlsm", sheet = "Insulin") %>% 
  slice(1:337) %>% 
  rename(homa2b = `HOMA2 %B`, homa2ir = `HOMA2 IR`) %>%  
  dplyr::select(-`HOMA2 %S`) %>% 
  mutate(obs = row_number()) 

prep_df <- read_excel("prov/analysis/dsy05_homa2 indices calculation.xlsx") %>% 
  mutate(obs = row_number()) %>% 
  left_join(homa2_df, by = c("obs","glucosef_mmol_l","insulinf_µU_ml")) %>% 
  dplyr::select(-obs)


crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/dsy01_cross sectional df.RDS")) %>% 
  dplyr::filter(study == "TODAY") %>% 
  group_by(study, study_id) %>% 
  mutate(tglhdl_ratio = tgl/hdlc) %>% 
  ungroup()

tglhdl_ratio_mod <-lm(tglhdl_ratio ~ age_category, data = crosssec_df)

fig_df <- crosssec_df %>% 
  mutate(tglhdl_ratio_residual = residuals(tglhdl_ratio_mod)) %>% 
  dplyr::select(study_id,tglhdl_ratio,cpeptidef) %>% 
  left_join(prep_df %>% 
              dplyr::select(study_id, homa2b, homa2ir), 
            by = "study_id")

#-----------------------------------------------------------------------------------------------------------------------------

ir_ratio_plot <- ggplot(fig_df, aes(x = tglhdl_ratio, y = homa2ir)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "solid") +
  labs(
    x = "Triglycerides/HDL Cholestrol Ratio",
    y = "HOMA2-IR"
  ) +
  theme_minimal() +
  stat_cor()


b_cpep_plot <- ggplot(fig_df, aes(x = cpeptidef, y = homa2b)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "solid") +
  labs(
    x = "Fasting C-peptide (ng/mL)",
    y = "HOMA2-B"
  ) +
  theme_minimal() +
  stat_cor()

library(patchwork)

combined_plot <- ir_ratio_plot + b_cpep_plot + 
  plot_layout(guides = "collect") & 
  plot_annotation(tag_levels = 'A')  


combined_plot %>% 
  ggsave(plot = ., filename = paste0(path_diabetes_subphenotypes_youth_folder,"/figures/prov/homa2 indices by ratio and cpeptide.jpg"),width = 14, height = 5.5)


# Fit linear models for each plot
model_ir <- lm(homa2ir ~ tglhdl_ratio, data = fig_df)
model_b <- lm(homa2b ~ cpeptidef, data = fig_df)

# Summarize the models to get coefficients (intercept and slope)
summary(model_ir)
summary(model_b)
coef(model_ir)
coef(model_b)
