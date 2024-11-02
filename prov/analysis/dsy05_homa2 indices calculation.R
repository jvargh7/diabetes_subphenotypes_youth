rm(list=ls());gc();source(".Rprofile")

library(ggplot2)

crosssec_df <- readRDS(paste0(path_diabetes_subphenotypes_youth_folder,"/working/cleaned/prov/dsy01_cross sectional df.RDS")) %>% 
  dplyr::filter(study == "TODAY") %>% 
  group_by(study, study_id) %>% 
  mutate(tglhdl_ratio = tgl/hdlc) %>% 
  ungroup()

tglhdl_ratio_mod <-lm(tglhdl_ratio ~ age_category, data = crosssec_df)

fig_df <- crosssec_df %>% 
  mutate(tglhdl_ratio_residual = residuals(tglhdl_ratio_mod))
#-------------------------------------------------------------------------------------------------------------------------------
# Function to calculate HOMA indices
calculate_homa <- function(glucose_mg_dl, insulin_uU_ml) {
  glucose_mmol_l <- glucose_mg_dl / 18  # Convert glucose from mg/dL to mmol/L
  insulin_mU_l <- insulin_uU_ml         # Conversion isn't necessary if in µU/mL or mU/L
  
  homa2_ir <- (insulin_mU_l * glucose_mmol_l) / 22.5
  homa2_b <- (20 * insulin_mU_l) / (glucose_mmol_l - 3.5)
  
  return(data.frame(HOMA2_IR = homa2_ir, HOMA2_B = homa2_b))
}

# Apply the function to the dataset
fig_df1 <- fig_df %>%
  rowwise() %>%
  mutate(homa = list(calculate_homa(glucosef, insulinf)))

# Flatten the list to create columns
fig_df2 <- tidyr::unnest(fig_df1, homa)

#-----------------------------------------------------------------------------------------------------------------------------

ir_ratio_plot <- ggplot(fig_df2, aes(x = tglhdl_ratio, y = HOMA2_IR)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "solid") +
  labs(
    title = "Scatter Plot of HOMA2-IR vs Triglycerides/HDL Ratio",
    x = "Triglycerides/HDL Ratio",
    y = "HOMA2-IR"
  ) +
  theme_minimal()

#-----------------------------------------------------------------------------------------------------------------------------

b_cpep_plot <- ggplot(fig_df2, aes(x = cpeptidef, y = HOMA2_B)) +
  geom_point(alpha = 0.6, color = "blue") +  
  geom_smooth(method = "lm", color = "red", se = TRUE, linetype = "solid") +
  labs(
    title = "Scatter Plot of HOMA2-B vs Fasting c-peptide",
    x = "Fasting C-peptide",
    y = "HOMA2-B"
  ) +
  theme_minimal()

#-------------------------------------------------------------------------------------------------------------------------------
## calculate HOMA2 indices

# convert unit, exclude outliers
# glucose 3.0-25.0 mmol/L (mg/dL --> mmol/L)
# insulin 20-400 pmol/L (µU/mL --> pmol/L)

# unit conversion reference:
# [1] Knopp JL, Holder-Pearson L, Chase JG. Insulin Units and Conversion Factors: A Story of Truth, Boots, and Faster Half-Truths. J Diabetes Sci Technol. 
#     2019 May;13(3):597-600. doi: 10.1177/1932296818805074. Epub 2018 Oct 13. PMID: 30318910; PMCID: PMC6501531.
# [2] Riemsma R, Corro Ramos I, Birnie R, et al. Integrated sensor-augmented pump therapy systems [the MiniMed® Paradigm™ Veo system and the Vibe™ and G4® PLATINUM CGM (continuous glucose monitoring) system] for managing blood glucose levels in type 1 diabetes: 
#     a systematic review and economic evaluation. Southampton (UK): NIHR Journals Library; 2016 Feb. (Health Technology Assessment, No. 20.17.) Appendix 5, Conversion tables for glycated haemoglobin and glucose values. 
#     Available from: https://www.ncbi.nlm.nih.gov/books/NBK348987/

value_select <- fig_df %>% 
  dplyr::select(study_id,glucosef,insulinf) %>% 
  group_by(study_id) %>% 
  mutate(
    # Convert glucose from mg/dL to mmol/L
    glucosef_mmol_l = glucosef / 18,
    # convert µU/mL to pmol/L
    insulinf_µU_ml = insulinf * 6
  ) %>% 
  ungroup() %>% 
  mutate(insulinf_µU_ml = case_when(
    insulinf_µU_ml < 20 ~ 20,
    insulinf_µU_ml > 400 ~ 400,
    TRUE ~ insulinf_µU_ml
  ))


library(openxlsx)

write.xlsx(value_select, file = "prov/analysis/dsy05_homa2 indices calculation.xlsx")
