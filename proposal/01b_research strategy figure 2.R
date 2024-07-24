
set.seed(2022)

today_mnsi <- readRDS(paste0(path_youth_folder,"/working/today/today_mnsi.RDS")) %>% 
  left_join(readRDS(paste0(path_youth_folder,"/working/today/today_baseline.RDS")) %>% 
              mutate(age_imp = case_when(age == "<=13" ~ runif(nrow(.),min=10,max=13),
                                         age == "14-15" ~ rbinom(nrow(.),1,p=0.5) + 14,
                                         age == ">15" ~ rbinom(nrow(.),1,p=0.5) + 16),
                     treatment = factor(treatment,levels=c(1:3),labels=c("Metformin only","Metformin + Rosiglitazone","Metformin + Lifestyle"))
                     
              ) %>% 
              dplyr::select(study_id,age_imp,treatment,dmduration)%>% 
              rename(dmduration_baseline = dmduration) ,
            by = c("study_id"))    %>% 
  mutate(age = age_imp + randdays/365)  %>% 
  dplyr::filter(dmduration_baseline == "<=5 months")

search_mnsi <- readRDS(paste0(path_youth_folder,"/working/search/search_mnsi.RDS"))  %>% 
  left_join(readRDS(paste0(path_youth_folder,"/working/search/search_baseline.RDS")) %>% 
              dplyr::select(study_id,dmduration) %>% 
              rename(dmduration_baseline = dmduration),
            by = "study_id") %>% 
  mutate(study_id = as.character(study_id)) %>% 
  dplyr::filter(dmduration_baseline >= 0 & dmduration_baseline <= duration_cutoff)


fig2_df <- bind_rows(today_mnsi %>% mutate(study = "TODAY") %>% 
                       dplyr::select(study_id,study,age,treatment,matches("obsmnsi"),matches("selfmnsi")),
                        search_mnsi %>% mutate(study = "SEARCH")  %>% 
                       dplyr::select(study_id,study,age,matches("obsmnsi"),matches("selfmnsi")) %>% 
                          mutate(treatment = "SEARCH")
) %>% 
  mutate(Deformities = case_when(obsmnsir1 == 1 | obsmnsil1 == 1 ~ 1,
                                 obsmnsir1 == 0 & obsmnsil1 == 0 ~ 0,
                                 TRUE ~ NA_real_),
         Dryskin = case_when(obsmnsir2 == 1 | obsmnsil2 == 1 ~ 1,
                                 obsmnsir2 == 0 & obsmnsil2 == 0 ~ 0,
                                 TRUE ~ NA_real_),
         Infection = case_when(obsmnsir3 == 1 | obsmnsil3 == 1 ~ 1,
                                 obsmnsir3 == 0 & obsmnsil3 == 0 ~ 0,
                                 TRUE ~ NA_real_),
         Fissure = case_when(obsmnsir4 == 1 | obsmnsil4 == 1 ~ 1,
                                 obsmnsir4 == 0 & obsmnsil4 == 0 ~ 0,
                                 TRUE ~ NA_real_),
         Ulceration = case_when(obsmnsir_ulcer == 1 | obsmnsil_ulcer == 1 ~ 1,
                                obsmnsir_ulcer == 0 & obsmnsil_ulcer == 0 ~ 0,
                                TRUE ~ NA_real_),
         Reflex = case_when(obsmnsir_reflex %in% c("Absent","Reduced") | obsmnsil_reflex %in% c("Absent","Reduced") ~ 1,
                            obsmnsir_reflex == "Present" & obsmnsil_reflex == "Present" ~ 0,
                            TRUE ~ NA_real_),
         Perception = case_when(obsmnsir_perception %in% c("Absent","Reduced") | obsmnsil_perception %in% c("Absent","Reduced") ~ 1,
                            obsmnsir_perception == "Present" & obsmnsil_perception == "Present" ~ 0,
                            TRUE ~ NA_real_),
         Filament = case_when(obsmnsir_filament %in% c("Absent","Reduced") | obsmnsil_filament %in% c("Absent","Reduced") ~ 1,
                            obsmnsir_filament == "Present" & obsmnsil_filament == "Present" ~ 0,
                            TRUE ~ NA_real_)
         
         ) %>% 
  mutate(E1_Abnormal = case_when(Deformities == 1 | Dryskin == 1 | Infection == 1 | Fissure == 1 ~ 1,
                                 TRUE ~ 0),
         E2_Ulceration = Ulceration,
         
         E3_Reflex = case_when(obsmnsir_reflex == "Absent" | obsmnsil_reflex == "Absent" ~ 1,
                               obsmnsir_reflex == "Reduced" | obsmnsil_reflex == "Reduced" ~ 0.5,
                               obsmnsir_reflex == "Present" & obsmnsil_reflex == "Present" ~ 0,
                               TRUE ~ NA_real_),
         
         E4_Perception = case_when(obsmnsir_perception == "Absent" | obsmnsir_perception == "Absent" ~ 1,
                                   obsmnsir_perception == "Reduced" | obsmnsir_perception == "Reduced" ~ 0.5,
                                   obsmnsir_perception == "Present" & obsmnsir_perception == "Present" ~ 0,
                               TRUE ~ NA_real_)
         ) %>% 
  
  mutate(age = round(age),
         mnsi_survey = apply(.[,regexpr("selfmnsi[0-9]+",colnames(.))>0],1,function(x) sum(x,na.rm=TRUE)),
         nonna_survey = apply(.[,regexpr("selfmnsi[0-9]+",colnames(.))>0],1,function(x) sum(!is.na(x),na.rm=TRUE)),
         
         mnsi_exam = apply(.[,regexpr("E[1-4]+_",colnames(.))>0],1,function(x) sum(x,na.rm=TRUE)),
         
         
         bpe = apply(.[,c("Deformities","Dryskin","Infection","Fissure",
                               "Ulceration","Reflex","Perception","Filament")],1,function(x)sum(x,na.rm=TRUE)),
         nonna_bpe = apply(.[,c("Deformities","Dryskin","Infection","Fissure",
                                     "Ulceration","Reflex","Perception","Filament")],1,function(x)sum(!is.na(x),na.rm=TRUE)))

# MEAN NUMBER OF SYMPTOMS -------

fig1 <- fig2_df %>% 
  dplyr::filter(study == "TODAY") %>% 
  dplyr::select(study,study_id,age,mnsi_survey, mnsi_exam) %>% 
  rename(Survey = mnsi_survey,
         Examination = mnsi_exam) %>% 
  pivot_longer(cols=one_of("Survey","Examination"),names_to = "var",
               values_to="value") %>% 
  ggplot(data=.,aes(x=age,y=value,group=var,col=var,fill=var)) +
  geom_smooth() +
  theme_bw() +
  xlab("Age (years)") +
  ylab("Mean count") +
  scale_y_continuous(limits=c(0,6),breaks=seq(0,6,2)) +
  theme(legend.position = "bottom") +
  scale_color_discrete("") +
  scale_fill_discrete("")

fig1 %>% 
  ggsave(.,filename = paste0(path_youth_folder,"/figures/research strategy figure_average of symptoms.png"),width=8,height=4)


# HEATMAP -------------
fig2 <- fig2_df %>%
  dplyr::filter(study == "TODAY") %>% 
  dplyr::select(study,study_id,age,Deformities,Dryskin,Infection,Fissure,
                Ulceration,Reflex, Perception, Filament) %>% 
  pivot_longer(cols=-one_of(c("study","study_id","age")),
               names_to="examination",values_to = "value") %>% 
  group_by(age,examination) %>% 
  summarize(value = mean(value,na.rm=TRUE),
            n = sum(!is.na(value))) %>% 
  ungroup() %>% 
  mutate(examination = factor(examination,
                              levels=c("Deformities","Dryskin","Infection","Fissure",
                                       "Ulceration","Reflex", "Perception", "Filament"),
                              labels=c("Deformities","Dry skin","Infection","Fissure",
                                       "Ulceration","Poor Reflex", "Poor Perception", "Abnormal Filament")
                              )) %>% 
  ggplot(data=.,aes(x=age,y=examination,fill=value*100,label=round(value*100,0))) +
  geom_tile() +
  # geom_text() +
  scale_fill_gradient2(name="Proportion",low="darkgreen",mid = "yellow",high="red",midpoint = 50) +
  theme_bw() +
  xlab("Age (years)") +
  ylab("") +
  theme(legend.position = "bottom",
        axis.text = element_text(size=20)) +
  scale_x_continuous(limits=c(10,30),breaks=seq(10,30,by=5))



require(ggpubr)
fig2 %>% 
  ggsave(.,filename = paste0(path_youth_folder,"/figures/research strategy figure_heatmap of symptoms.png"),width=8,height=10)


fig2_df %>% 
  distinct(study_id,.keep_all=TRUE) %>% 
  group_by(study) %>% 
  tally()

fig2_ids <- unique(fig2_df$study_id)

pdf(file = paste0(path_youth_folder,"/figures/research strategy figure_individual heatmaps.pdf"),width = 8,height=4)
for (i in fig2_ids){
  
  study = fig2_df %>% 
    dplyr::filter(study_id == i) %>% 
    dplyr::select(study) %>% 
    pull() %>% 
    unique()
  title = paste0("id = ",i," | study: ",study);
  
  figA <- fig2_df %>%
    dplyr::filter(study_id == i) %>% 
    dplyr::select(study,study_id,age,Deformities,Dryskin,Infection,Fissure,
                  Ulceration,Reflex, Perception, Filament) %>% 
    pivot_longer(cols=-one_of(c("study","study_id","age")),
                 names_to="examination",values_to = "value") %>% 
    group_by(age,examination) %>% 
    summarize(value = mean(value,na.rm=TRUE),
              n = sum(!is.na(value))) %>% 
    ungroup() %>% 
    mutate(examination = factor(examination,
                                levels=c("Deformities","Dryskin","Infection","Fissure",
                                         "Ulceration","Reflex", "Perception", "Filament"),
                                labels=c("Deformities","Dry skin","Infection","Fissure",
                                         "Ulceration","Poor Reflex", "Poor Perception", "Abnormal Filament")
    )) %>%  
    ggplot(data=.,aes(x=age,y=examination,fill=value*100,label=round(value*100,0))) +
    geom_tile() +
    # geom_text() +
    scale_fill_gradient2(name="Proportion",low="darkgreen",mid = "yellow",high="red",midpoint = 50) +
    theme_bw() +
    xlab("Age (years)") +
    ylab("") +
    theme(legend.position = "bottom") +
    scale_x_continuous(limits=c(10,30),breaks=seq(10,30,by=5)) 
  
  figA %>% 
    annotate_figure(., top = text_grob(title, 
                                       color = "black", face = "bold", size = 12)) %>% 
    print(.)
  
  
}

dev.off()


