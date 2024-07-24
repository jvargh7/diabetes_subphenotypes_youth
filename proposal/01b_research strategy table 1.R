source("code/Rprofile_run.R")

# Table for Youth Phenotypes proposal using SEARCH and TODAY

search <- readRDS(paste0(path_youth_folder,"/working/search/baseline.RDS"))  %>% 
  mutate(study = "SEARCH") %>% 
  dplyr::filter(age >= 10,age<20) %>% 
  dplyr::filter(dmduration >= 0 & dmduration <= duration_cutoff) %>% 
  mutate(study_id = as.character(study_id))

today <- readRDS(paste0(path_youth_folder,"/working/today/baseline.RDS"))  %>% 
  mutate(study = "TODAY") %>% 
  dplyr::filter(dmduration == "<=5 months")


tab1 <- bind_rows(search,
                  today %>% dplyr::select(-age,-bmiz,-wc,
                                          -dmduration,-bmipct)) %>% 
  dplyr::select(study,study_id,
                female,race,age,bmiz,hba1c,glucosef,
                insulinf,cpeptidef,ldlc,hdlc,tgl,
                sbp,dbp
                ) %>% 
  mutate(nonwhite = case_when(race == "white" ~ 0,
                              !is.na(race) ~ 1,
                              TRUE ~ NA_real_),
         tgl_hdlc = case_when(!is.na(hdlc) ~ tgl/hdlc,
                             TRUE ~ NA_real_)) %>% 
  group_by(study) %>% 
  dplyr::summarize_at(vars(female,nonwhite,age,bmiz,
                           hba1c,glucosef,insulinf,
                           cpeptidef,ldlc,hdlc,tgl,tgl_hdlc,sbp,dbp),
                      .f=function(x) list(n = sum(!is.na(x)),
                                          
                                          mean = round(mean(x,na.rm=TRUE),1))) %>% 
  mutate_all(~as.character(.))


write_csv(tab1,"code/01b_research strategy table 1.csv")

