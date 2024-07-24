

data_extract <- function(study_name, vl_column,data_path,df_name = character()){
  
  if(identical(df_name,character(0))){
    df_name = vl_column
  }
  
  
  # vl_sheet = readxl::read_excel("C:/code/phenotypes/shared/data/Phenotypes Variable List.xlsx",
  #                               sheet=study_name)
  vl_sheet = readxl::read_excel("C:/Users/JGUO258/Documents/JGUO/diabetes_subphenotypes_youth/data/Phenotypes Variable List.xlsx",
                                sheet=study_name)
  
  var_names <- vl_sheet %>% 
    dplyr::select(label,harmonized,one_of(vl_column)) %>% 
    rename_at(vars(one_of(vl_column)),~"selected") %>% 
    dplyr::filter(!is.na(selected))
  
  names_to_upper <- c("DPPOS","DPP","JHS","SEARCH","TODAY")
  
  if(study_name %in% names_to_upper){
    
    var_names$selected <- str_to_upper(var_names$selected)
    
    df <- read_csv(paste0(data_path,"/",df_name,".csv")) %>% 
      rename_all(~str_to_upper(.)) %>% 
      dplyr::select(one_of(var_names$selected)) 
  }
  if(!study_name %in% names_to_upper){
      df <- read_csv(paste0(data_path,"/",df_name,".csv")) %>% 
        dplyr::select(one_of(var_names$selected)) 
  
  }
  
  
  harmonized_vars <- var_names %>% 
    dplyr::filter(selected %in% names(df))
  
  df_cleaned <- df %>% 
    rename_at(vars(one_of(harmonized_vars$selected)),~harmonized_vars$harmonized)
  
  return(df_cleaned)
  
  
}
