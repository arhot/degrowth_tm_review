all_search_results <- readRDS("C:/Users/atoikka/OneDrive - University of Helsinki/analyyseja/degrowth_tm_review/scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023.RDS")
just_rout <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)rout") | str_detect(Publisher.y, "(?i)rout")) %>%
  mutate(study_id = paste("Rout", row_number(), sep="")) %>% as.data.frame()
just_mdpi <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)mdpi") | str_detect(Publisher.y, "(?i)mdpi")) %>%
  mutate(study_id = paste("mdpi", row_number(), sep="")) %>% as.data.frame()
just_wiley <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)wiley") | str_detect(Publisher.y, "(?i)wiley")) %>%
  mutate(study_id = paste("wiley", row_number(), sep="")) %>% as.data.frame()
just_sage <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)sage") | str_detect(Publisher.y, "(?i)sage")) %>%
  mutate(study_id = paste("sage", row_number(), sep="")) %>% as.data.frame()
just_springer <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)springer") | str_detect(Publisher.y, "(?i)springer")) %>%
  mutate(study_id = paste("springer", row_number(), sep="")) %>% as.data.frame()
just_nature_with_filename <-  read_delim("data/nature_checked.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
just_taylor_with_filename <-  read_delim("data/taylor_checked.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)
just_camb <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)camb") | str_detect(Publisher.y, "(?i)camb")) %>%
  mutate(study_id = paste("camb", row_number(), sep="")) %>% as.data.frame()
just_emerald <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)emerald") | str_detect(Publisher.y, "(?i)emerald")) %>%
  mutate(study_id = paste("emerald", row_number(), sep="")) %>% as.data.frame()

just_front_with_filename <-  read_delim("data/front_checked.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)
just_horse <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)horse") | str_detect(Publisher.y, "(?i)horse")) %>%
  mutate(study_id = paste("horse", row_number(), sep="")) %>% as.data.frame()
just_horse <- all_search_results %>% filter(str_detect(Publisher.x, "(?i)horse") | str_detect(Publisher.y, "(?i)horse")) %>%
  mutate(study_id = paste("horse", row_number(), sep="")) %>% as.data.frame()
