files <- list.files("data/df_by_publisher_with_full_text/rds/", full.names = TRUE)

read_and_select_and_rename <- function(path_and_file){
  df <- read_rds(path_and_file) 
  if (!("Title" %in% names(df))) {
    df <- df %>% mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) 
  }
    df <- df %>% 
             select(DOI, Journal="Source Title", Title, Authors = Authors.x, Year = Year.x, full_text)
return(df)
}

df <- read_rds(files[7]) %>% 
  mutate(across(where(is.list), unlist))

all_files <- files %>% as_tibble %>% 
  mutate(df = map(value, read_and_select_and_rename)) %>% unnest(df)

# add scopus

scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023 <- readRDS("C:/Users/atoikka/OneDrive - University of Helsinki/analyyseja/degrowth_tm_review/scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023.RDS") %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% 
  select(DOI, Journal="Source Title", Title, Authors = Authors.x, Year = Year.x, full_text=full_text_scopus) %>% 
  mutate(value="/scopus.rds", 
         across(where(is.list), unlist)) %>% filter(!is.na(full_text)) %>% 
  mutate(full_text = full_text %>% str_replace_all("serial JL[^']*((Elsevier B\\.V\\.)|(Elsevier Ltd))", ""))

scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023 <- 
  scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023 %>%
  mutate(full_text = full_text %>% str_replace("References[^ ]+$", ""))


all_files <- all_files %>% bind_rows(scopus_and_wos_combined_with_DOI_corrections_AND_full_textSD_26062023)

all_files <- all_files %>% mutate(publisher = str_extract(value, "(?<=/)[^/]+(?=\\.RDS)"))
table(all_files$Year)
table(all_files$Title, useNA=c("always"))

saveRDS(all_files, "data/all_data_with_full_text_07072023.RDS")
# write.csv2(all_files %>% select(-full_text), "data/all_data_for_manual_check_no_full_text_05072023v2.csv", row.names = F, fileEncoding = "WINDOWS-1252")


# read checked, delete 
deletions <- read_delim("data/all_data_manually_checked_no_full_text_05072023.csv", 
                                                              delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(DOI, delete)


# delete duplicates

dupl <- all_files %>%
  group_by(DOI) %>% filter(n()> 1)

all_files <- all_files[!(duplicated(all_files$DOI)),]

# empties 

issues <- all_files %>% filter(is.na(full_text))

all_files <- all_files %>% 
  left_join(deletions) %>% filter(is.na(delete)) 

all_files_with_full_text <- all_files %>% filter(!is.na(full_text))

# saveRDS(all_files_with_full_text, "data/all_files_with_full_text07072023.RDS")

# add new publisher entries after manual review

# filesbatch2 <- list.files("data/df_by_publisher_with_full_text_batch2/", full.names = TRUE)
# batch2_files <- filesbatch2 %>% as_tibble %>% 
#   mutate(df = map(value, read_and_select_and_rename)) %>% unnest(df)

# data/df_by_publisher_with_full_text_batch2/


# sample from each publisher

table(all_files$publisher, useNA="always")


samples_for_reading <- all_files %>% mutate(publisher = ifelse(is.na(publisher), "scopus", publisher)) %>% 
  group_by(publisher) %>%
  sample_n(size = 5) %>%
  mutate(end_of_DOI = DOI %>% str_extract("(?<=/).*"))

# Write to separate text files
for(i in seq_len(nrow(samples_for_reading))) {
  # Create filename
  filename <- paste0("data/data_for_manual_checks/", samples_for_reading$publisher[i], samples_for_reading$end_of_DOI[i], ".txt")
  
  # Write full_text to file
  writeLines(samples_for_reading$full_text[i], filename)
}












