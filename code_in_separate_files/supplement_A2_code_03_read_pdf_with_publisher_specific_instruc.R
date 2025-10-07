library(pdftools)
library(glue)
library(stringi)
library(stringdist)
library(fuzzyjoin)
library(tidyverse)
library(rvest)
library(rcrossref)
source("supplements_for_submission/supplement_A5_code_04_helper_functions_for_data_cleaning.R")
source("supplements_for_submission/supplement_A4_code_03_read_pdf_with_publisher_specific_instructions_for_cleaning.R")

rout_files <- list.files("data_rout/") %>% as_tibble()

just_rout <- just_rout %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>%
  stringdist_left_join(rout_files, by = c("Title" = "value"), 
                       method = "jw", 
                       max_dist = 0.2, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_rout/", value, sepa=""), NA_character_))
just_rout <- just_rout %>%
  mutate(across(where(is.list), unlist))

# write.csv2(just_rout, "data/rout_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_rout <- just_rout %>%
  mutate(full_text = map(.x=path_and_file, .f=read_and_clean_pdf, mode="rout"))

just_rout <- just_rout %>%
  mutate(across(where(is.list), unlist))

saveRDS(just_rout, "data/df_by_publisher_with_full_text/rds/routledge.RDS")

# MDPI
mdpi_files <- list.files("data_mdpi/") %>% as_tibble()

# pdf_link2
# sourcetitle
# number between {xxxx-xxxx/} and next /
# digits before /pdf

just_mdpi_with_filename <- just_mdpi %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x),
         source_title_in_lower = tolower(`Source Title`),
           digits_1 = str_match(pdf_link2, "\\d+-\\d+/\\s*(.*?)\\s*/")[,2], 
         digits_2 = str_match(pdf_link2, "/(\\d+)/pdf")[,2],
         to_match_filename = paste(source_title_in_lower, digits_1, digits_2, ".pdf", sep="-")) %>%
  stringdist_left_join(mdpi_files, by = c("to_match_filename" = "value"), 
                       method = "jw", 
                       max_dist = 0.1, # set to 0.1 for 90% similarity
                       distance_col = "dist") 

just_missing <- just_mdpi_with_filename  %>% filter(is.na(dist))

just_mdpi_with_filename <- just_mdpi_with_filename %>%
  group_by(DOI) %>%
  filter(dist == min(dist, na.rm = TRUE)) 


mdpi_for_check <- bind_rows(just_mdpi_with_filename, just_missing) %>% 
  mutate(across(where(is.list), unlist))

write.csv2(mdpi_for_check, "data/mdpi_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_mdpi_with_filename <-  read_delim("data/mdpi_checked.csv", 
                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_mdpi/", value, sep=""), NA_character_))

just_mdpi_with_filename <- just_mdpi_with_filename %>%
  mutate(full_text = map(.x=path_and_file, .f=read_and_clean_pdf, mode="mdpi")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_mdpi_with_filename, "data/df_by_publisher_with_full_text/rds/mdpi.RDS")

# Wiley
wiley_files <- list.files("data_wiley/") %>% as_tibble() %>%
  mutate(filename = value) %>% 
  separate(value, into = c("Category", "Year", "Author", "Title"), sep = " - ", extra = "merge")

just_wiley_fn <- just_wiley %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(wiley_files, by = c("Title"), 
                       method = "jw", 
                       max_dist = 0.2, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(filename), paste("data_wiley/", filename, sep=""), NA_character_))


wiley_for_check <- just_wiley_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(wiley_for_check, "data/wiley_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_wiley_with_filename <-  read_delim("data/wiley_checked.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_wiley_with_filename <- just_wiley_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, col_format="two_col", mode="wiley")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_wiley_with_filename, "data/df_by_publisher_with_full_text/rds/wiley.RDS")

# Sage

sage_files <- list.files("data_sage/") %>% as_tibble() %>%
  mutate(
    filename = value, 
    Author = str_extract(filename, ".*(?=-\\d{4})"),
    Year = str_extract(filename, "\\d{4}"),
    Title = str_extract(filename, "(?<=\\d{4}-).*(?=\\.pdf)")
  ) %>%
  mutate(
    title = str_replace_all(Title, "-", " ")
  )

just_sage_fn <- just_sage %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(sage_files, by = c("Title"), 
                       method = "jw", 
                       max_dist = 0.32, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(filename), paste("data_sage/", filename, sep=""), NA_character_))


sage_for_check <- just_sage_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(sage_for_check, "data/sage_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_sage_with_filename <-  read_delim("data/sage_checked.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_sage_with_filename <- just_sage_with_filename %>%
  mutate(full_text = map_chr(path_and_file, read_and_clean_pdf)) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_sage_with_filename, "data/df_by_publisher_with_full_text/rds/sage.RDS")

# Springer

springer_files <- list.files("data_springer/") %>% as_tibble()

just_springer_fn <- just_springer %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x),
         DOI_without_journal_code = str_extract(DOI, "(?<=/).*")) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(springer_files, by = c("DOI_without_journal_code"="value"), 
                       method = "jw", 
                       max_dist = 0.1, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_springer/", value, sep=""), NA_character_))


springer_for_check <- just_springer_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(springer_for_check, "data/springer_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_springer_with_filename <-  read_delim("data/springer_checked.csv", 
                                        delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_springer_with_filename <- just_springer_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, col_format="two_col")) %>% 
  mutate(across(where(is.list), unlist))

springer_addeds <- springer_for_check %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, col_format="two_col")) %>% 
  mutate(across(where(is.list), unlist))


just_springer_with_filename <- just_springer_with_filename %>% filter(!is.na(full_text)) %>% select(-dist) %>% 
  full_join(springer_addeds %>% select(-dist))

saveRDS(just_springer_with_filename, "data/df_by_publisher_with_full_text/rds/springer.RDS")

# Nature

nature_files <- list.files("data_nature/") %>% as_tibble() 
just_nature_fn <- just_nature %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x),
         end_of_DOI = DOI %>% str_extract("(?<=/).*")) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(nature_files, by = c("end_of_DOI"="value"), 
                       method = "jw", 
                       max_dist = 0.2, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_nature/", value, sep=""), NA_character_))


nature_for_check <- just_nature_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(nature_for_check, "data/nature_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_nature_with_filename <-  read_delim("data/nature_checked.csv", 
                                           delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_nature_with_filename <- just_nature_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, col_format="two_col")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_nature_with_filename, "data/df_by_publisher_with_full_text/rds/nature.RDS")



# T&F


taylor_files <- list.files("data_taylor/") %>% as_tibble()

just_taylor_fn <- just_taylor %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(taylor_files, by = c("Title"="value"), 
                       method = "jw", 
                       max_dist = 0.2, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_taylor/", value, sep=""), NA_character_))


taylor_for_check <- just_taylor_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(taylor_for_check, "data/taylor_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_taylor_with_filename <-  read_delim("data/taylor_checked.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_taylor_with_filename <- just_taylor_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, mode="two_col")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_taylor_with_filename, "data/df_by_publisher_with_full_text/rds/taylor.RDS")


# Cambridge



emerald_files <- list.files("data_cup/") %>% as_tibble()

just_camb_fn <- just_camb %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(camb_files %>% 
                         mutate(Title = str_replace_all(value, "-", " ")), 
                       method = "jw", 
                       max_dist = 0.3, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_camb/", value, sep=""), NA_character_))


camb_for_check <- just_camb_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(camb_for_check, "data/camb_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_camb_with_filename <-  read_delim("data/camb_checked.csv", 
                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_camb_with_filename <- just_camb_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, no_space_after_references="no")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_camb_with_filename, "data/df_by_publisher_with_full_text/rds/camb.RDS")


# Emerald



emerald_files <- list.files("data_emerald/") %>% as_tibble()

just_emerald_fn <- just_emerald %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(emerald_files, by=c("DOI"="value"), 
                       method = "jw", 
                       max_dist = 0.3, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_emerald/", value, sep=""), NA_character_))


emerald_for_check <- just_emerald_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(emerald_for_check, "data/emerald_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_emerald_with_filename <-  read_delim("data/emerald_checked.csv", 
                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_emerald_with_filename <- just_emerald_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, no_space_after_references="no")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_emerald_with_filename, "data/df_by_publisher_with_full_text/rds/emerald.RDS")

# Frontiers

front_files <- list.files("data_front/") %>% as_tibble()

just_front_fn <- just_front %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x),
         DOI_without_journal_code = str_extract(DOI, "(?<=/).*")) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(front_files, by=c("DOI_without_journal_code"="value"), 
                       method = "jw", 
                       max_dist = 0.3, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_front/", value, sep=""), NA_character_))


front_for_check <- just_front_fn %>% 
  mutate(across(where(is.list), unlist))

write.csv2(front_for_check, "data/front_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

just_front_with_filename <-  read_delim("data/front_checked.csv", 
                                          delim = ";", escape_double = FALSE, trim_ws = TRUE)

just_front_with_filename <- just_front_with_filename %>%
  mutate(full_text = map_chr(.x=path_and_file, .f=read_and_clean_pdf, col_format="two_col")) %>% 
  mutate(across(where(is.list), unlist))

saveRDS(just_front_with_filename, "data/df_by_publisher_with_full_text/rds/front.RDS")
# Arizona

arizona_files <- list.files("data_arizona/") %>% as_tibble() %>% 
  mutate(path_and_file = paste("data_arizona/", value, sep=""),
       full_text = map_chr(path_and_file, read_and_clean_pdf),
       first_7_words = word(full_text, start=1, end=7)) %>% 
  mutate(across(where(is.list), unlist))

just_arizona_fn <- just_arizona %>%
  mutate(Title = ifelse(is.na(Title.x), Title.y, Title.x)) %>% select(-Title.x, -Title.y) %>%
  stringdist_left_join(arizona_files, by=c("Title"="first_7_words"), 
                       method = "jw", 
                       max_dist = 0.8, # set to 0.1 for 90% similarity
                       distance_col = "dist") %>%
  mutate(path_and_file = ifelse(!is.na(value), paste("data_arizona/", value, sep=""), NA_character_))


arizona_for_check <- just_arizona_fn %>%  group_by(DOI) %>%
  filter(dist == min(dist, na.rm = TRUE)) %>% 
  mutate(across(where(is.list), unlist)) %>% select(-full_text)

write.csv2(arizona_for_check, "data/arizona_for_check.csv", row.names=F, fileEncoding = "WINDOWS-1252")

# just_arizona_with_filename <-  read_delim("data/arizona_checked.csv", 
#                                         delim = ";", escape_double = FALSE, trim_ws = TRUE)

arizona_done <- just_arizona_fn %>%  group_by(DOI) %>%
  filter(dist == min(dist, na.rm = TRUE)) %>% 
  mutate(across(where(is.list), unlist)) %>% 
  slice(1)

saveRDS(arizona_done, "data/df_by_publisher_with_full_text/rds/arizona.RDS")

# white horse

horse_files <- list.files("data/data_env_values_ingenta/") %>% as_tibble() %>% mutate(study_id = str_extract(value, ".*(?=\\.pdf)"))
just_horse_fn <- just_horse %>% left_join(horse_files, by=c("study_id")) %>% 
  mutate(path_and_file = ifelse(!is.na(value), paste("data/data_env_values_ingenta/", value, sep=""), NA_character_))

just_horse_fn <- just_horse_fn %>%
  mutate(full_text = map(.x=path_and_file, .f=read_and_clean_pdf, mode="horse"))

just_horse_fn <- just_horse_fn %>%
  mutate(full_text_scopus = unlist(full_text_scopus), full_text = unlist(full_text))

saveRDS(just_horse_fn, "data/df_by_publisher_with_full_text/rds/horse.RDS")
