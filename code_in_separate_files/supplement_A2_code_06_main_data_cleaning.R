library(quanteda)
library(cld2)
library(cld3)
library(readxl)

all_data_with_full_text <- read_rds("data/all_data_with_full_text11072023.RDS")

all_data_no_full_text <- all_data_with_full_text %>% select(-full_text)
#write_xlsx(all_data_no_full_text, "data/all_data_no_full_text12072023.xlsx")

corrections <- read_xlsx("data/all_data_no_full_text12072023_manual_checks.xlsx")

all_data_with_full_text_after_manual_check <- corrections %>% left_join(all_data_with_full_text %>% select(DOI, full_text))

deletions <- read_delim("data/all_data_manually_checked_no_full_text_05072023.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(DOI, delete) %>% filter(delete=="x")

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% anti_join(deletions)

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>%
  filter(!duplicated(DOI))

# further cleaning after first models

# topic 1 

topic_1_issues <- all_data_with_full_text_after_manual_check %>% filter(Journal=="ECOLOGICAL ECONOMICS") %>%
  mutate(end_of_DOI = DOI %>% str_extract("(?<=/).*"))



topic_1_issues_fixed <- topic_1_issues %>% 
  mutate(text_bits = str_split(full_text, "(?i)[[:space:]]references"),
         how_many_bits = lengths(text_bits),
         last_bit = map_chr(text_bits, ~ tail(.x, 1)),
         text_fixed = ifelse(how_many_bits > 1, str_replace(full_text, fixed(last_bit), ""), full_text)) %>% 
  select(DOI, text_fixed)

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% 
  left_join(topic_1_issues_fixed) %>% 
  mutate(full_text = ifelse(!is.na(text_fixed), text_fixed, full_text))


# topic 18
library(readxl)
first_model_top_papers_per_topic_13072023_remove_a_few <- read_excel("results/first_model_top_papers_per_topic_13072023_remove_a_few.xlsx") %>%
  select(document, delete) %>% filter(delete=="x")

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% anti_join(first_model_top_papers_per_topic_13072023_remove_a_few, by=c("DOI"="document"))

# manual fixes after corpus review

tarkastukset <- read_rds("data/manually_checking_all_texts_still_checked_13122023.RDS") %>% select(DOI, action)
tarkastukset <- tarkastukset[!duplicated(tarkastukset$DOI),] 

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% left_join(tarkastukset)

# remove_from_data 

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% filter(action != "remove_from_data" | is.na(action))

# check_manually 

tsekatut <- read_rds("data/manually_checked_last_bits_28122023.RDS")

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>%
  left_join(tsekatut, by="DOI", suffix = c("_left", "_right")) %>%
  mutate(action = coalesce(action_right, action_left)) %>%
  select(-ends_with("_right"), -ends_with("_left"))

# delete_text_after_last_references

deleted_after_last_references <- all_data_with_full_text_after_manual_check %>% filter(action == "delete_text_after_last_references") %>% 
  mutate(text_bits = str_split(full_text, "(?i)[[:space:]]R\\s*e\\s*f\\s*e\\s*r\\s*e\\s*n\\s*c\\s*e\\s*s"),
         how_many_bits = lengths(text_bits),
         last_bit = map_chr(text_bits, ~ tail(.x, 1)),
         text_fixed = ifelse(how_many_bits > 1, str_replace(full_text, fixed(last_bit), ""), full_text)) %>%
  select(DOI, text_fixed)

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% 
  left_join(deleted_after_last_references) %>% 
  mutate(full_text = ifelse(!is.na(text_fixed), text_fixed, full_text))

# make corpus #1 without using texts added from Parriquee's list

all_data_with_full_text_after_manual_check_no_parriquee <- all_data_with_full_text_after_manual_check %>%
  filter(value!="data/data_parriquee_gap_fills/")

all_data_with_full_text_after_manual_check_no_parriquee <- all_data_with_full_text_after_manual_check_no_parriquee %>%
  mutate(
    full_text_clean = mapply(
      function(authors, text) {
        # Split the authors string into individual names
        author_names <- str_split(authors, ";\\s*")[[1]]
        # Extract the last names
        last_names <- sapply(author_names, function(name) {
          str_extract(name, "[^,]+")
        })
        # Create a regex pattern to match any of the last names
        pattern <- paste(last_names, collapse = "|")
        # Replace the last names in the text with an empty string
        str_replace_all(text, pattern, "")
      },
      Authors, full_text
    )
  )

# full_text is the original, full_text_clean has author names removed - decide here 
all_data_corpus <- corpus(all_data_with_full_text_after_manual_check_no_parriquee %>% select(DOI, Year, Journal, Title, Authors, full_text), text_field="full_text")

all_data_tokens <- all_data_corpus %>% tokens(what="word",
                                              remove_punct = T,
                                              remove_symbols = T,
                                              remove_numbers = T,
                                              remove_url = T,
                                              remove_separators = T)

dfm_all_data <- dfm(all_data_tokens)

dfm_all_data <- dfm_select(dfm_all_data, pattern = stopwords("en"), selection = "remove")
dfm_all_data <- dfm_keep(dfm_all_data, min_nchar = 3)
dfm_all_data <- dfm_trim(dfm_all_data, min_docfreq = 0.02, docfreq_type = "prop")

# hunspell::dictionary(lang = "en_US")
# english_words <- readLines("C:/rlibs/4.3.1/hunspell/dict/en_US.dic") %>% gsub("/.+", "", .)

# dfm_all_data <- dfm_all_data %>% dfm_keep(english_words, valuetype="fixed")
# topfeatures(dfm_all_data, 100)


dg_specific_stopwords <- c("also", "new", "also", "use", "may", "one", "two", "fig", "thus", "see", "data", "can", "many",
                           "university", "journal", 
                           "with", "within", 
                           "den", "les",
                           "dunlap", "robbins", "something",
                           "nline", "iley", "ibrary", "nloaded", "elsinki", "erm", "niversity", "icense", "onditions",
                           "gif", "jpg", "sml", month.abb %>% tolower(), 
                           "mon", "tue", "wed", "thu", "fri", "sat", "sun",
                           "remote_assr", "username", 
                           "routledge", "altimg", "https", "doi", "con-", "com-", "tion", "ing",
                           "ecol", "econ", "environ", "rev", "res", "soc", "sci", "oxford", "ons", "s-and-conditions",
                           "reative", "press", "london", 
                           "accessed", "prod", "york", "int", "cambridge", "elgar", "stud", "polit",
                           "eds", "nat", "resour", "glob", "conserv"
                           )

dfm_all_data <- dfm_select(dfm_all_data, pattern = dg_specific_stopwords, selection = "remove")

selected_features <- grep("-$", featnames(dfm_all_data), value = TRUE, invert = F)
dfm_all_data <- dfm_select(dfm_all_data, selected_features, selection = "remove")


write_rds(dfm_all_data, "data/dfm_data_for_manual_cleaning_28122023.RDS")

df_features <- tibble(term=featnames(dfm_all_data))

writexl::write_xlsx(df_features, "data/terms_for_manual_check_28112023.xlsx")

manual_check_done <- readxl::read_xlsx("data/terms_for_manual_check_check_done28112023.xlsx") %>% filter(delete=="x")

dfm_all_data_manual <- dfm_select(dfm_all_data, manual_check_done$term, selection = "remove")


write_rds(dfm_all_data_manual, "data/dfm_data_withoutstopwords_final_28122023.RDS")
