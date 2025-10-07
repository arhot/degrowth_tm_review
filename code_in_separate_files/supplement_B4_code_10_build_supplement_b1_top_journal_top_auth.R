library(tidyverse)
library(stringdist)
library(fuzzyjoin)
library(tidytext)
source("supplements_for_submission/supplement_A5_code_04_helper_functions_for_data_cleaning.R")

# load model
model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)
dfm_all_data <-  readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")
stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format

# build df with columns:
# topic number & top 20 words

top_words <- tibble(map_dfr(model$topic_model, extract_top_terms_per_topic, n = 20) %>% select(topic, top_terms_vector)) %>% 
  left_join(map_dfr(model$topic_model, extract_top_terms_per_topic, n=20, matrix = "frex") %>% select(topic, FREX=top_terms_vector))

topic_labels <- readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels) 
topic_labels <- topic_labels %>% left_join(top_words)
  
# top authors & journals


first_model <- model %>% 
  pull(topic_model) %>% 
  .[[1]]

metadata <- stm_all_data$meta 
td_gamma <- tidy(first_model, matrix = "gamma",
                 document_names = metadata$DOI) %>% 
  left_join(metadata, by=c("document"="DOI")) %>% left_join(topic_labels %>% select(topic, labels))

most_likely_topic <- td_gamma %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup()

# top journals

most_likely_journal_per_topic <- most_likely_topic %>%
  group_by(topic, Journal) %>%
  summarise(count = n()) %>%
  arrange(topic, desc(count)) %>%
  group_by(topic) %>%
  slice_head(n = 5) %>%
  summarize(most_common_journals = paste(Journal, "(", count, ")", collapse = "; \n"))

top_words <- top_words %>% left_join(most_likely_journal_per_topic)

# top authors

most_likely_topic <- td_gamma %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup()

most_likely_topic_table_for_excel <- td_gamma %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>% group_by(topic) %>% arrange(desc(gamma), .by_group = T) %>% left_join(topic_labels %>% select(topic, labels))
  

most_likely_topic_table_for_excel <- most_likely_topic_table_for_excel %>%
  group_by(topic) %>%
  group_split() %>%
  set_names(nm = unique(most_likely_topic_table_for_excel$labels)) # Setting names for each sheet

writexl::write_xlsx(most_likely_topic_table_for_excel, "supplements_for_submission/supplement_B2_all_data_with_main_topic_share_from_main_topic.xlsx")


most_likely_topic_long_author <- most_likely_topic %>%
  separate_rows(Authors, sep = ";") %>%
  mutate(Authors = trimws(Authors)) 

most_likely_author_per_topic <- most_likely_topic_long_author %>%
  group_by(topic, Authors) %>%
  summarise(count = n()) %>%
  arrange(topic, desc(count)) %>%
  group_by(topic) %>%
  slice_head(n = 10) %>%
  summarize(most_common_authors = paste(Authors, collapse = "; \n"))

top_words <- top_words %>% left_join(most_likely_author_per_topic)


# publications (top 5 docs with highest prop)

top10_documents_per_topic <- td_gamma %>%
  group_by(topic) %>%
  arrange(desc(gamma), .by_group = TRUE) %>%
  slice(1:10) %>% select(topic, Journal, Title ,document) %>%
  summarize(most_typical_papers = paste(Journal, ":", Title, ":", document, collapse = "; \n")) %>% ungroup()

top_words <- top_words %>% left_join(top10_documents_per_topic)

top_words <- top_words%>%
  left_join(gamma_terms %>% select(topic, gamma) %>% mutate(topic = as.numeric(topic))) 

top_words <- top_words %>% arrange(desc(gamma))

writexl::write_xlsx(top_words, "supplements_for_submission/supplement_B1_topics_with_most_common_journals_authors_and_most_representative_papers.xlsx")

