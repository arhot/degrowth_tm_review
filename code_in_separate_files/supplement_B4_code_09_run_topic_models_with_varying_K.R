library(quanteda)
library(stm)
library(tidytext)
library(tidyverse)
library(ggtext)
library(ggthemes)

dfm_all_data <- readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")

stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format
  
many_models <- tibble(K = c(10:25)) %>%
  mutate(topic_model = map(K, ~stm(documents = stm_all_data$documents, 
                                   vocab = stm_all_data$vocab, K = ., 
                                   prevalence = ~s(Year), 
                           init.type = "Spectral", data=stm_all_data$meta,
                                          verbose = T)))

write_rds(many_models, "results/degrowth_models_with_varying_K_02022025.RDS")

# save needed data for making single paper plots 

dfm_all_data <- readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")

model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

first_model <- model %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

td_gamma <- tidy(first_model, matrix = "gamma",
                 document_names = stm_all_data$meta$Title) %>% 
  mutate(doc_id = rep(seq_len(nrow(stm_all_data$meta)), 20))    %>% 
  left_join(stm_all_data$meta %>% mutate(doc_id = row_number()), by=c("doc_id"))

topic_labels <- readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels, topic_type_for_presentations) 

td_gamma <- td_gamma %>% left_join(topic_labels)

write_csv(td_gamma, "supplements_for_submission/supplement_B3_all_papers_with_share_from_each_topic_and_metadata.csv")
