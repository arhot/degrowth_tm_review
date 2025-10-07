library(janitor)
dfm_all_data <- readRDS("data/dfm_data_no_parriquee_manual_stopwrod_removeal_28122023.RDS")

model <- read_rds("results/many_models_20K_manual_stopwords_28122023.RDS")

metadata <- stm_all_data$meta 
td_gamma <- tidy(model$topic_model[[1]], matrix = "gamma",
                 document_names = metadata$DOI) %>% 
  left_join(metadata, by=c("document"="DOI"))

most_likely_topic <- td_gamma %>%
  group_by(document) %>%
  top_n(1, wt = gamma) %>%
  ungroup()

str_to_proper_title <- function(title) {
  # Define small words that should not be capitalized in titles
  small_words <- c(" of ", " and ", " in ", " the ", " on ", " at ", " for ", " with ", " a ", " an ", " but ", " or ", " nor ", " to ", " as ")
  
  # Convert to title case
  title <- str_to_title(title)
  
  # Ensure small words are in lowercase
  for (word in small_words) {
    title <- str_replace_all(title, fixed(str_to_title(word)), tolower(word))
  }
  
  return(title)
}

table1 <- most_likely_topic %>% mutate(Journal = Journal %>% str_to_proper_title() %>% fct_infreq() %>% fct_explicit_na() %>% fct_lump_min(, min=6)) %>% tabyl(Journal, show_na = F) %>%
  adorn_pct_formatting(digits = 2)



