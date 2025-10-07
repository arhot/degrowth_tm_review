dfm <- readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")
library(quanteda)
library(tidyverse)


# total counts
total_counts <- colSums(dfm)

# number of docs where feature appears
doc_freq <- docfreq(dfm)

# combine into a tibble
feature_stats <- tibble(
  feature = names(total_counts),
  total_count = as.integer(total_counts),
  n_docs = as.integer(doc_freq)
)

# preview
feature_stats %>% arrange(desc(total_count)) %>% head(10)

# save the results
writexl::write_xlsx(feature_stats, "supplements_for_submission/supplement_A4_total_term_occurrences.xlsx")
