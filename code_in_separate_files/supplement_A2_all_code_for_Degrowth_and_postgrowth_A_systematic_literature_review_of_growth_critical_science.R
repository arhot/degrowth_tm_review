
# This file includes all the code for the analyses in Degrowth and postgrowth: A systematic literature review of growth-critical science

# ---- Core data handling ----
library(tidyverse)   # includes dplyr, ggplot2, tidyr, readr, purrr, stringr, tibble, etc.
library(janitor)
library(readxl)

# ---- Text processing & topic modeling ----
library(quanteda)
library(stm)
library(tidytext)
library(cld2)
library(cld3)

# ---- Similarity & fuzzy matching ----
library(stringdist)
library(fuzzyjoin)

# ---- Visualization ----
library(ggtext)
library(ggthemes)
library(scales)
library(ggpattern)
library(ggrepel)
library(ggraph)
library(igraph)
library(networkD3)
library(PRISMAstatement) # Has been removed from CRAN, but still available on GitHub.
library(tidygraph)     
library(DiagrammeRsvg)  
library(rsvg)     

# ---- Utilities ----
library(broom)
library(stminsights)
library(reshape2)  # used for melt() in heatmap step

# ---- Repeatedly used helper functions


read_and_clean_pdf <- function(path_and_file, mode="not_rout", col_format="one_col", no_space_after_references="yes") {
  print(path_and_file)
  a <<- a + 1 
  print(a)
  if (is.na(path_and_file)) {
    return(NA_character_)
  } 
  if(col_format=="two_col"){
    text <- tabulizer::extract_text(path_and_file)  
  }
  else {
    text <- pdf_text(path_and_file)
    if(mode=="rout") {
      text <- text[-1] %>% glue_collapse()
    } else { text <- text %>% glue_collapse() }
  }
  # drop everything after "\n\n\nAcknowledgements\n", 
  # drop everything after "\n\n\nDisclosure statement\n" 
  # \n\n\nNotes on contributors\n
  text <- str_replace_all(text, "-\n", "")
  text <- ifelse(str_detect(text, "(?i)\nReferences\n"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)\nReferences\n)"), text)
  text <- ifelse(str_detect(text, "(?i)\r\nReferences\r\n"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)\r\nReferences\r\n)"), text)
  if(no_space_after_references=="no") {
    text <- ifelse(str_detect(text, "(?i)\nReferences"), 
                   str_extract(text,pattern = "(?s).*(?=(?i)\nReferences)"), text)
  }
  text <- ifelse(str_detect(text, "(?i)Acknowledgements"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)Acknowledgements)"), text)
  text <- ifelse(str_detect(text, "(?i)Acknowledgment"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)Acknowledgment)"), text)
  text <- ifelse(str_detect(text, "(?i)Disclosure statement"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)Disclosure statement)"), text)
  text <- ifelse(str_detect(text, "(?i)Notes on contributor"), 
                 str_extract(text,pattern = "(?s).*(?=(?i)Notes on contributor)"), text)
  text <- ifelse(str_detect(text, "(?i)Declaration of Conflicting Interests "), 
                 str_extract(text,pattern = "(?s).*(?=(?i)Declaration of Conflicting Interests )"), text)
  text <- text %>% 
    # replace \n with space
    stri_replace_all_regex("\n", " ")
  text <- str_replace_all(text, "\\s+", " ")
  text = str_replace_all(text, "[\r\n]", " ")
  if(mode=="wiley") {
    text <- text %>% str_replace_all("Downloaded from[^.]*\\.", "")
  }
  if(mode=="mdpi"){
    text <- ifelse(str_detect(text, "(?i)Author contributions"), 
                   str_extract(text,pattern = "(?s).*(?=(?i)Author contributions)"), text)
  }
  return(text)
}

mdpi_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  links <- page %>% html_nodes("a") %>% html_attr("href")
  pdf_link <- links[str_detect(links, "pdf\\?vers")] 
  pdf_link <- pdf_link[!is.na(pdf_link)]
  link <- ifelse(length(pdf_link==1), paste("https://www.mdpi.com", pdf_link, sep=""), NA)
  return(link)
}


cup_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  link <- page %>% html_nodes("a") %>% html_attr("href") %>% str_subset("pdf") %>% .[1]
  link <- paste("https://www.cambridge.org", link, sep="")
  return(link)
}

ari_get_link_to_pdf <- function(doi) {
  link <- paste("https://dx.doi.org/", doi, sep="")
  page <- read_html(link)
  link <- page %>% html_nodes("a") %>% html_attr("href") %>% str_subset("galley") %>% .[2]
  link <- paste("https://journals.librarypublishing.arizona.edu", link, sep="")
  return(link)
}

generate_url_from_doi_per_publisher <- function(DOI, publisher) {
  url_for_download <- NA_character_
  if (publisher == "rout") { url_for_download <-  paste("https://www.tandfonline.com/doi/pdf/", DOI, "?download=true", sep="")}
  if (publisher == "mdpi") { url_for_download <- NA_character_ } 
  if (publisher == "wiley") { url_for_download <- paste("https://onlinelibrary.wiley.com/doi/pdfdirect/", DOI, "?download=true", sep="") }
  if (publisher == "sage") {url_for_download <-  paste("https://journals.sagepub.com/doi/pdf/", DOI, "?download=true", sep="")}
  if (publisher == "springer") {url_for_download <- paste("https://link.springer.com/content/pdf/", DOI, ".pdf", sep="")}
  if (publisher == "nature") { end_of_DOI <-  str_extract(DOI, "\\/(.*)")
  url_for_download = paste("https://www.nature.com/articles", end_of_DOI, ".pdf", sep="")}
  if (publisher == "taylor") {url_for_download <- paste("https://www.tandfonline.com/doi/pdf/", DOI, "?download=true", sep="")}
  if (publisher == "camb") {url_for_download = cup_get_link_to_pdf(DOI)}
  if (publisher == "emerald") {url_for_download <- paste("https://www.emerald.com/insight/content/doi/", DOI, "/full/pdf", sep="")}
  if (publisher== "front") {url_for_download <- paste("https://www.frontiersin.org/articles/", DOI, "/pdf", sep="")}
  if (publisher == "arizona") { url_for_download <- ari_get_link_to_pdf(DOI)}
  return(url_for_download)
}

extract_top_terms <- function(model, n = 10) {
  tidy_model <- tidy(model)
  top_terms <- tidy_model %>%
    arrange(desc(beta)) %>%
    slice(1:n) %>%
    pull(term)
  
  return(top_terms)
}

compare_two_levels <- function(df, level1, level2) {
  x <- df %>% filter(model_id == level1)
  y <- df %>% filter(model_id == level2)
  simi_frames <- tidyr::expand_grid(level_first = level1, value_first = x$top_terms_list, level_second = level2, value_second = y$top_terms_list)
  simi_frame <- simi_frame %>% mutate(level_first = as.factor(level_first), level_second = as.factor(level_second)) %>%
    left_join(df %>% select(model_id, terms_as_list), by=c("level_first"="grouping", "value_first"="value")) %>% 
    left_join(df %>% select(model_if, value, terms_as_list) %>% rename(terms_second = terms_as_list), by=c("level_second"="grouping", "value_second"="value")) %>% 
    mutate(similar_words = map2(terms_first, terms_second, function(terms_first, terms_second) sum(unlist(terms_first) %in% unlist(terms_second))) %>% unlist())
  simi_frame
}

compare_two_models <- function(expanded_many_models, model1, model2) {
  # Filter rows for each level
  x <- expanded_many_models %>% filter(model_id == model1) %>% select(topic_number, top_terms_list, top_terms_vector)
  y <- expanded_many_models %>% filter(model_id == model2) %>% select(topic_number, top_terms_list, top_terms_vector)
  
  # Create all combinations of topics between the two levels
  simi_frame <- tidyr::crossing(
    level1_data = x,
    level2_data = y
  ) %>% unnest() %>% rename(
    topic_first = topic_number,
    terms_in_list_first = top_terms_list,
    topic_second = topic_number1,
    terms_in_list_second = top_terms_list1,
    terms_in_character_first = top_terms_vector,
    terms_in_character_second = top_terms_vector1
  ) %>%
    mutate(
      level_first = model1,
      level_second = model2
    )
  
  # Calculate the similarity (overlap) for each combination
  simi_frame <- simi_frame %>%
    mutate(
      similar_words = map2(
        terms_in_list_first, terms_in_list_second, 
        ~ sum(unlist(.x) %in% unlist(.y))
      ) %>% unlist()
    )
  
  return(simi_frame)
}

extract_top_terms_per_topic <- function(model, n = 10, matrix="beta") {
  tidy_model <- tidy(model, matrix=matrix)
  K <- model$settings$dim$K  # Extracting the number of topics (K) from the model
  
  top_terms_per_topic <- tidy_model %>%
    group_by(topic) %>%
    {if("beta" %in% names(.)) arrange(., desc(beta)) else .}%>%
    slice_head(n = n) %>%
    summarise(
      K = first(K),  # Include K in the output
      topic_number = first(topic),  # Include topic number
      top_terms_list = list(term),
      top_terms_vector = paste(term, collapse = ", ")
    )
  
  return(top_terms_per_topic)
}

similarity_results <- function(models_to_compare) {
  
  similarity_results <- models_to_compare %>%
    mutate(
      comparison_frames = map2(level_first, level_second, ~compare_two_models(expanded_many_models, .x, .y))
    ) %>% select(-level_first, -level_second) %>%
    unnest(comparison_frames) %>%
    group_by(level_first, topic_first) %>%
    summarise(
      level_second = max(level_second), 
      most_similar_second= topic_second[which.max(similar_words)],
      most_similar_value = similar_words[which.max(similar_words)],
      terms_first = first(terms_in_character_first),
      terms_second = terms_in_character_second[which.max(similar_words)]
    ) 
  return(similarity_results)
}

# ---- Data building ----
# The code in this section searches the articles for inclusion in the data and builds the data file


# Search term for Web of Science
# https://www.webofscience.com/wos/woscc/summary/684b3348-bf92-4f22-9c70-5817df20de37-714c509e/relevance/1
# (ALL=(postgrowth)) NOT WC=(Physics Applied) 
# NOT WC=(Materials Science, Multidisciplinary) NOT WC=(Physics, Condensed Matter) NOT WC=(Optics) 
# NOT WC=(Engineering Electrical Electronic) NOT WC=(Physics Multidisciplinary) NOT WC=(Electrochemistry) 
# NOT WC=(Chemistry Multidisciplinary) NOT WC=(Crystallography)
# Repeat for degrowth, de-growth, post-growth



# output is limited to 1000 per file and R reads the csv slightly differently depending on the contents, so fix that to combine
columns_that_are_read_in_different_formats  <- c("Volume", "DOI Link", "Web of Science Record", "Issue", "Start Page", "Part Number", "End Page", "Article Number")

degrowth_results <- read_excel("data/degrowth_results_1_1000.xls") %>% select(-(columns_that_are_read_in_different_formats)) %>% 
  bind_rows(read_excel("data/degrowth_results_1001_end.xls") %>% select(-(columns_that_are_read_in_different_formats))) %>% mutate(Search_term = "Degrowth")
de_growth_all <- read_excel("data/de_growth_all.xls") %>% select(-(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "De-growth")


postgrowth_all <- read_excel("data/postgrowth_all.xls") %>% select(-(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "Postgrowth")
post_growth_all <- read_excel("data/post_growth_all.xls") %>% select(-(columns_that_are_read_in_different_formats)) %>% mutate(Search_term = "Post-growth")

# do papers appear in multiple searchers
duplicated_in_multiple <- bind_rows(degrowth_results, de_growth_all, postgrowth_all, post_growth_all) %>% 
  group_by(DOI) %>% 
  summarize(N = n()) %>% filter(N > 1)

# full data

full_data <- degrowth_results  %>% 
  bind_rows(de_growth_all) %>% 
  bind_rows(postgrowth_all) %>%
  bind_rows(post_growth_all) %>%
  select(Search_term, "Article Title", "Source Title", "Document Type", "Author Keywords", "Abstract", "Publication Year", "DOI", "WoS Categories") %>%
  mutate(Search_term = ifelse(DOI %in% duplicated_in_multiple$DOI, "Multiple_searches", Search_term))



scopus <- read_csv("data_collection_files/scopus.csv")

journals_scopus <- scopus %>% group_by(`Source title`) %>% summarize(N=n()) %>% filter(N>1) %>% arrange(desc(N))


scopus_final_search_08032023 <- read_csv("data/scopus_final_search_08032023.csv") %>% select(Authors, Title, `Source title`, DOI, Publisher, Year, Abstract) %>% mutate(Scopus = "Yes")%>% filter(!is.na(DOI))

columns_that_are_read_in_different_formats  <- c("Volume", "DOI Link", "Web of Science Record", "Issue", "Start Page", "Part Number", "End Page", "Article Number")

dg_final_search_wos_08032023_first <- read_excel("data/dg_final_search_wos_08032023_first.xls") %>% select(-columns_that_are_read_in_different_formats)
dg_final_search_wos_08032023_last <- read_excel("data/dg_final_search_wos_08032023_last.xls") %>% select(-columns_that_are_read_in_different_formats)

wos_data <- dg_final_search_wos_08032023_first %>% bind_rows(dg_final_search_wos_08032023_last) %>% select(Authors, Title=`Article Title`, `Source Title`, DOI, Year=`Publication Year`, Abstract, Publisher) %>% 
  mutate(WOS = "Yes") %>% filter(!is.na(DOI))

all_search_results <- wos_data %>% full_join(scopus_final_search_08032023, by=c("DOI"))

# manual fixes
all_search_results$DOI[514] <- NA
all_search_results$DOI[1032] <- "10.19246/DOCUGEO2281-7549/202102_24"


# Full texts were downloaded based on this file
# See paper for statement for details on how texts were obtained using institutional access.

all_data_with_full_text <- read_rds("data/all_data_with_full_text11072023.RDS")

all_data_no_full_text <- all_data_with_full_text %>% select(-full_text)

corrections <- read_xlsx("data/all_data_no_full_text12072023_manual_checks.xlsx")

all_data_with_full_text_after_manual_check <- corrections %>% left_join(all_data_with_full_text %>% select(DOI, full_text))

deletions <- read_delim("data/all_data_manually_checked_no_full_text_05072023.csv", 
                        delim = ";", escape_double = FALSE, trim_ws = TRUE) %>% 
  select(DOI, delete) %>% filter(delete=="x")

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% anti_join(deletions)

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>%
  filter(!duplicated(DOI))

# further corrections

ee_formatting_correction <- all_data_with_full_text_after_manual_check %>% filter(Journal=="ECOLOGICAL ECONOMICS") %>%
  mutate(end_of_DOI = DOI %>% str_extract("(?<=/).*"))

ee_formatting_corrected <- ee_formatting_correction %>% 
  mutate(text_bits = str_split(full_text, "(?i)[[:space:]]references"),
         how_many_bits = lengths(text_bits),
         last_bit = map_chr(text_bits, ~ tail(.x, 1)),
         text_fixed = ifelse(how_many_bits > 1, str_replace(full_text, fixed(last_bit), ""), full_text)) %>% 
  select(DOI, text_fixed)

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% 
  left_join(ee_formatting_corrected) %>% 
  mutate(full_text = ifelse(!is.na(text_fixed), text_fixed, full_text))


# remove untopical papers discovered 
first_model_top_papers_per_topic_13072023_remove_a_few <- readxl::read_excel("results/first_model_top_papers_per_topic_13072023_remove_a_few.xlsx") %>%
  select(document, delete) %>% filter(delete=="x")

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% anti_join(first_model_top_papers_per_topic_13072023_remove_a_few, by=c("DOI"="document"))

# manual fixes after corpus review

tarkastukset <- read_rds("data/manually_checking_all_texts_still_checked_13122023.RDS") %>% select(DOI, action)
tarkastukset <- tarkastukset[!duplicated(tarkastukset$DOI),] 

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% left_join(tarkastukset)

# remove_from_data 

all_data_with_full_text_after_manual_check <- all_data_with_full_text_after_manual_check %>% filter(action != "remove_from_data" | is.na(action))

# check PDF reading manually and fix issues

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


# ---- Data cleaning ----
# This section cleans the data extracted from PDF files for analysis

# full_text is the original full text
all_data_corpus <- corpus(all_data_with_full_text_after_manual_check %>% select(DOI, Year, Journal, Title, Authors, full_text), text_field="full_text")

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

# Generate descriptive table of most common terms

dfm <- read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")


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

# Make table of most common journals

dfm <- read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")


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

# Make data preprocessing figure 


fig1 <- prisma(found = 1000,
               found_other = 1000,
               no_dupes = 1787, 
               screened =1771, 
               screen_exclusions = 597, 
               full_text = 1174,
               full_text_exclusions = 17, 
               qualitative = 979,
               quantitative = NULL,
               width = 800, height = 800, 
               labels = list(found = "Web of Science, n = 1533", found_other = "Scopus, n = 698",
                             screened = "Title and publication type and \n platform screened, n = 1771",
                             screen_exclusions = "Removed books, editorials, book reviews, \n and publications not related to \n economic degrowth based on title, n = 597",
                             full_text_exclusions = "Removed based on abstract and \n full-text review, n = 230", 
                             qualitative = "Included in data set, n = 943",
                             quantitative = "  Corpus of 943 documents and 11,008 terms"))
# 
# svg <- export_svg(fig1)
# rsvg::rsvg_pdf(charToRaw(svg), "supplements_for_submission/figure1_flowchart_for_data_management.pdf")
# 
# prisma_export(fig1, "supplements_for_submission/figure1_flowchart_for_data_management.png")

fig1 %>%
  DiagrammeRsvg::export_svg() %>%
  charToRaw() %>%
  rsvg::rsvg_svg(file = "supplements_for_submission/figure1_flowchart_for_data_management.svg")

rsvg::rsvg_pdf("supplements_for_submission/figure1_flowchart_for_data_management.svg", "supplements_for_submission/figure1_flowchart_for_data_management.pdf",  width = 595 , height = 842 )

# ---- Analysis ----
# This section runs the topic model and builds the relevant tables and supplementary tables 



dfm_all_data <- read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")

stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format

many_models <- tibble(K = c(10:25)) %>%
  mutate(topic_model = map(K, ~stm(documents = stm_all_data$documents, 
                                   vocab = stm_all_data$vocab, K = ., 
                                   prevalence = ~s(Year), 
                                   init.type = "Spectral", data=stm_all_data$meta,
                                   verbose = T)))

write_rds(many_models, "results/degrowth_models_with_varying_K_02022025.RDS")

# save needed data for making single paper plots 

dfm_all_data <- read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")

model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

first_model <- model %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]



topic_labels <- readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels, topic_type_for_presentations) 

td_gamma <- td_gamma %>% left_join(topic_labels)

write_csv(td_gamma, "supplements_for_submission/supplement_B3_all_papers_with_share_from_each_topic_and_metadata.csv")

# Build data frame with most common authors, journals, and most typical papers



# load model
model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)
dfm_all_data <-  read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")
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


td_gamma <- tidy(first_model, matrix = "gamma",
                 document_names = stm_all_data$meta$Title)

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

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  left_join(td_frex_top10_labels) %>% 
  mutate(Frex_label = reorder(Frex_label, gamma), 
         term = reorder(term, gamma)) %>%
  mutate(new_label = paste0("**Most probable: ", terms, "**<br>*Highest FREX: ", term, "*"))

top_words <- top_words%>%
  left_join(gamma_terms %>% select(topic, gamma) %>% mutate(topic = as.numeric(topic))) 

top_words <- top_words %>% arrange(desc(gamma))

writexl::write_xlsx(top_words, "supplements_for_submission/supplement_B1_topics_with_most_common_journals_authors_and_most_representative_papers.xlsx")

# Build figure 2 for paper

dfm_all_data <- read_rds("data/dfm_data_withoutstopwords_final_28122023.RDS")

stm_all_data <- quanteda::convert(dfm_all_data, to = "stm")  # convert to stm format

model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

first_model <- model %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

td_beta <- tidy(first_model, matrix = "beta") 


top_terms <- td_beta %>%
  arrange(beta) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  arrange(-beta) %>%
  select(topic, term) %>%
  summarise(terms = list(term)) %>%
  mutate(terms = map(terms, paste, collapse = ", ")) %>% 
  unnest(cols=c(terms))
top_terms

topic_labels <- readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels, topic_type_for_presentations) 

top_terms <- top_terms %>% left_join(topic_labels)

td_gamma <- tidy(first_model, matrix = "gamma",
                 document_names = stm_all_data$meta$Title) %>% 
  mutate(doc_id = rep(seq_len(nrow(stm_all_data$meta)), 20))    %>% 
  left_join(stm_all_data$meta %>% mutate(doc_id = row_number()), by=c("doc_id"))
td_frex_top10 <- tidy(first_model, matrix="frex") %>% group_by(topic) %>% slice(1:10)
td_frex_top10_labels <- td_frex_top10 %>%
  group_by(topic) %>%
  summarize(term = paste(term, collapse = ", "),
            .groups = "drop") %>%
  mutate(Frex_label = paste0("Topic ", topic, ": ", term))

gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_terms, by = "topic") %>%
  left_join(td_frex_top10_labels) %>% 
  mutate(Frex_label = reorder(Frex_label, gamma), 
         term = reorder(term, gamma)) %>%
  mutate(new_label = paste0("**Most probable: ", terms, "**<br>*Highest FREX: ", term, "*"))


# paper version

main_topic_figure_for_paper <- gamma_terms %>%
  ggplot(aes(term, gamma, label = new_label, fill = topic_type_for_presentations)) +
  geom_col() +  # Enable the legend by removing show.legend = FALSE
  ggtext::geom_richtext(hjust = 0, nudge_y = 0.0005, size = 3, fill = NA, label.color = NA) +
  geom_text(aes(label = labels, y = 0.001), hjust = 0, size = 4) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.18),
    labels = scales::label_percent()
  ) + 
  theme_minimal(base_size = 11) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.92, 0.18),  # Adjust the legend position
    legend.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_text(size = 12),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12)
  ) +
  labs(
    x = "",
    y = "Percent of corpus from topic",
    fill = "Topic type", 
  )

main_topic_figure_for_paper

ggsave("supplements_for_submission/figure2_topic_model_with_943_papers_and_20_topics.pdf", plot = main_topic_figure_for_paper, device = cairo_pdf, width = 7, height = 5, dpi = 300)


ggsave("supplements_for_submission/figure2_topic_model_with_943_papers_and_20_topics.png", plot = main_topic_figure_for_paper, width = 9.69, height = 6.27, units = "in", dpi=300)

ggsave("supplements_for_submission/figure2_topic_model_A4_landscape_600dpi.png",
       plot = main_topic_figure_for_paper,
       device = ragg::agg_png, width = 11.69, height = 8.27, units = "in", dpi = 600)


bw_topic_figure_for_paper <- gamma_terms %>%
  ggplot(aes(term, gamma, label = new_label, fill = topic_type_for_presentations)) +
  geom_col_pattern(
    color = "black",
    pattern = c("stripe", "crosshatch", "none")  # Different patterns for print clarity
  ) +
  ggtext::geom_richtext(hjust = 0, nudge_y = 0.0005, size = 3, fill = NA, label.color = NA) +
  geom_text(aes(label = labels, y = 0.001), hjust = 0, size = 4, color = "black") +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.18),
    labels = scales::percent_format()
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.75, 0.15),
    legend.background = element_rect(fill = "white", color = NA)
  ) +
  labs(
    x = "",
    y = "Percent of corpus from topic",
    fill = "Topic type"
  )



bw_topic_figure_for_paper

# Explore relationships between topics 


# Function for making a single paper plot with all topics (font sizes optimized for pdf saving, increase font size for screen viewing)

td_gamma <- read.csv("supplements_for_submission/supplement_B3_all_papers_with_share_from_each_topic_and_meta.csv")

topic_bar_plot <- function(data, document_id, label_id=document_id, labels_var = "labels") {
  
  document_data <- data %>% 
    filter(DOI == document_id)
  
  plot <- ggplot(document_data, aes(x = reorder(!!sym(labels_var), gamma), y = gamma)) +
    geom_bar(stat = "identity") +
    coord_flip() +  # To make horizontal bars
    labs(
      title = paste("Topic Distribution for", label_id),
      x = "Topic",
      y = "Proportion of words from topic"
    ) +
    theme_minimal(
      
    ) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(size = 2.5*2.8),
      axis.text = element_text(size = 2.5*2.8), 
      axis.title = element_text(size = 2.5*2.8) 
    ) + 
    geom_text(aes(label = round(gamma, 2)), hjust = -0.1, size = 2.5)  
  
  return(plot)
}

# Make examples for main paper 

a <- topic_bar_plot(td_gamma, "10.1016/j.ecolecon.2010.06.020", label_id = "Levallois, C. (2010) Can de-growth be considered a policy option? \nA historical note on Nicholas Georgescu-Roegen and the Club of Rome")

ggsave("supplements_for_submission/figure4_example_of_one_topic_paper.pdf", plot = a, device = cairo_pdf, width = 7, height = 5, dpi = 300)

ggsave("supplements_for_submission/figure4_example_of_one_topic_paper.svg", plot = a, width = 7, height = 5, dpi = 300)

b <- topic_bar_plot(td_gamma, "10.1016/j.ecolecon.2020.106653", label_id = "Mair, S; Druckman, A; Jackson, T. (2020) A tale of two utopias: Work in a post-growth world")

ggsave("supplements_for_submission/figure5_example_of_multi_paper.pdf", plot = b, device = cairo_pdf, width = 7, height = 5, dpi = 300)
ggsave("supplements_for_submission/figure5_example_of_multi_paper.svg", plot = b, width = 7, height = 5, dpi = 300)


# Function for finding overlap in two topics
# convenience function filters papers such that topic_1 and topic_2 contribute at least min_share to the vocabulary of the paper 

topic_combination_finder <- function(data, topic_1, topic_2, min_share) {
  # Filter the data frame based on the given topics and minimum gamma share
  filtered_data <- data %>%
    group_by(document) %>%
    filter(any(topic == topic_1 & gamma >= min_share) & 
             any(topic == topic_2 & gamma >= min_share)) %>%
    ungroup() %>%
    filter(topic %in% c(topic_1, topic_2)) %>%
    arrange(document)
  return(filtered_data)
}

# let's see the overlap of critique of capitalism and energy, for example
papers_drawing_from_topics_8_and_13 <- topic_combination_finder(td_gamma, 8, 13, min_share=0.1)

# Loop over all combinations of topics from 1 to 20
combo_list <- list()

for (i in 1:19) {
  for (j in (i + 1):20) {
    # Generate the filtered data frame for each combination
    combo_df <- topic_combination_finder(td_gamma, i, j, min_share = 0.1)
    
    # Create a name for the data frame in the list
    combo_name <- paste0("Combo_topic_", i, "_", j)
    
    # Store the data frame in the list with the generated name
    combo_list[[combo_name]] <- combo_df
  }
}

write_rds(combo_list, "results/papers_combining_two_topics.rds")
writexl::write_xlsx(combo_list, "supplements_for_submission/supplement_B4_articles_combining_each_topic_pair_one_sheet.xlsx")

td_gamma_75_share <- td_gamma %>% filter(gamma > 0.75)

writexl::write_xlsx(td_gamma_75_share, "supplements_for_submission/supplement_B5_articles_with_75_percent_from_single_topic.xlsx")

# Make figure 5, relationships between all topic pairs


# Get the lengths of each data frame in combo_list
# combo_list is generated by code in supplement B3 code 12
combo_lengths <- map_int(combo_list, nrow)

print(combo_lengths)

topic_matrix <- matrix(NA, nrow = 20, ncol = 20)

for (name in names(combo_lengths)) {
  topics <- as.numeric(unlist(strsplit(gsub("Combo_topic_", "", name), "_")))
  topic_matrix[topics[1], topics[2]] <- combo_lengths[[name]]
  topic_matrix[topics[2], topics[1]] <- combo_lengths[[name]]  # Ensure symmetry
}

topic_labels <- td_gamma %>% group_by(topic) %>% slice(1) %>% select(labels)

rownames(topic_matrix) <- topic_labels$labels
colnames(topic_matrix) <- topic_labels$labels

heatmap_data <- melt(topic_matrix, varnames = c("Topic1", "Topic2"), value.name = "Length")


upper_triangle_data <- heatmap_data %>%
  filter(as.numeric(factor(Topic1)) < as.numeric(factor(Topic2))) %>% 
  mutate(Number_of_papers = Length/2)

# Create the heatmap with only the upper triangle
heatmap <- ggplot(upper_triangle_data, aes(x = Topic1, y = Topic2, fill = Length)) +
  geom_tile() +
  # Adjust the gradient to make the highest values less dark (medium gray)
  scale_fill_gradient(low = "white", high = "gray30", na.value = "white") +
  geom_text(aes(label = Number_of_papers), color = "black", na.rm = TRUE, size = 2) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
    panel.grid = element_blank(),
    plot.title = element_text(size = 2.5*2.8),
    axis.text = element_text(size = 2.5*2.8), 
    axis.title = element_text(size = 2.5*2.8),
    legend.position = "none") +
  labs(x = "Topic", y = "Topic", caption = "Number of papers using both topics \n(at least 10% of vocabulary from both topics )")

ggsave("supplements_for_submission/figure6_heatmap_of_topic_combinations.pdf", plot = heatmap, device = cairo_pdf, width = 7, height = 5, dpi = 300)

ggsave("supplements_for_submission/figure6_heatmap_of_topic_combinations.pdf", plot = heatmap, device = cairo_pdf, width = 7, height = 5, dpi = 300)
ggsave(plot=heatmap, "supplements_for_submission/figure5_topic_pairs_heatmap.svg", width = 7, height = 5)


# Make topic network figure



model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

first_model <- model %>% 
  pull(topic_model) %>% 
  .[[1]]

stm_corrs <- get_network(model = first_model,
                         method = 'simple',
                         labels = paste('Topic', 1:20),
                         cutoff = 0.01)


topic_names  <- readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels) %>% arrange(topic)

V(stm_corrs)$name <- topic_names$labels
# plot network
ggraph(stm_corrs, layout = 'fr') +
  geom_edge_link(
    aes(edge_width = weight)) +
  geom_node_point(size = 1, colour = 'black')  +
  geom_node_label(
    aes(label = name, size = props), repel = TRUE, alpha = 0.85, box.padding=1, point.padding=1, nudge_x=0.05, nudge_y=0.05) +
  scale_size(range = c(4,6), labels = scales::percent) +
  labs(size = 'Topic Proportion',  edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()




extrafont:: loadfonts(device = "win") 

# Generate the network plot
# Compute the layout before plotting
layout <- create_layout(stm_corrs, layout = 'fr')

plot_network <- ggraph(layout) +
  # Grayscale edges
  geom_edge_link(aes(edge_width = weight), color = "gray60") + 
  
  # Nodes
  geom_node_point(size = 1, colour = 'black') +
  
  # Boxed text labels
  geom_label_repel(aes(x = x, y = y, label = name), 
                   size = 3, 
                   box.padding = 0.5, 
                   point.padding = 0.5, 
                   force = 2, 
                   max.overlaps = 100,
                   fill = "white",  # White background for labels
                   color = "black", # Black text
                   label.size = 0.25, # Thin black border
                   family = "Arial") +  
  
  # Scaling and labels
  scale_size(range = c(4, 6), labels = scales::percent) +
  labs(size = 'Topic Proportion', edge_width = 'Topic Correlation') +
  scale_edge_width(range = c(1, 3)) +
  theme_graph()

ggsave("supplements_for_submission/figure6_network_of_topics.png", plot_network, width = 8, height = 6, dpi = 300)


# Save the figure in proper sequence
ggsave("supplements_for_submission/figure6_network_of_topics.png", plot_network, width = 8, height = 6, dpi = 300)
ggsave("Figure_1.pdf", plot_network, width = 8, height = 6) # If PDF format is needed




data_frame_of_varying_K_models <- read_rds("results/degrowth_models_with_varying_K_02022025.RDS") 

many_models <- tibble(
  model_id = 1:16,
  stm_model = data_frame_of_varying_K_models
)


# Apply the function to each model and bind the results
expanded_many_models <- map_df(many_models$stm_model$topic_model, extract_top_terms_per_topic, .id = "model_id")

compare_two_levels <- function(expanded_many_models, level1, level2) {
  # Filter rows for each level and select necessary columns
  x <- expanded_many_models %>% 
    filter(K == level1) %>% 
    select(topic_number, top_terms_list, top_terms_vector)
  
  y <- expanded_many_models %>% 
    filter(K == level2) %>% 
    select(topic_number, top_terms_list, top_terms_vector)
  
  # Rename columns in y for the second level to avoid name conflicts
  y <- y %>%
    rename(
      topic_number2 = topic_number,
      top_terms_list2 = top_terms_list,
      top_terms_vector2 = top_terms_vector
    )
  
  # Ensure both levels have the expected columns
  if (!all(c("topic_number", "top_terms_list", "top_terms_vector") %in% names(x)) || 
      !all(c("topic_number2", "top_terms_list2", "top_terms_vector2") %in% names(y))) {
    stop("Required columns are missing in one of the levels.")
  }
  
  # Create all combinations of topics between the two levels
  simi_frame <- tidyr::crossing(
    level1_data = x,
    level2_data = y
  ) %>%
    unnest(level1_data, level2_data) %>%
    rename(
      topic_first = topic_number,
      terms_in_list_first = top_terms_list,
      terms_in_character_first = top_terms_vector,
      topic_second = topic_number2,
      terms_in_list_second = top_terms_list2,
      terms_in_character_second = top_terms_vector2
    ) %>%
    mutate(
      level_first = level1,
      level_second = level2
    )
  
  # Calculate the similarity (overlap) for each combination
  simi_frame <- simi_frame %>%
    mutate(
      similar_words = map2(
        terms_in_list_first, terms_in_list_second, 
        ~ sum(unlist(.x) %in% unlist(.y))
      ) %>% unlist()
    )
  
  return(simi_frame)
}

# Example call to the function:
# result <- compare_two_levels(expanded_many_models, 20, 19)

unique_K_values <- expanded_many_models$K %>% unique() %>% sort(decreasing = TRUE)

# Create pairs of K values for comparison
K_pairs <- tibble(
  level_first = unique_K_values[-length(unique_K_values)], 
  level_second = unique_K_values[-1]
)

# Apply compare_two_levels and find the most similar topic
similarity_results <- K_pairs %>%
  mutate(
    comparison_frames = map2(level_first, level_second, ~compare_two_levels(expanded_many_models, .x, .y))
  ) %>% select(-level_first, -level_second) %>%
  unnest(comparison_frames) %>%
  group_by(level_first, topic_first) %>%
  summarise(
    level_second = max(level_second), 
    most_similar_second= topic_second[which.max(similar_words)],
    most_similar_value = similar_words[which.max(similar_words)],
    terms_first = first(terms_in_character_first),
    terms_second = terms_in_character_second[which.max(similar_words)]
  ) 

writexl::write_xlsx(similarity_results, "results/expanded_grid_comparing_topic_split_from10_to_25.xlsx")

labels_for_K20 <- readxl::read_excel("results/analysis_table_from_dec2812023_models_20frex.xlsx") %>% select(topic, labels) %>%
  mutate(K = 20)

all_similarities_labeled <- similarity_results %>% left_join(labels_for_K20, by=c("level_first"="K", "topic_first"="topic"))

# Function to propagate labels downwards or upwards
propagate_labels <- function(data, current_level, direction = "down") {
  # Define the next level based on direction
  next_level <- if (direction == "down") current_level - 1 else current_level + 1
  
  # Filter rows for the current level and where most_similar_value > 6
  current_level_data <- data %>%
    filter(level_first == current_level, most_similar_value > 6)
  
  # Update labels for the next level based on most similar topics from the current level
  updated_data <- data %>%
    left_join(current_level_data, 
              by = c("level_first" = "level_second", 
                     "topic_first" = "most_similar_second"),
              suffix = c("", "_next")) %>%
    mutate(
      labels = if_else(
        level_first == next_level & is.na(labels) & most_similar_value_next > 6,
        labels_next,
        labels
      )
    ) %>%
    select(-ends_with("_next"))  # Remove joined columns
  
  return(updated_data)
}

# Recursively apply label propagation going downwards from level 20
for (lvl in 20:11) {
  all_similarities_labeled <- propagate_labels(all_similarities_labeled, lvl, direction = "down")
}

propagate_labels_upwards <- function(data, current_level) {
  # Define the previous level for upward propagation
  previous_level <- current_level - 1
  
  # Filter rows for the previous level with most_similar_value > 6
  previous_level_data <- data %>%
    filter(level_first == previous_level, most_similar_value > 6) %>%
    select(topic_first, labels) %>%
    rename(
      topic_second = topic_first,
      labels_previous = labels
    )
  
  # Join the previous level labels with the current level
  updated_data <- data %>%
    left_join(previous_level_data, 
              by = c("topic_first" = "topic_second")) %>%
    mutate(
      labels = if_else(
        level_first == current_level & is.na(labels) & most_similar_value >6,
        labels_previous,
        labels
      )
    ) %>%
    select(-labels_previous)  # Clean up temporary columns
  
  return(updated_data)
}

all_similarities_labeled <- ungroup(all_similarities_labeled)
# Recursively apply label propagation going upwards from level 20
for (lvl in 21:max(all_similarities_labeled$level_first)) {
  all_similarities_labeled <- propagate_labels_upwards(all_similarities_labeled, lvl)
}

writexl::write_xlsx(all_similarities_labeled, "supplements_for_submission/supplement_C1_topic_development_from_K10_to_K25.xlsx")

# plotting

topic_similarities_for_plot <- readxl::read_excel("supplements_for_submission/supplement_C1_topic_development_from_K10_to_K25.xlsx")
# https://www.displayr.com/sankey-diagrams-r/
library(networkD3)
topic_similarities_for_plot$id<- 0:(nrow(topic_similarities_for_plot)-1)

topic_similarities_for_plot <- topic_similarities_for_plot %>% 
  mutate(name = paste(grouping, value, sep=":"), 
         source_name = paste(level_first, value_first, sep=":"),
         plot_name = paste(name, manual_label))

## check if there is an input coming in for each
## mutate: final tie is from_below if there is an input, from_above if not

nodes <- topic_similarities_for_plot %>% select(id, labels)

links <- topic_similarities_for_plot %>%
  # Join the data frame to itself to find matching rows for the target
  inner_join(topic_similarities_for_plot, by = c("level_first" = "level_second", 
                        "topic_first" = "most_similar_second"),
             suffix = c("_source", "_target")) %>%
  # Select the required columns for the Sankey plot
  select(source = id_source, target = id_target, value = most_similar_value_source)



sankeyNetwork(Links = links, Nodes = nodes, NodeID = "labels",
              Source="source", Target="target", Value="value", fontSize = 12, sinksRight = F, iterations=0)

# analyse exclu and coherence


k_result <- many_models %>%
  mutate(exclusivity = map(stm_model, exclusivity),
         semantic_coherence = map(stm_model, semanticCoherence, stm_all_data$documents),
                 residual = map(stm_model, checkResiduals, stm_all_data$documents),
         bound =  map_dbl(stm_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(stm_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(stm_model, function(x) length(x$convergence$bound)))

k_result

k_result %>%
  transmute(K = model_id +10, 
            Exclusivity = map_dbl(exclusivity, mean),
            `Semantic coherence` = map_dbl(semantic_coherence, mean)
            ) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y")

model_avg <- k_result %>%
  transmute(K = model_id +9, 
            Exclusivity = map_dbl(exclusivity, mean),
            SemanticCoherence = map_dbl(semantic_coherence, mean),
            Type = "Average")  # Label the averages

# Individual topic data (flattened)
individual_topics <- k_result %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  transmute(K = model_id +9,
            Exclusivity = exclusivity,
            SemanticCoherence = semantic_coherence,
            Type = "Topic")  %>% 
  group_by(K) %>% mutate(Topic = row_number()) %>% ungroup()


individual_topics <- individual_topics %>% left_join(topic_similarities_for_plot %>% select(K = level_first, Topic = topic_first, labels))

# Combine both data sets
combined_data <- bind_rows(model_avg, individual_topics)

ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity, color = factor(K))) +
  # Scatter points for individual topics
  geom_point(data = filter(combined_data, Type == "Topic"), 
             aes(shape = Type), size = 1, alpha = 0.3, show.legend = FALSE) +
  # Larger scatter points for model averages
  geom_point(data = filter(combined_data, Type == "Average"), 
             aes(shape = Type), size = 3, alpha = 0.8, show.legend = TRUE) +
  # Adding labels for individual topics (using text from 'labels' column)
  geom_text(data = filter(combined_data, Type == "Topic"), 
            aes(label = labels), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.5, show.legend = FALSE) +
  # Adding labels for model averages (using factor(K))
  geom_text(data = filter(combined_data, Type == "Average"), 
            aes(label = factor(K)), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.9, show.legend = FALSE) +
  # Customize color palette and shapes
  scale_color_manual(values = scales::hue_pal()(length(unique(combined_data$K)))) +
  labs(title = "Scatterplot of Exclusivity and Semantic Coherence by Model") +
  theme_minimal()





data_frame_of_varying_K_models <- read_rds("results/degrowth_models_with_varying_K_02022025.RDS") 

many_models <- tibble(
  model_id = 1:16,
  stm_model = data_frame_of_varying_K_models
)


# Apply the function to each model and bind the results
expanded_many_models <- map_df(many_models$stm_model$topic_model, extract_top_terms_per_topic, .id = "model_id")

compare_two_levels <- function(expanded_many_models, level1, level2) {
  # Filter rows for each level and select necessary columns
  x <- expanded_many_models %>% 
    filter(K == level1) %>% 
    select(topic_number, top_terms_list, top_terms_vector)
  
  y <- expanded_many_models %>% 
    filter(K == level2) %>% 
    select(topic_number, top_terms_list, top_terms_vector)
  
  # Rename columns in y for the second level to avoid name conflicts
  y <- y %>%
    rename(
      topic_number2 = topic_number,
      top_terms_list2 = top_terms_list,
      top_terms_vector2 = top_terms_vector
    )
  
  # Ensure both levels have the expected columns
  if (!all(c("topic_number", "top_terms_list", "top_terms_vector") %in% names(x)) || 
      !all(c("topic_number2", "top_terms_list2", "top_terms_vector2") %in% names(y))) {
    stop("Required columns are missing in one of the levels.")
  }
  
  # Create all combinations of topics between the two levels
  simi_frame <- tidyr::crossing(
    level1_data = x,
    level2_data = y
  ) %>%
    unnest(level1_data, level2_data) %>%
    rename(
      topic_first = topic_number,
      terms_in_list_first = top_terms_list,
      terms_in_character_first = top_terms_vector,
      topic_second = topic_number2,
      terms_in_list_second = top_terms_list2,
      terms_in_character_second = top_terms_vector2
    ) %>%
    mutate(
      level_first = level1,
      level_second = level2
    )
  
  # Calculate the similarity (overlap) for each combination
  simi_frame <- simi_frame %>%
    mutate(
      similar_words = map2(
        terms_in_list_first, terms_in_list_second, 
        ~ sum(unlist(.x) %in% unlist(.y))
      ) %>% unlist()
    )
  
  return(simi_frame)
}

# Example call to the function:
# result <- compare_two_levels(expanded_many_models, 20, 19)

unique_K_values <- expanded_many_models$K %>% unique() %>% sort(decreasing = TRUE)

# Create pairs of K values for comparison
K_pairs <- tibble(
  level_first = unique_K_values[-length(unique_K_values)], 
  level_second = unique_K_values[-1]
)

# Apply compare_two_levels and find the most similar topic
similarity_results <- K_pairs %>%
  mutate(
    comparison_frames = map2(level_first, level_second, ~compare_two_levels(expanded_many_models, .x, .y))
  ) %>% select(-level_first, -level_second) %>%
  unnest(comparison_frames) %>%
  group_by(level_first, topic_first) %>%
  summarise(
    level_second = max(level_second), 
    most_similar_second= topic_second[which.max(similar_words)],
    most_similar_value = similar_words[which.max(similar_words)],
    terms_first = first(terms_in_character_first),
    terms_second = terms_in_character_second[which.max(similar_words)]
  ) 

writexl::write_xlsx(similarity_results, "results/expanded_grid_comparing_topic_split_from10_to_25.xlsx")

labels_for_K20 <- readxl::read_excel("results/analysis_table_from_dec2812023_models_20frex.xlsx") %>% select(topic, labels) %>%
  mutate(K = 20)

all_similarities_labeled <- similarity_results %>% left_join(labels_for_K20, by=c("level_first"="K", "topic_first"="topic"))

# Function to propagate labels downwards or upwards
propagate_labels <- function(data, current_level, direction = "down") {
  # Define the next level based on direction
  next_level <- if (direction == "down") current_level - 1 else current_level + 1
  
  # Filter rows for the current level and where most_similar_value > 6
  current_level_data <- data %>%
    filter(level_first == current_level, most_similar_value > 6)
  
  # Update labels for the next level based on most similar topics from the current level
  updated_data <- data %>%
    left_join(current_level_data, 
              by = c("level_first" = "level_second", 
                     "topic_first" = "most_similar_second"),
              suffix = c("", "_next")) %>%
    mutate(
      labels = if_else(
        level_first == next_level & is.na(labels) & most_similar_value_next > 6,
        labels_next,
        labels
      )
    ) %>%
    select(-ends_with("_next"))  # Remove joined columns
  
  return(updated_data)
}

# Recursively apply label propagation going downwards from level 20
for (lvl in 20:11) {
  all_similarities_labeled <- propagate_labels(all_similarities_labeled, lvl, direction = "down")
}

propagate_labels_upwards <- function(data, current_level) {
  # Define the previous level for upward propagation
  previous_level <- current_level - 1
  
  # Filter rows for the previous level with most_similar_value > 6
  previous_level_data <- data %>%
    filter(level_first == previous_level, most_similar_value > 6) %>%
    select(topic_first, labels) %>%
    rename(
      topic_second = topic_first,
      labels_previous = labels
    )
  
  # Join the previous level labels with the current level
  updated_data <- data %>%
    left_join(previous_level_data, 
              by = c("topic_first" = "topic_second")) %>%
    mutate(
      labels = if_else(
        level_first == current_level & is.na(labels) & most_similar_value >6,
        labels_previous,
        labels
      )
    ) %>%
    select(-labels_previous)  # Clean up temporary columns
  
  return(updated_data)
}

all_similarities_labeled <- ungroup(all_similarities_labeled)
# Recursively apply label propagation going upwards from level 20
for (lvl in 21:max(all_similarities_labeled$level_first)) {
  all_similarities_labeled <- propagate_labels_upwards(all_similarities_labeled, lvl)
}

writexl::write_xlsx(all_similarities_labeled, "supplements_for_submission/supplement_C1_topic_development_from_K10_to_K25.xlsx")

# plotting

topic_similarities_for_plot <- readxl::read_excel("supplements_for_submission/supplement_C1_topic_development_from_K10_to_K25.xlsx")
# https://www.displayr.com/sankey-diagrams-r/
topic_similarities_for_plot$id<- 0:(nrow(topic_similarities_for_plot)-1)

topic_similarities_for_plot <- topic_similarities_for_plot %>% 
  mutate(name = paste(grouping, value, sep=":"), 
         source_name = paste(level_first, value_first, sep=":"),
         plot_name = paste(name, manual_label))

## check if there is an input coming in for each
## mutate: final tie is from_below if there is an input, from_above if not

nodes <- topic_similarities_for_plot %>% select(id, labels)

links <- topic_similarities_for_plot %>%
  # Join the data frame to itself to find matching rows for the target
  inner_join(topic_similarities_for_plot, by = c("level_first" = "level_second", 
                                                 "topic_first" = "most_similar_second"),
             suffix = c("_source", "_target")) %>%
  # Select the required columns for the Sankey plot
  select(source = id_source, target = id_target, value = most_similar_value_source)



sankeyNetwork(Links = links, Nodes = nodes, NodeID = "labels",
              Source="source", Target="target", Value="value", fontSize = 12, sinksRight = F, iterations=0)

# analyse exclu and coherence


k_result <- degrowth_models_with_varying_K__3_10002022025 %>% mutate(exclusivity = map(topic_model, exclusivity),
    exclusivity        = map(topic_model, stm::exclusivity),
    semantic_coherence = map(topic_model, ~ stm::semanticCoherence(.x, stm_all_data$documents)))


model_avg <- k_result %>%
  transmute(K=K, 
            Exclusivity = map_dbl(exclusivity, mean),
            SemanticCoherence = map_dbl(semantic_coherence, mean),
            Type = "Average")  # Label the averages

# Individual topic data (flattened)
individual_topics <- k_result %>%
  unnest(cols = c(exclusivity, semantic_coherence)) %>%
  transmute(K = K,
            Exclusivity = exclusivity,
            SemanticCoherence = semantic_coherence,
            Type = "Topic")  %>% 
  group_by(K) %>% mutate(Topic = row_number()) %>% ungroup()

# Combine both data sets
combined_data <- bind_rows(model_avg, individual_topics)

# Make figure 3

figure3 <- ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity)) +
  geom_point(
    data = filter(combined_data, Type == "Average"), 
    aes(shape = Type), size = 1, alpha = 0.3
  ) +
  geom_text(
    data = filter(combined_data, Type == "Average"), 
    aes(label = K),
    nudge_x = 0.05,  # move right
    nudge_y = 0.02,  # move up
    size = 3, alpha = 0.9
  ) +
  theme_minimal() +
  theme(legend.position = "none")

figure3

ggsave("supplements_for_submission/figure3_exclusivity_and_semantic_coherence.pdf", plot = figure3, device = cairo_pdf, width = 7, height = 5, dpi = 300)

ggsave("supplements_for_submission/figure3_exclusivity_and_semantic_coherence.svg", plot = figure3, width = 7, height = 5, dpi = 300)

# Make supplement figure 

ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity, color = factor(K))) +
  # Scatter points for individual topics
  geom_point(data = filter(combined_data, Type == "Topic"), 
             aes(shape = Type), size = 1, alpha = 0.3, show.legend = FALSE) +
  # Larger scatter points for model averages
  geom_point(data = filter(combined_data, Type == "Average"), 
             aes(shape = Type), size = 3, alpha = 0.8, show.legend = TRUE) +
  # Adding labels for individual topics (using text from 'labels' column)
  geom_text(data = filter(combined_data, Type == "Topic"), 
            aes(label = labels), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.5, show.legend = FALSE) +
  # Adding labels for model averages (using factor(K))
  geom_text(data = filter(combined_data, Type == "Average"), 
            aes(label = factor(K)), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.9, show.legend = FALSE) +
  # Customize color palette and shapes
  scale_color_manual(values = scales::hue_pal()(length(unique(combined_data$K)))) +
  labs(title = "Scatterplot of Exclusivity and Semantic Coherence by Model") +
  theme_minimal()




# ---- make comparisons to other reviews ---

# load paper df with most common topic

file_path <- "supplements_for_submission/supplement_B2_all_data_with_main_topic_share_from_main_topic.xlsx"

most_likely_topics <- excel_sheets(file_path) %>% 
  set_names() %>% 
  map_df(~ read_excel(file_path, sheet = .x), .id = "sheet_name")



# compare paper by paper to fitz

fitz_table_with_papers <- read_csv("extracted_results_from_other_reviews_for_comparisn/fitz_table_with_papers.csv")


# Define the fuzzy join function
fuzzy_left_join_custom <- function(df1, df2, title_dist = 0.2) {
  
  # Use fuzzyjoin's stringdist_left_join with custom distance for each variable
  fuzzy_joined <- stringdist_left_join(df1, df2, 
                                       by = c("Title" = "Title"),
                                       method = "jw",  # Jaro-Winkler distance method for strings
                                       max_dist = list(Title = title_dist),
                                       distance_col = NULL) %>%
    
    #  select(-ends_with(".y")) %>%
    #rename_all(~gsub("\\.x", "", .))  # Remove .x suffix
    
    return(fuzzy_joined)
}

prepare_comparison_data <- function(dataframe, variable1, variable2) {
  # Ensure the variable names are provided as strings
  variable1 <- as.character(variable1)
  variable2 <- as.character(variable2)
  
  # Step 1: Create a contingency table to count occurrences
  ct <- as.data.frame(table(dataframe[[variable1]], dataframe[[variable2]]), stringsAsFactors = FALSE)
  
  # Remove zero counts
  ct <- ct[ct$Freq > 0, ]
  
  # Rename columns for clarity
  names(ct)[1:2] <- c("from", "to")
  names(ct)[3] <- "weight"
  
  return(ct)
}

prepare_network_data <- function(graph_data, variable) {
  
  
  # Step 2: Create the graph
  g <- graph_from_data_frame(graph_data, directed = FALSE)
  g_tidy <- as_tbl_graph(g)
  
  # Step 3: Assign vertex types and attributes
  g_tidy <- g_tidy %>%
    mutate(type = ifelse(str_detect(name, variable), "Label", "Theme"),
           shape = ifelse(type == "Label", "circle", "square"))
  
  # Return the tidy graph object
  return(g_tidy)
}

# Function 2: Plot the network graph
plot_network_graph <- function(g_tidy, plot_title) {
  
  # Set seed for reproducibility
  set.seed(123)
  
  # Create the layout
  layout <- create_layout(g_tidy, layout = "fr",
                          weights = E(g_tidy)$weight)
  
  # Generate the plot
  plot <- ggraph(layout) +
    # Edges
    geom_edge_link(aes(width = weight), alpha = 0.6, color = "grey") +
    # Nodes
    geom_node_point(aes(fill = type, shape = type), size = 5, color = "black") +
    # Node labels with increased font size
    geom_node_text(aes(label = name),
                   size = 5,
                   repel = TRUE,
                   max.overlaps = Inf,
                   box.padding = unit(0.5, "lines"),
                   point.padding = unit(0.3, "lines")) +
    # Scales
    scale_edge_width(range = c(0.5, 2)) +
    scale_fill_manual(values = c("Label" = "lightblue", "Theme" = "salmon")) +
    scale_shape_manual(values = c("Label" = 21, "Theme" = 22)) +
    # Theme adjustments with border
    theme_void() +
    theme(legend.position = "none",
          plot.background = element_rect(color = "black", fill = NA, size = 1)) +
    labs(title = plot_title)
  
  # Return the plot
  return(plot)
}


# Example usage with two data frames (assuming df1 is most_likely_topics and df2 is fitz_table_with_papers)
fitz_comp <- fuzzy_left_join_custom(most_likely_topics, fitz_table_with_papers, title_dist = 0.1) %>% relocate(Title.y, .after=Title.x) %>% filter(!is.na(Theme))

sum(!is.na(fitz_comp$Title.y))/nrow(fitz_comp)
sum(!is.na(fitz_comp$Title.y))

# Optional: Save result to CSV
write.csv2(fitz_comp, "extracted_results_from_other_reviews_for_comparisn/fitzpatrick_all_themes_from_matched_docs.csv", row.names = FALSE)


fitz_comp_the_other_way <- fitz_table_with_papers %>% distinct(Title, .keep_all = T) %>% fuzzy_left_join_custom(., most_likely_topics, title_dist = 0.1) %>%
  relocate(Title.y, .after=Title.x) %>% filter(`Document Type`=="Article" & Language == "English")

sum(!is.na(fitz_comp_the_other_way$Title.y))/nrow(fitz_comp_the_other_way)
sum(!is.na(fitz_comp_the_other_way$Title.y))



fitz_comp$labels <- paste("Topic:", fitz_comp$labels)
fitz_comp$Theme <- paste("Theme:", fitz_comp$Theme)

fitz_comparison <- prepare_comparison_data(fitz_comp, "labels", "Theme") %>% prepare_network_data("^Theme:") %>% 
  plot_network_graph("Topics compared the policy themes from Fitzpatrick et al, 2022")

fitz_comparison

weiss_cattaneo_extracted_table <- read_csv("extracted_results_from_other_reviews_for_comparisn/weiss_cattaneo_extracted_table.csv")

# Example usage with two data frames (assuming df1 is most_likely_topics and df2 is weiss_table_with_papers)
weiss_comp <- fuzzy_left_join_custom(most_likely_topics, weiss_cattaneo_extracted_table, title_dist = 0.1) %>% relocate(Title.y, .after=Title.x) %>% filter(!is.na(Title.y))

# Optional: Save result to CSV
write.csv2(weiss_comp, "extracted_results_from_other_reviews_for_comparisn/weiss_all_themes_from_matched_docs.csv", row.names = FALSE)


weiss_comp_the_other_way <- weiss_cattaneo_extracted_table %>% distinct(Title, .keep_all = T) %>% fuzzy_left_join_custom(., most_likely_topics, title_dist = 0.1) %>%
  relocate(Title.y, .after=Title.x)

sum(!is.na(weiss_comp_the_other_way$Title.y))/nrow(weiss_comp_the_other_way)
sum(!is.na(weiss_comp_the_other_way$Title.y))

weiss_comp$labels <- paste("Topic:", weiss_comp$labels)
weiss_comp$Theme <- paste("MRT:", weiss_comp$`Main research topic related to degrowth`)
weiss_comparison <- prepare_comparison_data(weiss_comp, "labels", "Theme") %>% prepare_network_data("^MRT:") %>% 
  plot_network_graph("Topics compared the main research themes from Weiss & Cattaneo, 2022")

weiss_comparison

# compare paper by paper to engler

# manual fix of topic titles in Excel introduce non-standard strings - recode manually instead

engler_data <- read_csv("extracted_results_from_other_reviews_for_comparisn/engler_data.csv")
# 
# fuzzy_left_join_author_year <- function(df1, df2, author_dist = 0.2) {
#   fuzzy_joined <- fuzzy_left_join(
#     df1, df2,
#     by = c("Year" = "Year", "Authors" = "Author"),
#     match_fun = list(`==`, function(a, b) {
#       stringdist(a, b, method = "jw") <= author_dist
#     })
#   )
#   
#   fuzzy_joined <- fuzzy_joined %>%
#     select(-ends_with(".y")) %>%
#     rename_all(~ gsub("\\.x$", "", .))
#   
#   return(fuzzy_joined)
# }
# 
# 
# engler_labs <- data.frame(Cluster = c(1:7), Cluster_name = c("Conceptual foundations", 
#                                                              "Degrowth, culture and power",
#                                                              "Flavors of degrowth",
#                                                              "Quantitative modeling",
#                                                              "The limits to growth", 
#                                                              "Transition management",
#                                                              "Degrowth perception and communication"))
# 
# engler_data <- engler_data %>% left_join(engler_labs)
# 
# most_likely_topics$Authors <- enc2utf8(most_likely_topics$Authors)
# engler_data$Author <- enc2utf8(engler_data$Author)
#   
# engler_comp <- fuzzy_left_join_author_year(most_likely_topics, engler_data, author_dist = 0.2) %>% filter(!is.na(ID))
engler_comp$labels <- paste("Topic:", engler_comp$labels)
engler_comp$Cluster_name <- paste("Cluster:", engler_comp$Cluster_name)

# Optional: Save result to CSV
write.csv2(engler_comp, "extracted_results_from_other_reviews_for_comparisn/engler_all_themes_from_matched_docs.csv", row.names = FALSE)

engler_comp <- read_csv2("extracted_results_from_other_reviews_for_comparisn/engler_all_themes_from_matched_docs.csv") %>% 
  mutate(labels = case_when(labels == "Topic: (Survey) methods" ~ "Topic: Survey methods",
                            labels == "Topic: Climate politics and models" ~ "Topic: Climate policies and models",
                            .default = labels))

engler_comparison <- prepare_comparison_data(engler_comp, "labels", "Cluster_name") %>% prepare_network_data("^Cluster:") %>% 
  plot_network_graph( "Topics compared the policy themes from Engler et al, 2024")

engler_comparison

# load the main results table 

topics_table <- readxl::read_excel("results/analysis_table_from_dec2812023_models_20frex.xlsx")

# compare topic to topic to savin

savin_van_den_bergh_table2 <- read_csv("extracted_results_from_other_reviews_for_comparisn/savin_van_den_bergh_table2.csv")

# For topics_table
topics_table <- topics_table %>%
  mutate(
    FREX_words = str_split(FREX, pattern = ",\\s*")
  )

# For savin_van_den_bergh_table2
savin_van_den_bergh_table2 <- savin_van_den_bergh_table2 %>%
  mutate(
    MDT_words = str_split(`Most discriminating terms`, pattern = ",\\s*")
  )


combinations <- tidyr::crossing(
  topics_table %>% select(labels, FREX_words),
  savin_van_den_bergh_table2 %>% select(`Topic Label`, MDT_words)
)

combinations <- combinations %>%
  rowwise() %>%
  mutate(
    overlap_count = length(intersect(FREX_words, MDT_words))
  ) %>%
  ungroup()

result <- combinations %>%
  select(labels, `Topic Label`, overlap_count) %>%
  arrange(desc(overlap_count))


# Remove zero counts
result <- result[result$overlap_count > 0, ]

# Rename 'Freq' column to 'weight'
names(result)[3] <- "weight"

g <- graph_from_data_frame(result, directed = FALSE)
g_tidy <- as_tbl_graph(g)

# Update the type variable to match the scale levels
g_tidy <- g_tidy %>%
  mutate(type = ifelse(name %in% unique(savin_van_den_bergh_table2$`Topic Label`), "Theme", "Label"),
         shape = ifelse(type == "Theme", "square", "circle"))

set.seed(123)  # For reproducibility

layout <- create_layout(g_tidy, layout = "fr",
                        weights = E(g)$weight)

# Plot using ggraph with adjusted node size and consistent mappings
savin_van_den_bergh_comparison <- ggraph(layout) +
  # Edges
  geom_edge_link(aes(width = weight), alpha = 0.6, color = "grey") +
  # Nodes
  geom_node_point(aes(fill = type, shape = type), size = 5, color = "black") +
  # Node labels with increased font size
  geom_node_text(aes(label = name),
                 size = 5,
                 repel = TRUE,
                 max.overlaps = Inf,
                 box.padding = unit(0.5, "lines"),
                 point.padding = unit(0.3, "lines")) +
  # Scales
  scale_edge_width(range = c(0.5, 2)) +
  scale_fill_manual(values = c("Label" = "lightblue", "Theme" = "salmon")) +
  scale_shape_manual(values = c("Label" = 21, "Theme" = 22)) +
  # Theme adjustments
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = NA, size = 1))+ labs(title = "Topics compared to the topics from Savin & van den Bergh, 2024")

savin_van_den_bergh_comparison

ggsave(
  filename = "supplements_for_submission/supplement_D1_comparison_plots_to_other_reviews.pdf", 
  plot = gridExtra::marrangeGrob(list(fitz_comparison, 
                                      weiss_comparison,
                                      engler_comparison,
                                      savin_van_den_bergh_comparison), nrow=1, ncol=1), 
  width = 15, height = 9, dpi = 300
)

# comparison table

all_comparisons <- list(prepare_comparison_data(fitz_comp, "labels", "Theme") %>% 
                          rename(labels = from, fitz_themes = to) %>% group_by(labels) , 
                        prepare_comparison_data(weiss_comp, "labels", "Theme") %>% rename(labels = from, weiss_themes = to),
                        prepare_comparison_data(engler_comp, "labels", "Cluster_name") %>% rename(labels = from, engler_clusters = to),
                        result %>% mutate(labels = paste0("Topic: ", labels)) %>% rename(savin_topics = `Topic Label` )) 


dfs_filtered <- lapply(all_comparisons, function(df) {
  vars <- names(df)
  varying_var <- vars[!(vars %in% c('labels', 'weight'))]
  df %>%
    group_by(labels) %>%
    filter(weight == max(weight)) %>%
    summarise(across(all_of(varying_var), ~ paste(unique(.), collapse = ", ")), .groups = 'drop')
})


dfs_prepared <- lapply(dfs_filtered, function(df) {
  vars <- names(df)
  varying_var <- vars[!(vars %in% c('labels', 'weight'))]
  df %>% select(labels, all_of(varying_var))
})

final_result <- purrr::reduce(dfs_filtered, full_join, by = 'labels')

strings_to_remove <- c("Topic: ", "Theme: ", "MRT: ", "Cluster: ")

strip_strings <- function(x, patterns) {
  for (p in patterns) {
    x <- gsub(p, "", x, fixed = TRUE)
  }
  return(x)
}

final_result <- final_result %>%
  mutate(across(everything(), ~ strip_strings(., strings_to_remove)))

final_result <- final_result %>% left_join(readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(topic, labels, gamma)) %>% 
  arrange(desc(gamma))

final_result <- final_result%>% # select(-gamma) %>% 
  set_names(c("Labels", "Fitzpatrick et al. (2022) themes", "Weiss & Cattaneo (2017) themes", "Engler et al. (2024) clusters",
              "Savin & van den Bergh (2024) topics", "Gamma"))

final_result <- final_result %>% left_join(readxl::read_xlsx("results/analysis_table_from_dec2812023_models.xlsx") %>% select(Labels=labels, topic_type_for_presentations))

writexl::write_xlsx(final_result, "results/comparison_table_to_earlier_reviews.xlsx")


# compare topic by from 11 topic model of ours

data_frame_of_varying_K_models <- read_rds("results/degrowth_models_with_varying_K_02102024.RDS") 

just_K11 <- tibble(
  model_id = 1:16,
  stm_model = data_frame_of_varying_K_models
) %>% filter(model_id == 2)
# compare topic to topic to savin

extract_frext <- function(topic_model, n = 20) {
  tidy(topic_model, matrix = "frex") %>%
    group_by(topic) %>%
    slice_head(n = 20) %>%
    mutate(rank = row_number()) %>% summarize(top_terms_vector = paste(term, collapse = ", ")) %>%
    ungroup() 
  
}
just_11_K_expanded <- extract_frext(just_K11$stm_model[[1]])


savin_van_den_bergh_table2 <- read_csv("extracted_results_from_other_reviews_for_comparisn/savin_van_den_bergh_table2.csv")

# For topics_table
just_11_K_expanded <- just_11_K_expanded %>%
  mutate(
    FREX_words = str_split(top_terms_vector, pattern = ",\\s*")
  )

just_11_K_expanded$labels <- c("Urban planning", "Tourism", "Food (and agriculture)", "Circular economy and business", "Survey methods", "Economic models",
                               "Work", "Critique of capitalism", "Climate policies and models", "Decoupling and indicators", "Environmental justice_our")

# For savin_van_den_bergh_table2
savin_van_den_bergh_table2 <- savin_van_den_bergh_table2 %>%
  mutate(
    MDT_words = str_split(`Most discriminating terms`, pattern = ",\\s*")
  )


combinations <- tidyr::crossing(
  just_11_K_expanded %>% select(labels, FREX_words),
  savin_van_den_bergh_table2 %>% select(`Topic Label`, MDT_words)
)

combinations <- combinations %>%
  rowwise() %>%
  mutate(
    overlap_count = length(intersect(FREX_words, MDT_words))
  ) %>%
  ungroup()

result <- combinations %>%
  select(labels, `Topic Label`, overlap_count) %>%
  arrange(desc(overlap_count))


# Remove zero counts
result <- result[result$overlap_count > 0, ]

# Rename 'Freq' column to 'weight'
names(result)[3] <- "weight"

g <- graph_from_data_frame(result, directed = FALSE)
g_tidy <- as_tbl_graph(g)

# Update the type variable to match the scale levels
g_tidy <- g_tidy %>%
  mutate(type = ifelse(name %in% unique(savin_van_den_bergh_table2$`Topic Label`), "Theme", "Label"),
         shape = ifelse(type == "Theme", "square", "circle"))

set.seed(123)  # For reproducibility

layout <- create_layout(g_tidy, layout = "fr",
                        weights = E(g)$weight)

# Plot using ggraph with adjusted node size and consistent mappings
savin_van_den_bergh_comparison <- ggraph(layout) +
  # Edges
  geom_edge_link(aes(width = weight), alpha = 0.6, color = "grey") +
  # Nodes
  geom_node_point(aes(fill = type, shape = type), size = 5, color = "black") +
  # Node labels with increased font size
  geom_node_text(aes(label = name),
                 size = 5,
                 repel = TRUE,
                 max.overlaps = Inf,
                 box.padding = unit(0.5, "lines"),
                 point.padding = unit(0.3, "lines")) +
  # Scales
  scale_edge_width(range = c(0.5, 2)) +
  scale_fill_manual(values = c("Label" = "lightblue", "Theme" = "salmon")) +
  scale_shape_manual(values = c("Label" = 21, "Theme" = 22)) +
  # Theme adjustments
  theme_void() +
  theme(legend.position = "none",
        plot.background = element_rect(color = "black", fill = NA, size = 1))+ labs(title = "Topics compared to the topics from Savin & van den Bergh, 2024")

savin_van_den_bergh_comparison



# ---- Final sensitivity analysis: compare main model with one with min docfreq=5 (more terms kept)

library(quanteda)
library(stm)
library(tidytext)
library(tidyverse)
library(ggtext)
library(ggthemes)

dfm_all_data <- readRDS("data/dfm_data_withoutstopwords_final__min5_15082025.RDS")

stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format

model <- tibble(K = c(20)) %>%
  mutate(topic_model = map(K, ~stm(documents = stm_all_data$documents, 
                                   vocab = stm_all_data$vocab, K = ., 
                                   prevalence = ~s(Year), 
                                   init.type = "Spectral", data=stm_all_data$meta,
                                   verbose = T)))

write_rds(model, "results/degrowth_model_with_min_term_freq_5.RDS")

chosen_model <- read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

library(dplyr)
library(purrr)
library(stringr)
library(stm)

library(dplyr)
library(purrr)
library(stringr)
library(tidytext)  # for tidy(stm, matrix = "beta")

# Extract STM objects
stm1 <- model$topic_model[[1]]
stm2 <- chosen_model$topic_model[[1]]

# Get top 20 terms for each topic in each model
top_terms1 <- tidy(stm1, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 20, with_ties = FALSE) %>%
  summarise(terms = list(term), .groups = "drop")

top_terms2 <- tidy(stm2, matrix = "beta") %>%
  group_by(topic) %>%
  slice_max(beta, n = 20, with_ties = FALSE) %>%
  summarise(terms = list(term), .groups = "drop")

# For each topic in model1, find best matching topic in model2 and compute overlaps/differences
matches2 <- map_dfr(seq_len(nrow(top_terms2)), function(i) {
  t1_id   <- top_terms2$topic[i]
  terms1  <- top_terms2$terms[[i]]
  
  # Overlap with every topic in model 2
  overlap_counts <- map_int(top_terms1$terms, ~ length(intersect(terms1, .x)))
  best_idx <- which.max(overlap_counts)
  
  t2_id   <- top_terms1$topic[best_idx]
  terms2  <- top_terms1$terms[[best_idx]]
  
  overlap <- intersect(terms1, terms2)
  only_1  <- setdiff(terms1, terms2)
  only_2  <- setdiff(terms2, terms1)
  
  tibble(
    topic_model1   = t1_id,
    topic_model2   = t2_id,
    terms_model1   = str_c(terms1, collapse = ", "),
    terms_model2   = str_c(terms2, collapse = ", "),
    n_overlap      = length(overlap),
    only_in_model1 = str_c(only_1, collapse = ", "),
    only_in_model2 = str_c(only_2, collapse = ", ")
  )
})

matches2

write_csv2(matches, "results/overlap_between_models_with_min_term_freq_5.csv")







