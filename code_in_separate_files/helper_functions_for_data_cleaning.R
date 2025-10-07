
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


