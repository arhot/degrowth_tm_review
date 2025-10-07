library(tidyverse)

# Function for making a single paper plot with all topics (font sizes optimized for pdf saving, increase font size for screen viewing)

td_gamma <- read.csv("supplements_for_submission/supplement_B3_all_papers_with_share_from_each_topic_and_metadata.csv")

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

ggsave("supplements_for_submission/figure3_example_of_one_topic_paper.pdf", plot = a, device = cairo_pdf, width = 7, height = 5, dpi = 300)


b <- topic_bar_plot(td_gamma, "10.1016/j.ecolecon.2020.106653", label_id = "Mair, S; Druckman, A; Jackson, T. (2020) A tale of two utopias: Work in a post-growth world")

ggsave("supplements_for_submission/figure4_example_of_multi_paper.pdf", plot = b, device = cairo_pdf, width = 7, height = 5, dpi = 300)


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
