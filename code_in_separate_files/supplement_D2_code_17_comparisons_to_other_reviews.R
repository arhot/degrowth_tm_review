library(tidyverse)
library(igraph)
library(ggraph)
library(ggplot2)
library(tidygraph)
library(dplyr)
library(stringr)
library(fuzzyjoin)
library(readr)

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

data_frame_of_varying_K_models <- readRDS("results/degrowth_models_with_varying_K_02102024.RDS") 

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


