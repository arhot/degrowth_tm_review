

library(stm)
library(dplyr)
library(broom)
library(purrr)
library(tidytext)
library(tidyr)

data_frame_of_varying_K_models <- readRDS("results/degrowth_models_with_varying_K_02022025.RDS") 

many_models <- tibble(
  model_id = 1:16,
  stm_model = data_frame_of_varying_K_models
)

# Function to extract top terms for each topic from an STM model using tidy()
extract_top_terms_per_topic <- function(model, n = 10) {
  tidy_model <- tidy(model)
  K <- model$settings$dim$K  # Extracting the number of topics (K) from the model
  
  top_terms_per_topic <- tidy_model %>%
    group_by(topic) %>%
    arrange(desc(beta)) %>%
    slice_head(n = n) %>%
    summarise(
      K = first(K),  # Include K in the output
      topic_number = first(topic),  # Include topic number
      top_terms_list = list(term),
      top_terms_vector = paste(term, collapse = ", ")
    )
  
  return(top_terms_per_topic)
}


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

