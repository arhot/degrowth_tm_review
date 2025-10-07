

library(stm)
library(dplyr)
library(broom)
library(purrr)
library(tidytext)
library(tidyr)
library(quanteda)

many_models <- readRDS("results/degrowth_models_with_varying_K__3_10002022025.RDS") 

dfm_all_data <- readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")

stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format
k_result <- many_models %>%
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, stm_all_data$documents),
         #        residual = map(STM, checkResiduals, stm_all_data$documents),
         # bound =  map_dbl(STM, function(x) max(x$convergence$bound)),
         # lfact = map_dbl(STM, function(x) lfactorial(x$settings$dim$K)),
         # lbound = bound + lfact,
         # iterations = map_dbl(STM, function(x) length(x$convergence$bound))
         )

k_result

k_result %>%
  mutate(
            Exclusivity = map_dbl(exclusivity, mean),
            `Semantic coherence` = map_dbl(semantic_coherence, mean)
            ) %>%
  gather(Metric, Value, -K) %>%
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(size = 1.5, alpha = 0.7, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y")

model_avg <- k_result %>%
  mutate(
            Exclusivity = map_dbl(exclusivity, mean),
            SemanticCoherence = map_dbl(semantic_coherence, mean),
            Type = "Average")  %>% select(-exclusivity, -semantic_coherence)




ggplot(combined_k10_25, aes(x = SemanticCoherence, y = Exclusivity, 
                          color = as_factor(K)
                          )
                          ) +
  geom_point(data = filter(combined_k10_25, Type == "Topic"),
             aes(shape = Type), size = 1, alpha = 0.3, show.legend = FALSE) +
  geom_point(data = filter(combined_k10_25, Type == "Average"), 
             aes(shape = Type), size = 3, alpha = 0.8, show.legend = TRUE) +
  geom_text(data = filter(combined_k10_25, Type == "Topic"),
            aes(label = K), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.5, show.legend = FALSE) +
  geom_text(data = filter(combined_k10_25, Type == "Average"), 
            aes(label = K), hjust = -0.2, vjust = -0.2, size = 3, alpha = 0.9, show.legend = FALSE) +
  # Customize color palette and shapes
  scale_color_manual(values = scales::hue_pal()(length(unique(combined_data$K)))) +
  labs(title = "Scatterplot of Exclusivity and Semantic Coherence by Number of Topics") +
  theme_minimal()

ggsave("supplements_for_submission/supplement_C3figure_with_plot_and_topic_level_statistical_indicators_from_models_with_varying_number_of_topics.pdf", plot = combined_data, device = cairo_pdf, width = 7, height = 5, dpi = 300)


figure3 <- ggplot(combined_data, aes(x = SemanticCoherence, y = Exclusivity)) +
  geom_point(
    data = filter(combined_data, Type == "Average"), 
    aes(shape = Type), size = 1, alpha = 0.8
  ) +
  geom_text(
    data = filter(combined_data, Type == "Average"), 
    aes(label = K),
    nudge_x = 0.02,  # move right
    nudge_y = 0.02,  # move up
    size = 3, alpha = 0.9
  ) +
  labs(title = "Scatterplot of Exclusivity and Semantic Coherence by Number of Topics") +
  theme_minimal() +
  theme(legend.position = "none")

ggsave("supplements_for_submission/figure3_exclusivity_and_semantic_coherence_by_topic_number.pdf", plot = figure3, device = cairo_pdf, width = 7, height = 5, dpi = 300)

