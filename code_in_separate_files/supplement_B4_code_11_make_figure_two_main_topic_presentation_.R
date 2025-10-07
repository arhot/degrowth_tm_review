library(stringdist)
library(fuzzyjoin)
library(tidyverse)

source("supplements_for_submission/supplement_A5_code_04_helper_functions_for_data_cleaning.R")

dfm_all_data <- readRDS("data/dfm_data_withoutstopwords_final_28122023.RDS")

stm_all_data <- convert(dfm_all_data, to = "stm")  # convert to stm format

model <- readr::read_rds("results/degrowth_models_with_varying_K_02022025.RDS") %>% filter(K == 20)

first_model <- model %>% 
  filter(K == 20) %>% 
  pull(topic_model) %>% 
  .[[1]]

td_beta <- tidy(first_model, matrix = "beta") 
td_frex_top10 <- tidy(first_model, matrix="frex") %>% group_by(topic) %>% slice(1:10)
td_frex_top10_labels <- td_frex_top10 %>%
  group_by(topic) %>%
  summarize(term = paste(term, collapse = ", "),
            .groups = "drop") %>%
  mutate(Frex_label = paste0("Topic ", topic, ": ", term))
td_gamma <- tidy(first_model, matrix = "gamma",
                 document_names = stm_all_data$meta$Title)


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
  ggtext::geom_richtext(hjust = 0, nudge_y = 0.0005, size = 1.8, fill = NA, label.color = NA) +
  geom_text(aes(label = labels, y = 0.001), hjust = 0, size = 2.5) +
  coord_flip() +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 0.18),
    labels = scales::label_percent()
  ) +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.92, 0.18),  # Adjust the legend position
    legend.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_text(size = 2.5*2.8),
    legend.text = element_text(size = 2.5*2.8),
    legend.title = element_text(size = 2.5*2.8)
  ) +
  labs(
    x = "",
    y = "Percent of corpus from topic",
    fill = "Topic type", 
  )

main_topic_figure_for_paper

ggsave("supplements_for_submission/figure2_topic_model_with_943_papers_and_20_topics.pdf", plot = main_topic_figure_for_paper, device = cairo_pdf, width = 7, height = 5, dpi = 300)


ggsave("supplements_for_submission/figure2_topic_model_with_943_papers_and_20_topics.png", plot = main_topic_figure_for_paper, width = 9.69, height = 6.27, units = "px", dpi=300)



library(ggtext)
library(scales)
library(ggpattern)

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
