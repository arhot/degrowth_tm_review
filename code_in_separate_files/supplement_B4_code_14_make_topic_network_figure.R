alibrary(stminsights)
library(ggraph)
library(igraph)

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

library(ggplot2)
library(ggrepel)
library(ggraph)
library(igraph)

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
