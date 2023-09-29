#I created a new CSV with all the values because it was easier than trying to make a table in R

library(tidyverse)

values <- read.csv("r_values.csv")
#View(values)
#glimpse(values)

graph_values <- values %>% 
  select(-c(Model, R2, n0, sd))

graph_r <- graph_values %>% 
  select('Species', 'HW', 'HWO', 'LW', 'LWO') %>% 
  pivot_longer(cols = c('HW', 'HWO', 'LW', 'LWO'),
                             names_to = "names",
                             values_to = "value")

graph_upr <- graph_values %>% 
  select('Species', 'HW_upr', 'HWO_upr', 'LW_upr', 'LWO_upr') %>% 
  pivot_longer(cols = c('HW_upr', 'HWO_upr', 'LW_upr', 'LWO_upr'),
               names_to = "upr_names",
               values_to = "upr_value")
graph_upr$names <- graph_r$names

graph_lwr <- graph_values %>% 
  select('Species', 'HW_lwr', 'HWO_lwr', 'LW_lwr', 'LWO_lwr') %>% 
  pivot_longer(cols = c('HW_lwr', 'HWO_lwr', 'LW_lwr', 'LWO_lwr'),
               names_to = "lwr_names",
               values_to = "lwr_value")
graph_lwr$names <- graph_r$names

graph1 <- left_join(graph_r, graph_upr, by = c("Species", "names"))
graph2 <- left_join(graph1, graph_lwr, by = c("Species", 'names'))

final <- graph2 %>% 
  select(-c(upr_names, lwr_names))

final %>% 
  ggplot(aes(x = names, y = value)) +
  geom_point() +
  facet_wrap(~Species, ncol = 6) +
  geom_errorbar(aes(ymin = lwr_value, ymax = upr_value)) +
  xlab("Treatment") +
  ylab("Intrinsic Rate of Growth (r)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9))

ggsave("r_values_graph.jpeg")
