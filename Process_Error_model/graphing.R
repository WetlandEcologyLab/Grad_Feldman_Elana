load("r_values_table.RData")
load("main_dfs.RData")
View(data_read)

library(tidyverse)
library(emmeans)
library(patchwork)

r_table <- data_read

# Graphs of the changes in r by species ####

r_table <- r_table %>% 
  mutate(trt = as.factor(paste0(Density, Phrag_Presence)))

r_table %>% 
  ggplot(aes(x = trt, y = value, color = Density, shape = Phrag_Presence)) +
  geom_point(size = 2) +
  facet_wrap(~Species) +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
  labs(x = "", y = "Growth Rate (r)")

ggsave("r-value_by-species.jpeg")

# Get the cover and biomass data ####

#using the full dataset
final.traits <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16" & Phrag_Presence == "W") %>%
  select(Species, Block, Density, Cover.Native, Height.Native, Cover.Phrag)

final.biomass <- biomass %>%
  filter(Phrag_Presence == "W") %>%
  select(Species, Block, Density, Native.Biomass, Phrag.Biomass)

final <- left_join(final.traits, final.biomass, by = c("Species", "Density", "Block"))
final.all <- left_join(final, r_table, by = c("Species", "Density"))

# Relationship between r value and phrag biomass ####

lm_biomass <- lm(value ~ Phrag.Biomass + Density, data = final.all)
summary(lm_biomass)
#relationship is significant for both r value and density
#r value shows negative relationship (p < 2e-16)
#over all r2 = .36
#density is significant relationship (p = 0.00659)
#phrag cover intercept lower in low density - not sure how to interpret that 


cor(final.all$Phrag.Biomass, final.all$value, method = "pearson", use = "complete.obs")
#negative correlation of -0.586

a <- final.all %>% 
  ggplot(aes(x = value, y = Phrag.Biomass, color = Density)) +
  geom_point(size = 2) +
  labs(x = "Growth Rate (r)", y = "*Phragmites* Biomass") +
  theme(axis.title.y = ggtext::element_markdown(),
        legend.position = "none") +
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) +
  ylim(0, 50)


# Relationship between r values and phrag cover ####

lm_cover <- lm(value ~ Cover.Phrag + Density, data = final.all)
summary(lm_cover)
#relationship is significant for both r value and density
#r value shows negative relationship (p < 2e-16)
#over all r2 = .37
#density is significant relationship (p = 0.0253)
#biomass coefficient lower in lower density - not sure how to interpret that 

cor(final.all$Cover.Phrag, final.all$value, method = "pearson", use = "complete.obs")
#negative correlation of -0.597

b <- final.all %>% 
  ggplot(aes(x = value, y = Cover.Phrag, color = Density)) +
  geom_point(size = 2) +
  labs(x = "Growth Rate (r)", y = "*Phragmites* Cover") +
  theme(axis.title.y = ggtext::element_markdown())+
  geom_smooth(method="lm", se=FALSE, fullrange = TRUE) 

a + b
ggsave("growth-rate_biomass_cover.jpeg")
