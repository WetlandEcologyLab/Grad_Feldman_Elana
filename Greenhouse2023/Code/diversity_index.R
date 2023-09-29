#Import####
load("Cleaned_Data/main_dfs.RData")
library(vegan)
library(tidyverse)
library(patchwork)

#make wide
#View(cover_dat)

#only want the final cover
final.dat <- cover_dat %>% 
  filter(Date == "2023-03-01")

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Mix', 'Density', 'Phrag_Presence', 'Replicate'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#make all percentages
final.dat2 <- mutate_if(final.dat2, is.numeric, ~.*100)

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- dplyr::select(final.dat3, -c("Tub", 'Date', 'Total', 'Notes'))
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#Plot ####
#change order of phrag presence and also labels
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                 labels = c("Absent", "Present"))

((a <- final.dat %>% 
  filter(Mix != "PHAU") %>% 
  ggplot(aes(x = Mix, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Mix)),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Mix), width = 0),
               fun.data = mean_se, geom = "errorbar") +
  facet_wrap(~Phrag_Presence) +
  ylab("Mean Shannon Diversity Index") +
  ggtitle("(a)") +
  scale_color_manual(labels = c('High', 'Low'), values = c("red3", "darkblue"))+ #change legend labels
  theme(plot.title = element_text(size = 9),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 0.9)) +
  ylim(0, 2)
))

# Biomass ####
#make wide
#View(biomass_dat)

final.dat <- biomass_dat

#make a new column with the tub
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Mix', 'Density', 'Phrag_Presence', 'Replicate'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Now try the diversity calculation
final.dat3 <- dplyr::select(final.dat3, -"Tub")
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#Plot ####
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                   labels = c("Absent", "Present"))

((b <- final.dat %>% 
  filter(Mix != "PHAU") %>% 
  ggplot(aes(x = Mix, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Mix)),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Mix), width = 0),
               fun.data = mean_se, geom = "errorbar") +
  ylab("")+
  ggtitle("(b)") +
  facet_wrap(~Phrag_Presence) +
  scale_color_manual(labels = c('High', 'Low'), values = c("red3", "darkblue")) + #change legend labels
  theme(plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "right") +
  ylim(0, 2)
))

# Combine graphs ####
a+b
ggsave("shannon_tog.jpeg")

