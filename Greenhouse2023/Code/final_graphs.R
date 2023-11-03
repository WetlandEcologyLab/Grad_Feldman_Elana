#Import####
load("main_dfs.RData")
library(tidyverse)
library(viridis)
library(patchwork)

# Cover graphs ####
##Prep####
gh <- cover_dat
gh_final <- gh %>%
  filter(gh$Date == "2023-03-01") 

##Native cover####
trt_label <- c("W" = "Present", 
               "WO" = "Absent")
gh_final %>%
  select(-Phrag) %>%
  filter(Mix != "PHAU")%>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = Total, color = Density)) +
  facet_grid(~factor(Phrag_Presence, levels=c("WO", "W")), labeller = as_labeller(trt_label)) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar") +
  labs(x = "Seed Mix", y = "Final Total Native Cover") +
  scale_color_hue(labels = c('High', 'Low'))

ggsave("native_cover_by-mix.jpeg")

##Phrag Cover####
colors <- c("darkgray", "#F8766D", "#00BFC4")
gh_final %>% 
  mutate(Mix = factor(Mix,
                      levels = c("PHAU","Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(x = Mix, y = Phrag, color = Density)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun.data = mean_se, geom = "errorbar") +
  scale_color_manual(values = colors)+
  labs(x = "Seed Mix", y = "Final *P. australis* Cover")+
  scale_color_hue(labels = c('High', 'Low', 'Control')) +
  theme(axis.title.y = ggtext::element_markdown()) +
  scale_x_discrete(labels = c("Grass" = "Grass",
                              'Bulrush' = "Bulrush",
                              'Forb' = 'Forb',
                              'Equal'= "Equal",
                              'PHAU' = 'Control'))
  

ggsave("phrag_cover_by-mix.jpeg")

##Stacked bargraph####
###Functional group ####
####With phrag ####
colors <- c("#C77CFF", "#7CAE00", "#00BFC4", "#F8766D")

gh_totals <- gh_final %>%
  filter(Mix != "PHAU") %>%
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE),
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = Phrag)

gh_tw <- gh_totals %>%
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

a <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal", "PHAU"))) %>% 
  filter(Phrag_Presence != "WO") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "", fill = "Group", 
       title = "*P.australis* Present")+
  facet_grid(~Mix) +
  scale_fill_manual(values = colors)+
  theme(plot.title = ggtext::element_markdown(size = 9),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))

# gh_tw %>%
#   mutate(Mix = factor(Mix,
#                       levels = c("Grass", "Bulrush", "Forb", "Equal", "PHAU"))) %>% 
#   filter(Phrag_Presence != "WO") %>%
#   ggplot(aes(fill = group, y = cover, x = Density)) +
#   geom_bar(position = "fill", stat = "identity") +
#   labs(x = "Density", y = "Relative Abundance", fill = "Group")+
#   facet_grid(~Mix) +
#   scale_fill_manual(values = colors)+
#   theme(plot.title = ggtext::element_markdown(size = 10),
#         legend.key.size = unit(.25, "cm"),
#         legend.title = element_text(size = 10),
#         legend.position = "right") +
#   scale_x_discrete(labels = c("H" = "High",
#                               'L' = "Low"))
# ggsave("stacked_cover_group_w.jpeg")

#### Without phrag ####
b <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  filter(Phrag_Presence != "W") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Group", 
       title = "(a) *P.australis* Absent")+
  facet_grid(~Mix) +
  scale_fill_manual(values = colors)+
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low")) 



# ggsave("stacked_group.jpeg")

### Stacked with Species ####
####with phrag ####
gh_sl <- gh_final %>% 
  pivot_longer(
    cols = 7:16,
    names_to = "Species",
    values_to = "Cover"
  )

#colors
cp <- c("khaki3", "yellow2", "wheat2",
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

c <- gh_sl %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "", fill = "Species",
       title = "*P.australis* Present") +
  facet_grid(~Mix)+
  scale_fill_manual(values = cp,
                    labels = c("BOMA", "SCAC", "SCAM",
                               "DISP", "MUAS", "PUNU",
                               "EUMA", "EUOC", "SOCA",
                               "PHAU"))+
  theme(plot.title = ggtext::element_markdown(size = 9),
        legend.text = ggtext::element_markdown(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low")) +
  guides(fill=guide_legend(ncol = 1))

# gh_sl %>% 
#   filter(Phrag_Presence != "WO") %>% 
#   mutate(Species = factor(Species, 
#                           levels = c("BOMA", "SCAC", "SCAM",
#                                      "DISP", "MUAS", "PUNU",
#                                      "EUMA", "EUOC", "SOCA",
#                                      "Phrag"))) %>% 
#   ggplot(aes(fill = Species, y = Cover, x = Density)) +
#   geom_bar(position = "fill", stat = "identity") +
#   labs(x = "Density", y = "Relative Abundance", fill = "Species") +
#   facet_grid(~Mix)+
#   scale_fill_manual(values = cp,
#                     labels = c('*Bolboschoenus martitimus*', 
#                                '*Schoenoplectus acutus*',
#                                '*Schoenoplectus americanus*', 
#                                '*Distichlis spicata*',
#                                '*Muhlenbergia asperifolia*', 
#                                '*Puccinellia nuttalliana*',
#                                "*Eutrochium maculatum*",
#                                '*Euthamia occidentalis*',
#                                "*Solidago canadensis*",
#                                '*Phragmites australis*'))+
#   theme(plot.title = ggtext::element_markdown(size = 10),
#         legend.text = ggtext::element_markdown(),
#         legend.key.size = unit(.25, "cm"),
#         legend.title = element_text(size = 10),
#         legend.position = "bottom")+
#   scale_x_discrete(labels = c("H" = "High",
#                               'L' = "Low")) +
#   guides(fill=guide_legend(ncol = 2))
# 
# ggsave("stacked_species_cover_w.jpeg")

####no phrag ####
#colors
d <- gh_sl %>% 
  filter(Phrag_Presence != "W") %>% 
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Species",
       title = "(b) *P.australis* Absent") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp) +
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))
# ggsave("stacked_species.jpeg")

##Reduction graph####

#I averaged across all the treatments/controls first because this was fully randomized - no blocks

#get the treatment PHAU values
b <- gh_final %>%
  filter(Phrag_Presence == "W") %>%
  dplyr::select(Mix, Density, Phrag_Presence, Replicate, Phrag)

#get the control values
final.matrix <- gh_final %>% 
  filter(Mix == "PHAU") %>%
  dplyr::select(Mix, Replicate, Total)

c <- mean(final.matrix$Total)

#calculate the percent cover across all 
final.df <- b %>%
  mutate(P.Cover.Red = (Phrag - c)/c)

#graph
# final.df %>%
#   mutate(Mix = factor(Mix,
#                       levels = c("Forb", "Bulrush", "Grass", "Equal"))) %>% 
#   ggplot(aes(x = Mix, y = P.Cover.Red * -1, color = Density), size = 2) +
#   ylim(0,1)+
#   stat_summary(aes(group = Density),
#                size = 2,
#                fun = mean, geom = "point", 
#                position = position_dodge(0.95)) +
#   stat_summary(aes(group = Density, width = 0),
#                size = 1,
#                fun.data = mean_se, geom = "errorbar",
#                position = position_dodge(0.95)) +
#   theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
#         axis.title.y = ggtext::element_markdown()) +
#   labs(y = "Proportion Reduction in *Phragmites* Cover", x = "Mix")+
#   scale_color_manual(labels = c('High', 'Low'), values = c("red3", "darkblue"))

#ggsave("phrag_cover_red.jpeg")
cover <- final.df %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  ggplot(aes(x = Mix, y = P.Cover.Red * -1, color = Density), size = 1) +
  ylim(0,1)+
  stat_summary(aes(group = interaction(Mix, Density)),
               size = 1,
               fun = mean, geom = "point", 
               position = position_dodge(0.95)) +
  stat_summary(aes(group = interaction(Mix, Density), width = 0),
               size = .5,
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  labs(y = "Reduction in *P.australis* <br>Proportional Cover", x = "", title = "(a)")+
  scale_color_manual(values = c("darkblue", "red3"))

## Raw Data over time #### 
### Native cover ####
cover_dat %>% 
  filter(Mix != "PHAU") %>% 
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  mutate(Phrag_Presence = factor(Phrag_Presence,
                                 levels = c("WO", "W"),
                                labels = c("Absent", "Present"))) %>% 
  ggplot(aes(x = Date, y = Total, color = Density, shape = Phrag_Presence)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2, position = position_jitter(seed=3)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = 0),
               fun.data = mean_se, geom = "errorbar", position = position_jitter(seed=3)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(),
        legend.title = ggtext::element_markdown(),
        legend.position = "bottom") +
  labs(y = "Total Proportional Native Cover", x = "Date",
       shape = "*P.australis* Presence") +
  facet_grid(~Mix) +
  scale_color_manual(values = c("darkblue", "red3")) 

ggsave("native_cover_over-time.jpeg")

###Phrag cover ####
#NAs for density and phrag_presence are throwing off the graph
cover_dat$Density <- as.character(cover_dat$Density)
cover_dat$Phrag_Presence <- as.character(cover_dat$Phrag_Presence)
cover_dat$Density[is.na(cover_dat$Density)] <- "Control"
cover_dat$Phrag_Presence[is.na(cover_dat$Phrag_Presence)] <- "Control"
cover_dat$Density <- as.factor(cover_dat$Density)
cover_dat$Phrag_Presence <- as.factor(cover_dat$Phrag_Presence)

cover_dat %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Mix = factor(Mix,
                      levels = c('PHAU',"Bulrush", "Forb", "Grass", "Equal"),
                      labels = c("Control", "Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                      levels = c("Control", "L", "H"),
                      labels = c("Control", "Low", "High"))) %>% 
  ggplot(aes(x = Date, y = Phrag, color = Density)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence)),
               fun = mean, geom = "point", size = 2, position = position_dodge(width = 1)) +
  stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
               fun.data = mean_se, geom = "errorbar", position = position_dodge(width = 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(),
        legend.position = "bottom") +
  labs(y = "Proportional *P.australis* Cover", x = "Date") +
  facet_grid(~Mix) +   
  scale_color_manual(values = c("#7D7D7D", "darkblue", "red3"))

ggsave("phrag_cover_over-time.jpeg")

# Biomass graphs ####

##Native biomass####
biomass_dat %>%
  group_by(Mix, Density, Phrag_Presence, Replicate) %>% 
  summarize(PHAU = PHAU,
            Total_Native = sum(EUMA,SOCA, EUOC, SCAC,
                               SCAM, DISP, MUAS, PUNU,BOMA, na.rm = TRUE)) %>% 
  filter(Mix != "PHAU")%>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  mutate(Phrag_Presence = factor(Phrag_Presence,
                                 levels = c("WO", "W"),
                                 labels = c("Absent", "Present"))) %>% 
  ggplot(aes(x = Mix, y = Total_Native, color = Density)) +
  facet_grid(~factor(Phrag_Presence)) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar", width = 0) +
  labs(x = "Seed Mix", y = "Total Native Biomass")+
  scale_color_manual(values = c("darkblue", "red3"))+
  coord_cartesian(ylim = c(0, 90))

ggsave("native_biomass_by-mix.jpeg")

##Phrag Biomass####
biomass_dat$Density <- as.character(biomass_dat$Density)
biomass_dat$Density[is.na(biomass_dat$Density)] <- "C"
biomass_dat$Density <- factor(biomass_dat$Density,
                              levels = c("C", "L", "H"),
                              labels = c("Control", "Low", "High"))


biomass_dat %>% 
  mutate(Mix = factor(Mix,
                      levels = c("PHAU", "Bulrush", "Forb", "Grass",  "Equal"))) %>% 
  ggplot(aes(x = Mix, y = PHAU, color = Density)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun.data = mean_se, geom = "errorbar", width = 0) +
  labs(x = "Seed Mix", y = "*P.australis* Biomass")+
  scale_color_manual(values = c("#7D7D7D", "darkblue", "red3"))+
  scale_x_discrete(labels = c("Grass" = "Grass",
                              'Bulrush' = "Bulrush",
                              'Forb' = 'Forb',
                              'Equal'= "Equal",
                              'PHAU' = 'Control')) +
  theme(axis.title.y = ggtext::element_markdown()) +
  coord_cartesian(ylim = c(0, 16))

ggsave("phrag_biomass_by-mix.jpeg")

##Stacked bargraph####

###by functional group ####

####With phrag ####
colors <- c("#C77CFF",  "#7CAE00", "#00BFC4", "#F8766D")
biomass_t <- biomass_dat %>%
  filter(Mix != "PHAU") %>%
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE),
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = PHAU)

biomass_l <- biomass_t %>%
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

e <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c( "Bulrush", "Forb", "Grass","Equal", "PHAU"))) %>% 
  filter(Phrag_Presence != "WO") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "", fill = "Group",
       title = "*P.australis* Present")+
  facet_grid(~Mix) +  
  theme(plot.title = ggtext::element_markdown(size = 9),
                            legend.key.size = unit(.25, "cm"),
                            legend.title = element_text(size = 9),
        legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))+
  scale_fill_manual(values = colors)

#### Without phrag ####
f <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  filter(Phrag_Presence != "W") %>%
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Group",
       title = "(c )*P.australis* Absent")+
  facet_grid(~Mix) +
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))+
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))+
  scale_fill_manual(values = colors)

# ggsave("stacked_biomass_group.jpeg")

### Stacked with Species ####
####with phrag ####
biomass_sl <- biomass_dat %>% 
  pivot_longer(
    cols = 5:14,
    names_to = "Species",
    values_to = "Biomass"
  )

#colors
cp <- c("yellow2", "wheat2",
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

g <- biomass_sl %>% 
  filter(Phrag_Presence != "WO") %>% 
  mutate(Species = factor(Species, 
                          levels = c("SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Species",
       title = "*P.australis* Present") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp)+ 
  theme(plot.title = ggtext::element_markdown(size = 9),
        legend.text = ggtext::element_markdown(),
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 9),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))+
  guides(fill=guide_legend(ncol = 1))

####no phrag ####
#colors
h <- biomass_sl %>% 
  filter(Phrag_Presence != "W") %>% 
  mutate(Species = factor(Species, 
                          levels = c("SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Relative Abundance", fill = "Species")+
  ggtitle("(d) *P.australis* Absent") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp)+  
  theme(plot.title = ggtext::element_markdown(size = 9), 
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  scale_x_discrete(labels = c("H" = "High",
                              'L' = "Low"))
(b+a) / (d+c) / (f+e) / (h+g) + plot_layout(guides = "collect")
ggsave("stacked_all.jpeg", height = 2500, width = 2500, units = "px")

# ggsave("stacked_biomass_species.jpeg")

##Reduction graph####
#I averaged across all the treatments/controls first because this was fully randomized - no blocks

#get the treatment PHAU values
b <- biomass_dat %>%
  filter(Phrag_Presence == "W") %>%
  dplyr::select(Mix, Density, Phrag_Presence, Replicate, PHAU)

#get the control values
final.matrix <- biomass_dat %>% 
  filter(Mix == "PHAU") %>%
  dplyr::select(PHAU)

c <- mean(final.matrix$PHAU)

#calculate the percent cover across all 
final.df <- b %>%
  mutate(P.Biomass.Red = (PHAU - c)/c) %>% 
  mutate(Pos.Red = P.Biomass.Red * -1)

#graph
biomass <- final.df %>% 
  mutate(Mix = factor(Mix,
                      levels = c( "Bulrush", "Forb","Grass", "Equal")),
         Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  ggplot(aes(x = Mix, y = Pos.Red, color = Density), size = 1) +
  scale_y_continuous(name = "Reduction in *P.australis* <br>Biomass", #for some reason ylim changes the data
                     breaks = seq(-.25, 2, by = .25)) +
  stat_summary(aes(group = interaction(Mix, Density)),
               fun = mean, geom = "point",
               position = position_dodge(0.95),
               size = 1) +
  stat_summary(aes(group = interaction(Mix, Density), width = 0),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(0.95), 
               size = .5) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9), 
        axis.title.y = ggtext::element_markdown(size = 11),
        plot.title = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  labs( x = "", title = "(b)")+
  scale_color_manual(values = c("darkblue", "red3"))


#Combine red graphs ####
cover/biomass
ggsave("red_cover_biomass_tog.jpeg")

