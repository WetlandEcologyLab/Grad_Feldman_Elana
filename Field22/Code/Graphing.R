load("clean_dfs.RData")
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(vegan)

#Invasive vs native####
##Fb####
graph_data <-fb %>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, Cheno, Typha, 
         BOMA, DISP, EUMA, SYCI, LEFA, SCAC, BICE, BIFR, EUOC, MUAS, SCAM, RUMA,
         RUST, Unk_Bulrush, SARU, Tamarisk) %>%  #remove unnecessary columns
  pivot_longer(
    cols = 6:24, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", "Tamarisk") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP", "MUAS") & Group == 3 ~ "Seeded",
    SPP %in% c("EUOC", "EUMA") & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "BIFR", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>%  #make a new column for species status
  group_by(Block, Group, Density, Date, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals

graph_data$Group <- factor(graph_data$Group, levels = c(5, 4, 3, 2, 1, 10),
                    labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial forb", "Control"))
graph_data$Density <- factor(graph_data$Density, levels = c("H", "L", "C"),
                      labels = c("High", "Low", "Control"))

((fb_plot <- graph_data %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(a) Farmington Bay") + 
    scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6)) +
    ylim(0, 1) +
    facet_wrap(~Group)
))


## UL####
graph_data2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
         Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
         EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>%
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% 
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>% 
  group_by(Block, Date, Density, Group, Status) %>%
  summarise(PC = sum(Percent_Cover, na.rm = TRUE))

graph_data2$Group <- factor(graph_data2$Group, levels = c(5, 4, 3, 2, 1, 10),
                           labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                                      "Perennial forb", "Control"))
graph_data2$Density <- factor(graph_data2$Density, levels = c("H", "L", "C"),
                             labels = c("High", "Low", "Control"))

((ul_plot <- graph_data2 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(b) Utah Lake") + 
    scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6)) +
    ylim(0, 1) +
    facet_wrap(~Group)
))

fb_plot / ul_plot + plot_layout(guides = "collect")
ggsave("native_seeded_invasive_cover.jpeg")

##2023 FB ####
graph_data23 <-fb23 %>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, Typha, 
                BOMA, DISP, SCAC, SCAM, RUMA, RUST) %>%  #remove unnecessary columns
  pivot_longer(
    cols = 6:13, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP") & Group == 3 ~ "Seeded",
    SPP %in% c("RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>%  #make a new column for species status
  group_by(Block, Density, Group, Date, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals

graph_data23$Group <- factor(graph_data23$Group, levels = c(5, 4, 3, 2, 1, 10),
                            labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                                       "Perennial forb", "Control"))
graph_data23$Density <- factor(graph_data23$Density, levels = c("H", "L", "C"),
                              labels = c("High", "Low", "Control"))

((fb23_plot <- graph_data23 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover") + 
    scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9)) +
    ylim(0,1) +
    facet_wrap(~Group)
))

ggsave("invasive_native_2023.jpeg")

#Barchart of final seeded ####

##FB####
cp <- c("#A6CEE3", "#1F78B4" ,"#B2DF8A", "#33A02C", "#FB9A99" ,
        "#E31A1C", "#FDBF6F" ,"#FF7F00", "#CAB2D6","#6A3D9A")

fb2 <-fb %>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, Cheno, Typha, 
         BOMA, DISP, EUMA, SYCI, LEFA, SCAC, BICE, BIFR, EUOC, MUAS, SCAM, RUMA,
         RUST, Unk_Bulrush, SARU, Tamarisk) %>%  #remove unnecessary columns
  filter(Date == "2022-09-16") %>%  #only the last sampling date
  pivot_longer(
    cols = 6:24, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", "Tamarisk") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP", "MUAS") & Group == 3 ~ "Seeded",
    SPP %in% c("EUOC", "EUMA") & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "BIRF", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  )) 

#change BIFR to BICE because I am combining them
fb2$SPP[fb2$SPP == "BIFR"] <- "BICE"

fb2$Group <- factor(fb2$Group, levels = c(5, 4, 3, 2, 1, 10),
                    labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial forb", "Control"))
fb2$Density <- factor(fb2$Density, levels = c("H", "L"),
                    labels = c("High", "Low"))

fb_stack <- fb2 %>% 
  dplyr::filter(Status == "Seeded") %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~Group) +
  scale_fill_manual(values = cp,
                    labels = c('BICE',
                               'BOMA',
                               'DISP',
                               'EUMA',
                               'EUOC',
                               'MUAS',
                               'RUMA',
                               'SCAC',
                               'SCAM',
                               'SYCI'))+
  labs(x = "", y = "Relative Abundance", 
       fill = "Species", title = "(a) Farmington Bay") +
  theme(plot.title = element_text(size = 9))
  

##UL####

cp2 <- c("#A6CEE3", "#1F78B4" ,"#B2DF8A", "#FB9A99" ,
        "#FDBF6F" ,"#FF7F00", "#CAB2D6","#6A3D9A")

ul2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
         Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
         EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>%
  filter(Date == "2022-09-16") %>% 
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% 
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))

ul2$Group <- factor(ul2$Group, levels = c(5, 4, 3, 2, 1, 10),
                    labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial forb", "Control"))
ul2$Density <- factor(ul2$Density, levels = c("H", "L"),
                      labels = c("High", "Low"))

ul_stack <- ul2 %>% 
  dplyr::filter(Status == "Seeded") %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~Group) +
  scale_fill_manual(values = cp2,
                    labels = c("BICE",
                               "BOMA",
                               'DISP',
                               'EUOC',
                               'RUMA',
                               'SCAC',
                               'SCAM',
                               'SYCI'))+
  labs(x = "Native Seeding Density", y = "Relative Abundance", 
       fill = "Species", title = "(b) Utah Lake") +
  theme(plot.title = element_text(size = 9),
        legend.position = "none")

fb_stack / ul_stack + plot_layout(guides = "collect")
ggsave("stacked_species.jpeg")

##FB2023####
cp3 <- c("#1F78B4" ,"#B2DF8A", "#FDBF6F" ,"#FF7F00", "#CAB2D6")

fb232 <- fb23 %>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, Typha, 
                BOMA, DISP, SCAC, SCAM, RUMA, RUST) %>%  #remove unnecessary columns
  filter(Date == "2023-09-11") %>%  #only the last sampling date
  pivot_longer(
    cols = 6:13, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP") & Group == 3 ~ "Seeded",
    SPP %in% c("RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))

fb232$Group <- factor(fb232$Group, levels = c(5, 4, 3, 2, 1, 10),
                    labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial forb", "Control"))
fb232$Density <- factor(fb232$Density, levels = c("H", "L"),
                      labels = c("High", "Low"))

fb232 %>% 
  dplyr::filter(Status == "Seeded") %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~Group) +
  scale_fill_manual(values = cp3,
                    labels = c("BOMA",
                               "DISP",
                               'RUMA',
                               'SCAC',
                               'SCAM'))+
  labs(x = "Native Seeding Density", y  = "Relative Abundance", 
       fill = "Species") +
  theme(plot.title = element_text(size = 9),
        legend.position = "right")

ggsave("stacked_species_23.jpeg")

# Diversity index ####
##FB####
#only want the final cover
fb_di <- fb %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block, Plot, Group, Density, PHAU:RUST, SARU, Tamarisk)
  
#make all percentages
fb_di <- mutate_if(fb_di, is.numeric, ~.*100)

#make a new column with the tub
fb_di <- fb_di %>% 
  unite(col = "ID",
        c('Block', 'Plot'))

#name the rows
fb_di2 <- fb_di
row.names(fb_di2) <- fb_di2$"ID"

#Now try the diversity calculation
fb_di2 <- dplyr::select(fb_di2, -c(ID, Group, Density))
div <- diversity(fb_di2, "shannon")
fb_di$shannon <- div

#Plot
#change order of phrag presence and also labels
fb_di$Group <- factor(fb_di$Group, levels = c(5, 4, 3, 2, 1, 10),
                                   labels = c("Annual Forb", "Bulrush", "Grass", "Rush",  
                                              "Perennial Forb", "Control"))
fb_di$Density <- factor(fb_di$Density, levels = c("L", "H", "C"),
                        labels = c("Low", "High", "Control"))

((a <- fb_di %>% 
  ggplot(aes(x = Group, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Group)),
                 fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Group), width = 0),
                 fun.data = mean_se, geom = "errorbar") +
  labs(y = "Mean Shannon Diversity Index", x = "Seed Mix", title = "(a) Farmington Bay") +
  scale_color_manual(values = c("red3", "darkblue", "gray1")) + #change legend labels
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "blank") +
  ylim(0, 2.5)
))
##UL####
#only want the final cover
ul_di <- ul %>% 
  filter(Date == "2022-09-16") %>% 
  dplyr::select(Block, Plot, Group, Density, PHAU, BOMA:ASIN, ALPR, CYDA, BY:LASE)

#make all percentages
ul_di <- mutate_if(ul_di, is.numeric, ~.*100)

#make a new column with the tub
ul_di <- ul_di %>% 
  unite(col = "ID",
        c('Block', 'Plot'))

#name the rows
ul_di2 <- ul_di
row.names(ul_di2) <- ul_di2$"ID"

#Now try the diversity calculation
ul_di2 <- dplyr::select(ul_di2, -c(ID, Group, Density))
div <- diversity(ul_di2, "shannon")
ul_di$shannon <- div

#Plot
#change order of phrag presence and also labels
ul_di$Group <- factor(ul_di$Group, levels = c(5, 4, 3, 2, 1, 10),
                      labels = c("Annual Forb", "Bulrush", "Grass", "Rush",  
                                 "Perennial Forb", "Control"))
ul_di$Density <- factor(ul_di$Density, levels = c("L", "H", "C"),
                        labels = c("Low", "High", "Control"))

((b <- ul_di %>% 
  ggplot(aes(x = Group, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Group)),
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Density, Group), width = 0),
               fun.data = mean_se, geom = "errorbar") +
  labs(y = "", x = "Seed Mix", title = "(b) Utah Lake") +
  scale_color_manual(values = c("red3", "darkblue", "gray1")) + #change legend labels
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  ylim(0, 2.5)
))
  
a + b
ggsave("diversity_index_both.jpeg")

## Fb 2023 ####
#only want the final cover
fb23_di <- fb23 %>% 
  filter(Date == "2023-09-11") %>% 
  dplyr::select(Block, Plot, Group, Density, PHAU:RUST)

#make all percentages
fb23_di <- mutate_if(fb23_di, is.numeric, ~.*100)

#make a new column with the tub
fb23_di <- fb23_di %>% 
  unite(col = "ID",
        c('Block', 'Plot'))

#name the rows
fb23_di2 <- fb23_di
row.names(fb23_di2) <- fb23_di2$"ID"

#Now try the diversity calculation
fb23_di2 <- dplyr::select(fb23_di2, -c(ID, Group, Density))
div <- diversity(fb23_di2, "shannon")
fb23_di$shannon <- div

#Plot
#change order of phrag presence and also labels
fb23_di$Group <- factor(fb23_di$Group, levels = c(5, 4, 3, 2, 1, 10),
                        labels = c("Annual Forb", "Bulrush", "Grass", "Rush",  
                                   "Perennial Forb", "Control"))
fb23_di$Density <- factor(fb23_di$Density, levels = c("L", "H", "C"),
                        labels = c("Low", "High", "Control"))

fb23_di %>% 
    ggplot(aes(x = Group, y = shannon, color = Density)) +
    stat_summary(aes(group = interaction(Density, Group)),
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Density, Group), width = 0),
                 fun.data = mean_se, geom = "errorbar") +
    labs(y = "Mean Shannon Diversity Index", x = "Seed Mix") +
    scale_color_manual(values = c("red3", "darkblue", "gray1")) + #change legend labels
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "right") +
    ylim(0, 2.5)

ggsave("diversity_index_23.jpeg")

# Flags over time ####
##FB####
fb_flag <- fb %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")
 
fb_flag %>%  
  ggplot(aes(x = Date, y = Count, color = Meas_Num)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Plot)
#it does go up over time, so that's good at least 

##UL####
ul_flag <- ul %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")

ul_flag %>%  
  ggplot(aes(x = Date, y = Count, color = Meas_Num)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Plot)
#goes up and then levels out

##2023####
fb23_flag <- fb23 %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")

fb23_flag %>%  
  ggplot(aes(x = Date, y = Count, color = Meas_Num)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~Plot)
#goes up and then seems to come down right after cutting, so that's all correct 

# Flags with different species ####
##FB####
fb_flag <- fb %>% 
  dplyr::select(Plot, Typha, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            Typha = Typha)

cor.test(fb_flag$Typha, fb_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.53, p < 2.2e16
#good positive, very significant

summary(lm(Typha ~ avg_meas, fb_flag))
#not great R2

fb_flag <- fb %>% 
  dplyr::select(Plot, PHAU, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            PHAU = PHAU)

cor.test(fb_flag$PHAU, fb_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.16, p = 0.002
#weak positive, significant
summary(lm(PHAU ~ avg_meas, fb_flag))
#terrible R2

fb_flag <- fb %>% 
  dplyr::select(Plot, DISP, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            DISP = DISP)

cor.test(fb_flag$DISP, fb_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.35, p = 6.744e-11
#okay positive, very significant

summary(lm(DISP ~ avg_meas, fb_flag))
#pretty bad R2

##UL####
ul_flag <- ul %>% 
  dplyr::select(Plot, PHAU, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            PHAU = PHAU)

cor.test(ul_flag$PHAU, ul_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(454) = 0.24, p = 2.571e-07
#weak positive, significant

summary(lm(PHAU ~ avg_meas, ul_flag))
#pretty bad R2

ul_flag <- ul %>% 
  dplyr::select(Plot, SCAM, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            SCAM = SCAM)

cor.test(ul_flag$SCAM, ul_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(454) = 0.32, p = 4.403e-12
#okay positive, very significant
summary(lm(SCAM ~ avg_meas, ul_flag))
#pretty bad R2

ul_flag <- ul %>% 
  dplyr::select(Plot, Cheno, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            Cheno = Cheno)

cor.test(ul_flag$Cheno, ul_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(454) = 0.73, p  < 2.2e-16
#strong positive, very significant

summary(lm(Cheno ~ avg_meas, ul_flag))
#actually decent R2

##2023####
fb23_flag <- fb23 %>% 
  dplyr::select(Plot, Typha, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            Typha = Typha)

cor.test(fb23_flag$Typha, fb23_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.07, p = 0.197
#very weak, not significant

summary(lm(Typha ~ avg_meas, fb23_flag))
#very bad R2

fb23_flag <- fb23 %>% 
  dplyr::select(Plot, PHAU, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            PHAU = PHAU)

cor.test(fb23_flag$PHAU, fb23_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.36, p = 2.665e-11
#decent positive, very significant

summary(lm(PHAU ~ avg_meas, fb23_flag))
#not great R2

fb23_flag <- fb23 %>% 
  dplyr::select(Plot, DISP, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            DISP = DISP)

cor.test(fb23_flag$DISP, fb23_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.05, p = 0.3897
#weak, not significant

summary(lm(DISP ~ avg_meas, fb23_flag))
#very bad R2

fb23_flag <- fb23 %>% 
  dplyr::select(Plot, SCAC, Date, Measurement.1, Measurement.2, Measurement.3) %>% 
  group_by(Plot, Date) %>% 
  summarize(avg_meas = sum(Measurement.1, Measurement.2, Measurement.3)/3,
            SCAC = SCAC)

cor.test(fb23_flag$SCAC, fb23_flag$avg_meas,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(328) = 0.1, p = 0.06911
#weak, significant

summary(lm(SCAC ~ avg_meas, fb23_flag))
#very bad R2

#Flags with invasive versus native ####
##FB####
fb_flag <- fb %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3, Native.Cover, Invasive.Cover) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")

cor.test(fb_flag$Invasive.Cover, fb_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(988) = 0.35, p < 2.2e-16
#okay positive, very significant 

cor.test(fb_flag$Native.Cover, fb_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(988) = 0.77, p < 2.2e-16
#strong positive, very significant

##UL####
ul_flag <- ul %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3, Native.Cover, Invasive.Cover) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")

cor.test(ul_flag$Invasive.Cover, ul_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(1383) = 0.48, p < 2.2e-16
#decent positive, very significant 

cor.test(ul_flag$Native.Cover, ul_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(1383) = 0.69, p < 2.2e-16
#strong positive, very significant

##2023####
fb23_flag <- fb23 %>% 
  dplyr::select(Block, Plot, Group, Density, Date, Measurement.1, Measurement.2, Measurement.3, Native.Cover, Invasive.Cover) %>% 
  pivot_longer(cols = c(Measurement.1, Measurement.2, Measurement.3), names_to = "Meas_Num", values_to = "Count")

cor.test(fb23_flag$Invasive.Cover, fb23_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(988) = 0.71, p < 2.2e-16
#strong positive, very significant 

cor.test(fb23_flag$Native.Cover, fb23_flag$Count,
         method = "pearson", use = "complete.obs", exact = FALSE, conf.int = TRUE)
#r(988) = 0.19, p = 4.078e-09
#weak positive, very significant

# Wells ####
load("wells.RData")

cp <- c("cadetblue4", "cornflowerblue", "deepskyblue", "deepskyblue4", "cyan3", "darkblue")
##FB ####
a <- wells_fb %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line() +
  labs(x = "Date", y = "Water Depth (cm)", title = "(b) Farmington Bay 2022") +
  ylim(-100, 30) +
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp)

##ul ####
b <- wells_ul %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "Water Depth (cm)", title = "(a) Utah Lake 2022")+
  ylim(-100, 30)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp)

##2023 ####
c <- wells_2023 %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "Water Depth (cm)", title = "(c) Farmington Bay 2023")+
  ylim(-100, 30)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp)

b/a/c + plot_layout(guides = "collect")
ggsave("wells.jpeg")

# RUMA Spread ####
##Year 1 ####
fb$Group <- factor(fb$Group, levels = c(5, 4, 3, 2, 1, 10),
                   labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                              "Perennial forb", "Control"))
fb$Density <- factor(fb$Density, levels = c("H", "L", "C"),
                     labels = c("High", "Low", "Control"))
fb23$Group <- factor(fb23$Group, levels = c(5, 4, 3, 2, 1, 10),
                     labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                                "Perennial forb", "Control"))
fb23$Density <- factor(fb23$Density, levels = c("H", "L", "C"),
                       labels = c("High", "Low", "Control"))
a <- fb %>% 
  ggplot(aes(x = Date, y = RUMA, color = Density)) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "line") +
  stat_summary(aes(group = interaction(Group, Density), width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  facet_grid(~Group) +
  scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(),
        strip.text.x = element_text(size = 6)) +
  labs(x = "Date", y = "*Rumex maritimus* Cover", title = "(a) 2022 Growing Season") +
  coord_cartesian(ylim = c(0, .2))


##Year 2 ####

b <- fb23 %>% 
  ggplot(aes(x = Date, y = RUMA, color = Density)) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "line") +
  stat_summary(aes(group = interaction(Group, Density), width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  facet_grid(~Group) +
  scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(),
        strip.text.x = element_text(size = 6)) +
  labs(x = "Date", y = "*Rumex maritimus* Cover", title = "(b) 2023 Growing Season")+
  coord_cartesian(ylim = c(0, .2))

a / b + plot_layout(guides = "collect")
#you can see how RUMA spread into all the other parts, and also how it senesced and then regrew
ggsave("RUMA_both_years.jpeg")

# SCAC Spread ####
##Year 1 ####
fb$Group <- factor(fb$Group, levels = c(5, 4, 3, 2, 1, 10),
                   labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                              "Perennial forb", "Control"))
fb$Density <- factor(fb$Density, levels = c("H", "L", "C"),
                     labels = c("High", "Low", "Control"))
fb23$Group <- factor(fb23$Group, levels = c(5, 4, 3, 2, 1, 10),
                     labels = c("Annual Forb", "Bulrush", "Grass", "Rush",
                                "Perennial forb", "Control"))
fb23$Density <- factor(fb23$Density, levels = c("H", "L", "C"),
                       labels = c("High", "Low", "Control"))
a <- fb %>% 
  ggplot(aes(x = Date, y = SCAC, color = Density)) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "line") +
  stat_summary(aes(group = interaction(Group, Density), width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  facet_grid(~Group) +
  scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(),
        strip.text.x = element_text(size = 6)) +
  labs(x = "Date", y = "*Schoenoplectus acutus* Cover", title = "(a) 2022 Growing Season") +
  coord_cartesian(ylim = c(0, .2))


##Year 2 ####

b <- fb23 %>% 
  ggplot(aes(x = Date, y = SCAC, color = Density)) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "line") +
  stat_summary(aes(group = interaction(Group, Density), width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  facet_grid(~Group) +
  scale_color_manual(labels = c('High', 'Low', "Control"), values = c("red3",  "darkblue" , "grey1" )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(),
        strip.text.x = element_text(size = 6)) +
  labs(x = "Date", y = "*Schoenoplectus acutus* Cover", title = "(b) 2023 Growing Season")+
  coord_cartesian(ylim = c(0, .2))

a / b + plot_layout(guides = "collect")
#you can see how RUMA spread into all the other parts, and also how it senesced and then regrew
ggsave("SCAC_both_years.jpeg")

