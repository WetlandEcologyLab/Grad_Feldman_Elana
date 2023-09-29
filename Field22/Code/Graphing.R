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
    SPP %in% c("SYCI", "BICE", "BIFR", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>%  #make a new column for species status
  group_by(Block, Plot, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals

graph_data <- graph_data %>%#change the names so they make sense
  mutate(Plot = case_when(
    Plot == "1H" ~ "Perennial forbs (high)",
    Plot == "1L" ~ "Perennial forbs (low)",
    Plot == "2H" ~ "Rushes (high)",
    Plot == "2L" ~ "Rushes (low)",
    Plot == "3H" ~ "Grasses (high)",
    Plot == "3L" ~ "Grasses (low)",
    Plot == "4H" ~ "Bulrushes (high)",
    Plot == "4L" ~ "Bulrushes (low)",
    Plot == "5H" ~ "Annual forbs (high)",
    Plot == "5L" ~ "Annual forbs (low)",
    Plot == "C" ~ "Control",
    TRUE ~ "PROBLEM"
  ))

((fb_plot <- graph_data %>%
  mutate(Plot = factor(Plot, #set the order I want
                       levels = c("Annual forbs (high)", 
                                  "Annual forbs (low)", "Perennial forbs (high)",
                                  "Perennial forbs (low)", "Rushes (high)",
                                  "Rushes (low)", "Grasses (high)", 
                                  "Grasses (low)", "Bulrushes (high)",
                                  "Bulrushes (low)", "Control"))) %>% 
  ggplot(aes(x = Plot, y = PC, color = Status, shape = Status)) + #x is plot, y is cover
  stat_summary(aes(group = Status), #calculate means of the total cover
               fun = mean, geom = "point", size = 1) +
  stat_summary(aes(group = Status, width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  labs(x = "Functional Group", y = "Final Proportional Cover", title = "(a) Farmington Bay") + #labels
  scale_color_manual(labels = c('Invasive', 'Native', "Seeded"), values = c("red3",  "darkblue" , "grey1" )) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9)) +
    ylim(0, 1)
))

## UL####
graph_data2 <- ul%>%
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
  ))%>% 
  group_by(Block, Plot, Status) %>%
  summarise(PC = sum(Percent_Cover, na.rm = TRUE))

graph_data2 <- graph_data2 %>% 
  mutate(Plot = case_when(
    Plot == "1H" ~ "Perennial forbs (high)",
    Plot == "1L" ~ "Perennial forbs (low)",
    Plot == "2H" ~ "Rushes (high)",
    Plot == "2L" ~ "Rushes (low)",
    Plot == "3H" ~ "Grasses (high)",
    Plot == "3L" ~ "Grasses (low)",
    Plot == "4H" ~ "Bulrushes (high)",
    Plot == "4L" ~ "Bulrushes (low)",
    Plot == "5H" ~ "Annual forbs (high)",
    Plot == "5L" ~ "Annual forbs (low)",
    Plot == "C" ~ "Control",
    TRUE ~ "PROBLEM"
  ))

((ul_plot <- graph_data2 %>%
    mutate(Plot = factor(Plot, #set the order I want
                         levels = c("Annual forbs (high)", 
                                    "Annual forbs (low)", "Perennial forbs (high)",
                                    "Perennial forbs (low)", "Rushes (high)",
                                    "Rushes (low)", "Grasses (high)", 
                                    "Grasses (low)", "Bulrushes (high)",
                                    "Bulrushes (low)", "Control"))) %>% 
    ggplot(aes(x = Plot, y = PC, color = Status, shape= Status)) + #x is plot, y is cover
    stat_summary(aes(group = Status), #calculate means of the total cover
                 fun = mean, geom = "point", size = 1) +
    stat_summary(aes(group = Status, width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Functional Group", y = "", title = "(b) Utah Lake") + #labels
    scale_color_manual(labels = c('Invasive', 'Native', "Seeded"), values = c("red3",  "darkblue" , "grey1" )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9)) +
    ylim(0, 1)
  
))

fb_plot + ul_plot + plot_layout(guides = "collect")
ggsave("native_seeded_invasive_cover.jpeg")

##2023 FB ####
graph_data23 <-fb23 %>%
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
  ))%>%  #make a new column for species status
  group_by(Block, Plot, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals

graph_data23 <- graph_data23 %>%#change the names so they make sense
  mutate(Plot = case_when(
    Plot == "1H" ~ "Perennial forbs (high)",
    Plot == "1L" ~ "Perennial forbs (low)",
    Plot == "2H" ~ "Rushes (high)",
    Plot == "2L" ~ "Rushes (low)",
    Plot == "3H" ~ "Grasses (high)",
    Plot == "3L" ~ "Grasses (low)",
    Plot == "4H" ~ "Bulrushes (high)",
    Plot == "4L" ~ "Bulrushes (low)",
    Plot == "5H" ~ "Annual forbs (high)",
    Plot == "5L" ~ "Annual forbs (low)",
    Plot == "C" ~ "Control",
    TRUE ~ "PROBLEM"
  ))

((fb23_plot <- graph_data23 %>%
    mutate(Plot = factor(Plot, #set the order I want
                         levels = c("Annual forbs (high)", 
                                    "Annual forbs (low)", "Perennial forbs (high)",
                                    "Perennial forbs (low)", "Rushes (high)",
                                    "Rushes (low)", "Grasses (high)", 
                                    "Grasses (low)", "Bulrushes (high)",
                                    "Bulrushes (low)", "Control"))) %>% 
    ggplot(aes(x = Plot, y = PC, color = Status, shape = Status)) + #x is plot, y is cover
    stat_summary(aes(group = Status), #calculate means of the total cover
                 fun = mean, geom = "point", size = 1) +
    stat_summary(aes(group = Status, width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Functional Group", y = "Final Proportional Cover") + #labels
    scale_color_manual(labels = c('Invasive', 'Native', "Seeded"), values = c("red3",  "darkblue" , "grey1" )) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9)) +
    ylim(0, 1)
))

ggsave("invasive_native_2023.jpeg")

#Barchart of seeded ####

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
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
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
  labs(x = "Native seeding density", y = "", 
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
  labs(x = "Native seeding density", y = "Relative Abundance", 
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

#Plot ####
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
  labs(y = "Mean Shannon Diversity Index", x = "Seed mix", title = "(a) Farmington Bay") +
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

#Plot ####
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
  labs(y = "", x = "Seed mix", title = "(b) Utah Lake") +
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

#Plot ####
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
    labs(y = "Mean Shannon Diversity Index", x = "Seed mix") +
    scale_color_manual(values = c("red3", "darkblue", "gray1")) + #change legend labels
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          legend.position = "blank") +
    ylim(0, 2.5)

ggsave("diversity_index_23.jpeg")
