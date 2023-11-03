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
    SPP %in% c("PHAU", "Typha", "RUST", "Tamarisk") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP", "MUAS") & Group == 3 ~ "Seeded",
    SPP %in% c("EUOC", "EUMA") & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "BIFR", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>%  #make a new column for species status
  group_by(Block, Group, Density, Date, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals



graph_data$Group <- factor(graph_data$Group, levels = c(10, 5, 4, 3, 2, 1),
                    labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial forb"))
graph_data$Density <- factor(graph_data$Density, levels = c('C',"H", "L"),
                      labels = c("Control","High", "Low"))

((fb_plot <- graph_data %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(a) Farmington Bay 2022") + 
    scale_color_manual(labels = c('Control', 'High', 'Low'), values = c('#7D7D7D',"red3",  "darkblue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6),
          axis.title = element_text(size = 9)) +
    ylim(0, 1) +
    facet_wrap(~Group) +
    scale_x_date(date_labels = "%b %d")
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
    SPP %in% c("PHAU", "Typha", "RUST", 
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

graph_data2$Group <- factor(graph_data2$Group, levels = c(10, 5, 4, 3, 2, 1),
                           labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                      "Perennial forb"))
graph_data2$Density <- factor(graph_data2$Density, levels = c('C',"H", "L"),
                             labels = c("Control","High", "Low"))

((ul_plot <- graph_data2 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(b) Utah Lake 2022") + 
    scale_color_manual(labels = c('Control', 'High', 'Low'), values = c('#7D7D7D',"red3",  "darkblue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6),
          axis.title = element_text(size = 9)) +
    ylim(0, 1) +
    facet_wrap(~Group)+
    scale_x_date(date_labels = "%b %d")
))



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
    SPP %in% c("PHAU", "Typha", "RUST") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP %in% c("DISP") & Group == 3 ~ "Seeded",
    SPP %in% c("RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>%  #make a new column for species status
  group_by(Block, Density, Group, Date, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals

graph_data23$Group <- factor(graph_data23$Group, levels = c(10, 5, 4, 3, 2, 1),
                            labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                       "Perennial forb"))
graph_data23$Density <- factor(graph_data23$Density, levels = c('C',"H", "L"),
                              labels = c("Control","High", "Low"))


((fb23_plot <- graph_data23 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(c) Farmington Bay 2023") + 
    scale_color_manual(labels = c('Control', 'High', 'Low'), values = c('#7D7D7D',"red3",  "darkblue")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          axis.title = element_text(size = 9)) +
    ylim(0,1) +
    facet_wrap(~Group)+
    scale_x_date(date_labels = "%b %d")
))

fb_plot / ul_plot /fb23_plot + plot_layout(guides = "collect")
ggsave("native_seeded_invasive_cover_all.jpeg", height = 10, width = 8, units = "in")

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
    SPP %in% c("PHAU", "Typha", "RUST", "Tamarisk") ~ "Invasive",
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
       fill = "Species", title = "(a) Farmington Bay 2022") +
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
  labs(x = "", y = "Relative Abundance", 
       fill = "Species", title = "(b) Utah Lake 2022") +
  theme(plot.title = element_text(size = 9),
        legend.position = "none")


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
    SPP %in% c("PHAU", "Typha", "RUST") ~ "Invasive",
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

fb23_stack <- fb232 %>% 
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
       title = "(c) Farmington Bay 2023",
       fill = "Species") +
  theme(plot.title = element_text(size = 9),
        legend.position = "none")

fb_stack / ul_stack / fb23_stack + plot_layout(guides = "collect")
ggsave("stacked_species_all.jpeg", height = 11, width = 8, units = "in")

#Barchart of final native ####
c_labs <- c('BOMA', 'CHEN', 
            'CYER', 'DISP', 'LEFA', 
            'RACY', 'RUMA', 'SAAM', 
            'SARU', 'SCAC', 'SCAM', 'SCPU', 'SYCI')

cp_all <- c("#1F78B4" , 'plum1', 
            'springgreen', "#B2DF8A",  'lightcyan', 
            'paleturquoise', "#FDBF6F", 'khaki1', 
            'orchid4', "#FF7F00", "#CAB2D6", 'olivedrab3', "#6A3D9A")

legend(x = 1, legend = c_labs, fill = "cp_all")

##FB####
cp1 <- c("#1F78B4" , 'plum1', 
             "#B2DF8A",  'lightcyan', 
            'orchid4')

#change names of Cheno
fb2$SPP[fb2$SPP == "Cheno"] <- "CHEN"
a <- fb2 %>% 
  dplyr::filter(Status == "Native", Group == "Control", Percent_Cover > 0) %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = cp1,
                    labels = c( 'BOMA', 'CHEN',
                                'DISP', 'LEFA',
                                'SARU'))+
  labs(x = "", y = "Relative Abundance", 
       fill = "Species", title = "(a) Farmington Bay 2022") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_blank(),
        legend.position = "right",
        axis.ticks.x = element_blank())


##UL####
cp3 <- c( "#1F78B4" , 'plum1', 
            'springgreen', "#B2DF8A",  
            'paleturquoise', "#FDBF6F", 'khaki1', 
            'olivedrab3', "#6A3D9A")

#change names of Cheno
ul2$SPP[ul2$SPP == "Cheno"] <- "CHEN"
b <- ul2 %>% 
  dplyr::filter(Status == "Native", Group == "Control", Percent_Cover>0) %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = cp3,
                    labels = c('BOMA', 'CHEN',
                               'CYER', 'DISP',
                               'RACY', 'RUMA', 'SAAM',
                              'SCPU', 'SYCI'))+
  labs(x = "", y = "", 
       fill = "Species", title = "(b) Utah Lake 2022") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_blank(),
        legend.position="right",
        axis.ticks.x = element_blank())

##FB23####
cp2 <- c( "#1F78B4" , 
            "#B2DF8A",  
             "#FDBF6F", 
             "#FF7F00", "#CAB2D6")
#change names of Cheno
fb232$SPP[fb232$SPP == "Cheno"] <- "CHEN"
c <- fb232 %>% 
  dplyr::filter(Status == "Native", Group == "Control") %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = cp2,
                    labels = c('BOMA', 
                               'DISP', 
                               'RUMA', 
                               'SCAC', 'SCAM'))+
  labs(x = "", y  = "", 
       title = "(c) Farmington Bay 2023",
       fill = "Species") +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_blank(),
        legend.position = "right",
        axis.ticks.x = element_blank())

a+b+c 
ggsave("native_abundance_all.jpeg", height = 8, width = 8, units = "in")

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
fb_di$Group <- factor(fb_di$Group, levels = c(10, 5, 4, 3, 2, 1),
                                   labels = c('Control',"Annual Forb", "Bulrush", "Grass", "Rush",  
                                              "Perennial Forb"))
fb_di$Density <- factor(fb_di$Density, levels = c("C","L", "H"),
                        labels = c("Control","Low", "High"))

((a <- fb_di %>% 
  ggplot(aes(x = Group, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Group)),
                 fun = mean, geom = "point", size = 2,
               position = position_dodge(width = .5)) +
  stat_summary(aes(group = interaction(Density, Group), width = 0),
                 fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = .5)) +
  labs(y = "Mean Shannon Diversity Index", x = "Seed Mix", title = "(a) Farmington Bay 2022") +
  scale_color_manual(values = c('#7D7D7D',"darkblue", "red3")) + #change legend labels
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
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
ul_di$Group <- factor(ul_di$Group, levels = c(10, 5, 4, 3, 2, 1),
                      labels = c('Control',"Annual Forb", "Bulrush", "Grass", "Rush",  
                                 "Perennial Forb"))
ul_di$Density <- factor(ul_di$Density, levels = c("C","L", "H"),
                        labels = c("Control","Low", "High"))

((b <- ul_di %>% 
  ggplot(aes(x = Group, y = shannon, color = Density)) +
  stat_summary(aes(group = interaction(Density, Group)),
               fun = mean, geom = "point", size = 2,
               position = position_dodge(width = .5)) +
  stat_summary(aes(group = interaction(Density, Group), width = 0),
               fun.data = mean_se, geom = "errorbar",
               position = position_dodge(width = .5)) +
  labs(y = "", x = "Seed Mix", title = "(b) Utah Lake 2022") +
    scale_color_manual(values = c('#7D7D7D',"darkblue", "red3")) + #change legend labels
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title = element_text(size = 9),
        legend.position = "none") +
  ylim(0, 2.5)
))
  

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
fb23_di$Group <- factor(fb23_di$Group, levels = c(10, 5, 4, 3, 2, 1),
                      labels = c('Control',"Annual Forb", "Bulrush", "Grass", "Rush",  
                                 "Perennial Forb"))
fb23_di$Density <- factor(fb23_di$Density, levels = c("C","L", "H"),
                        labels = c("Control","Low", "High"))
c <- fb23_di %>% 
    ggplot(aes(x = Group, y = shannon, color = Density)) +
    stat_summary(aes(group = interaction(Density, Group)),
                 fun = mean, geom = "point", size = 2,
                 position = position_dodge(width = .5)) +
    stat_summary(aes(group = interaction(Density, Group), width = 0),
                 fun.data = mean_se, geom = "errorbar",
                 position = position_dodge(width = .5)) +
    labs(y = "", x = "Seed Mix", title= "(c) Farmington Bay 2023") +
  scale_color_manual(values = c('#7D7D7D',"darkblue", "red3")) + #change legend labels
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          axis.title = element_text(size = 9),
          legend.position = "right") +
    ylim(0, 2.5)

a + b + c
ggsave("diversity_all.jpeg", height = 8, width = 8, units = "in")

# Wells ####
load("wells.RData")

cp <- c("cadetblue4", "cornflowerblue", "deepskyblue", "deepskyblue4", "cyan3", "darkblue")
##FB ####
a <- wells_fb %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line() +
  labs(x = "Date", y = "Water Depth (cm)", title = "(a) Farmington Bay 2022") +
  ylim(-100, 30) +
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp) +
  scale_x_date(limits = as.Date(c("2022-06-01", "2022-10-01")))

##ul ####
b <- wells_ul %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "Water Depth (cm)", title = "(b) Utah Lake 2022")+
  ylim(-100, 30)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp) +
  scale_x_date(limits = as.Date(c("2022-06-01", "2022-10-01")))

##2023 ####
c <- wells_2023 %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  labs(x = "Date", y = "Water Depth (cm)", title = "(c) Farmington Bay 2023")+
  ylim(-100, 30)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp)+
  scale_x_date(limits = as.Date(c("2023-06-01", "2023-10-01")))

a/b/c + plot_layout(guides = "collect")
ggsave("wells.jpeg")

# SCAC Spread ####
##Year 1 ####
fb$Group <- factor(fb$Group, levels = c(10, 5, 4, 3, 2, 1),
                   labels = c("Control","Annual Forb", "Bulrush", "Grass", "Rush",
                              "Perennial forb"))
fb$Density <- factor(fb$Density, levels = c("C", "L", "H"),
                     labels = c("Control", "Low", "High"))
fb23$Group <- factor(fb23$Group, levels = c(10, 5, 4, 3, 2, 1),
                     labels = c("Control","Annual Forb", "Bulrush", "Grass", "Rush",
                                "Perennial forb"))
fb23$Density <- factor(fb23$Density, levels = c("C", "L", "H"),
                       labels = c("Control", "Low", "High"))
a <- fb %>% 
  ggplot(aes(x = Date, y = SCAC, color = Density)) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "point", size = 2) +
  stat_summary(aes(group = interaction(Group, Density)), #calculate means of the total cover
               fun = mean, geom = "line") +
  stat_summary(aes(group = interaction(Group, Density), width = 0), #calculate error bars
               fun.data = mean_se, geom = "errorbar", size = .5) +
  facet_grid(~Group) +
  scale_color_manual(values = c("#7D7D7D", "darkblue", "red3")) +
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
  scale_color_manual(values = c("#7D7D7D", "darkblue", "red3")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
        plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(),
        strip.text.x = element_text(size = 6)) +
  labs(x = "Date", y = "*Schoenoplectus acutus* Cover", title = "(b) 2023 Growing Season")+
  coord_cartesian(ylim = c(0, .2))

a / b + plot_layout(guides = "collect")
#you can see how RUMA spread into all the other parts, and also how it senesced and then regrew
ggsave("SCAC_both_years.jpeg")
