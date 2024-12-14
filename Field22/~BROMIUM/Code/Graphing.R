#Load data and packages
#All package version saved in renv.lock 
#renv::init, renv::restore
load("clean_dfs.RData")
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library(vegan)
library(gridExtra)
library(ggpattern)

#Graphs of invasive versus native versus seeded cover####
##Farmington Bay 2022####
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
  ))%>%  #make a new column for species status (native, invasive, seeded)
  group_by(Block, Group, Density, Date, Status) %>% #group by the plot and species status
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #calculate totals


#refactor to help with the graphing
graph_data$Group <- factor(graph_data$Group, levels = c(10, 5, 4, 3, 2, 1),
                           labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                      "Perennial forb"))
graph_data$Density <- factor(graph_data$Density, levels = c('C',"L", "H"),
                             labels = c("Control","Low", "High"))

((fb_plot <- graph_data %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(a) Farmington Bay 2022") + 
    scale_color_manual(labels = c('Control', 'Low', 'High'), values = c('#7D7D7D',"darkblue", "red3")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9), #angle the x axis text
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6), #change size of facet wrap titles
          axis.title = element_text(size = 9)) +
    ylim(0, 1) +
    facet_wrap(~Group) +
    scale_x_date(date_labels = "%b %d") #make the dates month/day
))


##Utah Lake 2022 ####
graph_data2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
                Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
                EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>% #remove unnecessary columns
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>%  #pivot so that all species names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "Typha", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  ))%>% #make a new column for species status (native, invasive, seeded)
  group_by(Block, Date, Density, Group, Status) %>%
  summarise(PC = sum(Percent_Cover, na.rm = TRUE)) #get the sum of the cover

#refactor everything to help with graphing
graph_data2$Group <- factor(graph_data2$Group, levels = c(10, 5, 4, 3, 2, 1),
                            labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                       "Perennial forb"))
graph_data2$Density <- factor(graph_data2$Density, levels = c('C',"L", "H"),
                              labels = c("Control", "Low", "High"))

((ul_plot <- graph_data2 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(c) Utah Lake 2022") + 
    scale_color_manual(labels = c('Control', 'Low', 'High'), values = c('#7D7D7D',"darkblue", "red3")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9), #angle the x axis text
          plot.title = element_text(size = 9),
          strip.text.x = element_text(size = 6), #change size of the facet wrap titles
          axis.title = element_text(size = 9)) +
    ylim(0, 1) +
    facet_wrap(~Group)+
    scale_x_date(date_labels = "%b %d") #change date to month/day
))


##Farmington Bay 2023 ####
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

#refactor to help with graphing
graph_data23$Group <- factor(graph_data23$Group, levels = c(10, 5, 4, 3, 2, 1),
                             labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                        "Perennial forb"))
graph_data23$Density <- factor(graph_data23$Density, levels = c('C',"L", 'H'),
                               labels = c("Control","Low", "High"))


((fb23_plot <- graph_data23 %>%
    ggplot(aes(x = Date, y = PC, color = Density, shape = Status)) + 
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "point", size = 2) +
    stat_summary(aes(group = interaction(Group, Density, Status)), #calculate means of the total cover
                 fun = mean, geom = "line") +
    stat_summary(aes(group = interaction(Group, Density, Status), width = 0), #calculate error bars
                 fun.data = mean_se, geom = "errorbar", size = .5) +
    labs(x = "Date", y = "Proportional Cover", title = "(b) Farmington Bay 2023") + 
    scale_color_manual(labels = c('Control','Low','High'), values = c('#7D7D7D', "darkblue","red3")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.9),
          plot.title = element_text(size = 9),
          axis.title = element_text(size = 9)) +
    ylim(0,1) +
    facet_wrap(~Group)+
    scale_x_date(date_labels = "%b %d") #change date to month/day
))

fb_plot /fb23_plot / ul_plot + plot_layout(guides = "collect")

#Stacked species barchart####

##Farmington Bay 2022####
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
  )) #label species as invasive, native, or seeded

#change BIFR to BICE because I am combining them
fb2$SPP[fb2$SPP == "BIFR"] <- "BICE"

fb2$SPP[fb2$SPP == "Cheno"] <- "CHEN" #change name to species code

#refactor to help with graphing
fb2$Group <- factor(fb2$Group, levels = c(10, 5, 4, 3, 2, 1),
                    labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial Forb"))


fb2$Density <- factor(fb2$Density, levels = c("C", "L", "H"),
                      labels = c("Control","Low", "High"))

cp <- c("#A6CEE3", "#1F78B4" ,'plum1', "#B2DF8A", "#33A02C", "#FB9A99" ,'lightcyan', 
        "#FDBF6F" ,'orchid4',"#FF7F00", "#CAB2D6","#6A3D9A") #colors to use in the graph

fb_stack <- (fb2 %>% 
               dplyr::filter(Status == "Seeded" | 
                               (Status == "Native" & Plot == "C"),
                             Percent_Cover >0) %>% #select species that are relevant
               ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
               geom_bar(position = "fill", stat = "identity") +
               facet_grid(~Group, scale = "free", space = "free") + #only show x categories relevant to the facet
               scale_fill_manual(values = cp,
                                 labels = c('BICE',
                                            'BOMA',
                                            'CHEN',
                                            'DISP',
                                            'EUMA',
                                            'EUOC',
                                            'LEFA',
                                            'RUMA',
                                            'SARU',
                                            'SCAC',
                                            'SCAM',
                                            'SYCI'))+ #use the manual colors I specified above
               labs(x = "Native Seeding Density", y = "Relative Abundance", 
                    fill = "Species", title = "(a) Farmington Bay 2022") +
               theme(plot.title = element_text(size = 9),
                     legend.position = "none"))

##Utah Lake 2022####

ul2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
                Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
                EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>% #only select needed columns
  filter(Date == "2022-09-16") %>% #only need the last date
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native" #label species by status
  ))

#refactor to help with graphing
ul2$Group <- factor(ul2$Group, levels = c(10, 5, 4, 3, 2, 1),
                    labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial Forb"))
ul2$Density <- factor(ul2$Density, levels = c("C", "L", "H"),
                      labels = c("Control", "Low", "High"))

#change names of Cheno to be a species code
ul2$SPP[ul2$SPP == "Cheno"] <- "CHEN"


cp2 <- c("#A6CEE3", "#1F78B4" ,'plum1', 'springgreen',"#B2DF8A",  "#FB9A99" ,
         'paleturquoise',  "#FDBF6F" ,'khaki1', "#FF7F00", "#CAB2D6", 'olivedrab3', "#6A3D9A") #colors to use in the graph


ul_stack <- ul2 %>% 
  dplyr::filter(Status == "Seeded" | 
                  (Status == "Native" & Plot == "C"),
                Percent_Cover >0) %>% #select species that are relevant
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~Group, scale = "free", space = "free") +
  scale_fill_manual(values = cp2,
                    labels = c("BICE",
                               "BOMA",
                               'CHEN',
                               'CYER',
                               'DISP',
                               'EUOC',
                               'RACY',
                               'RUMA',
                               'SAAM',
                               'SCAC',
                               'SCAM',
                               'SCPU',
                               'SYCI'))+ #manually add colors specified above
  labs(x = "Native Seeding Density", y = "Relative Abundance", 
       fill = "Species", title = "(c) Utah Lake 2022") +
  theme(plot.title = element_text(size = 9),
        legend.position = "none")



##Farmington Bay 2023####
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
  )) #label species by status (native, invasive, seeded)

#need to add some 0s back in so that we can plot perennial forbs as a blank
fb232 <- fb232 %>% 
  dplyr::filter(Status == "Seeded" | 
                  (Status == "Native" & Plot == "C") |
                  (SPP == "RUMA" & Plot =="1H") | #select some perennial forbs
                  (SPP == "RUMA" & Plot =="1L"),
                Percent_Cover >0) 

fb232 <- fb232 %>% 
  mutate(Percent_Cover = case_when(
    SPP == "RUMA" & Plot == "1H" ~  0, #make sure the cover is 0 for all perennial forbs
    SPP == "RUMA" & Plot == "1L" ~  0,
    TRUE ~ Percent_Cover
  ))

#refactor to help with graphing
fb232$Group <- factor(fb232$Group, levels = c(10, 5, 4, 3, 2, 1),
                      labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                 "Perennial Forb"))
fb232$Density <- factor(fb232$Density, levels = c("C","L", "H"),
                        labels = c('Control',"Low", "High"))

fb232$SPP[fb232$SPP == "Cheno"] <- "CHEN"#change name to species code
cp3 <- c("#1F78B4" ,"#B2DF8A", "#FDBF6F" ,"#FF7F00", "#CAB2D6") #colors to use in graph



fb23_stack <- fb232 %>% 
  ggplot(aes(fill = SPP, y = Percent_Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  facet_grid(~Group, scale = "free", space = "free") +
  scale_fill_manual(values = cp3,
                    labels = c("BOMA",
                               "DISP",
                               'RUMA',
                               'SCAC',
                               'SCAM'))+ #manually use colors specified above
  labs(x = "Native Seeding Density", y  = "Relative Abundance", 
       title = "(b) Farmington Bay 2023",
       fill = "Species") +
  theme(plot.title = element_text(size = 9),
        legend.position = "none")


fb_stack/ fb23_stack/ ul_stack + plot_layout(guides = "collect")
#ggsave("stacked_barchart.jpeg", width = 5, height = 8, unit = "in", dpi = 600)

##Make a legend for all the graphs ####
#Make an arbitrary graph using all the names and colors
Species <- c("BICE", 'BOMA', 'CHEN', 'CYER',
             'DISP', 'EUMA', 'EUOC', 'LEFA',
             'RACY', 'RUMA', 'SAAM', 'SARU',
             'SCAC', 'SCAM', 'SCPU', 'SYCI')
values <- c(rep(1, 16))

legend_data <- data.frame(Species, values)
cp6 <- c("#A6CEE3", "#1F78B4" ,'plum1', 'springgreen',"#B2DF8A", "#33A02C", "#FB9A99" ,'lightcyan',
         'paleturquoise',  "#FDBF6F" ,'khaki1',  'orchid4',"#FF7F00", "#CAB2D6", 'olivedrab3', "#6A3D9A") #colors to use in the graph

legend_graph <- legend_data %>% 
  ggplot(aes(fill = Species, y = values, x = Species)) +
  geom_bar(position = "fill", stat = "identity") +
  scale_fill_manual(values = cp6,
                    labels = c("BICE",
                               "BOMA",
                               'CHEN',
                               'CYER',
                               'DISP',
                               'EUMA',
                               'EUOC',
                               'LEFA',
                               'RACY',
                               'RUMA',
                               'SAAM',
                               'SARU',
                               'SCAC',
                               'SCAM',
                               'SCPU',
                               'SYCI'))
#ggsave("legend.jpeg", dpi = 700)
#save this graph, cut out the legend, and add it to the other graph in Canva

#Stacked species barchart####

##Farmington Bay 2022####
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
    SPP %in% c("SYCI", "BICE", "BIFR", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native"
  )) #label species as invasive, native, or seeded

#change BIFR to BICE because I am combining them
fb2$SPP[fb2$SPP == "BIFR"] <- "BICE"

fb2$SPP[fb2$SPP == "Cheno"] <- "CHEN" #change name to species code

#calculate the means of the cover by group
fb2 <- fb2 %>% group_by(Plot, Group, Density, SPP, Status) %>% 
  summarize(avg = mean(Percent_Cover))

#refactor to help with graphing
fb2$Group <- factor(fb2$Group, levels = c(10, 5, 4, 3, 2, 1),
                    labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial Forb"))


fb2$Density <- factor(fb2$Density, levels = c("C", "L", "H"),
                      labels = c("Control","Low", "High"))

fb2$SPP <- factor(fb2$SPP, levels = c('BICE',
                                      'BOMA',
                                      'DISP',
                                      'EUMA',
                                      'EUOC',
                                      'RUMA',
                                      'SCAC',
                                      'SCAM',
                                      'SYCI',
                                      'CHEN',
                                      'LEFA',
                                      'SARU',
                                      'PHAU',
                                      'Typha',
                                      'RUST',
                                      'Tamarisk'))
fb2$Status <- factor(fb2$Status, levels = c("Seeded", "Native", "Invasive"))


cp <- c('#009E73',
        'darkseagreen4',
        'chartreuse3',
        'green4',
        'darkolivegreen3',
        'darkseagreen3',
        'mediumspringgreen',
        'green3',
        'olivedrab',
        'dodgerblue4',
        'darkblue',
        'royalblue',
        '#D55E00',
        'orange',
        'wheat3',
        'goldenrod4') #colors to use in the graph

pat <- c('stripe',
         'none',
         'none') #patterns to use in graph

((fb_stack <- (fb2 %>% 
                 dplyr::filter(avg >0) %>% #select species that are relevant
                 ggplot(aes(fill = SPP, y = avg, x = Density)) +
                 geom_bar(position = 'stack', stat = 'summary', fun = 'mean') +
                 geom_col_pattern(aes(pattern = Status)) +
                 scale_pattern_manual(values = pat)+
                 facet_grid(~Group, scale = "free", space = "free") + #only show x categories relevant to the facet
                 scale_fill_manual(values = cp,
                                   labels = c('BICE',
                                              'BOMA',
                                              'DISP',
                                              'EUMA',
                                              'EUOC',
                                              'RUMA',
                                              'SCAC',
                                              'SCAM',
                                              'SYCI',
                                              'CHEN',
                                              'LEFA',
                                              'SARU',
                                              'PHAU',
                                              'Typha',
                                              'RUST',
                                              'Tamarisk'))+ #use the manual colors I specified above
                 labs(x = "Native Seeding Density", y = "Cumulative Proportional Cover",
                      fill = "Species", title = "(a) Farmington Bay 2022") +
                 theme(plot.title = element_text(size = 9),
                       legend.position = "none")) +
    ylim(0, 1)
))

##Utah Lake 2022####

ul2 <- ul%>%
  dplyr::select(Block, Plot, Group, Density, Date, PHAU, BOMA, BICE, CYER, RUMA,
                Cheno, SCAC, SCPU, SCAM, DISP, RACY, ASIN, ALPR, CYDA, Unk_Bulrush, BY, SYCI,
                EUOC, TYPHA, Tamarisk, POPE, POFR, SAAM, BASC, LASE) %>% #only select needed columns
  filter(Date == "2022-09-16") %>% #only need the last date
  pivot_longer(
    cols = 6:30, 
    names_to = "SPP",
    values_to = "Percent_Cover"
  ) %>% #pivot so that all names are in one column
  mutate(Status = case_when(
    SPP %in% c("PHAU", "TYPHA", "RUST", 
               "Tamarisk", "ALPR", "CYDA", "BY", 
               "BASC", "LASE") ~ "Invasive",
    SPP %in% c("BOMA", "SCAC", "SCAM") & Group == 4 ~ "Seeded",
    SPP == "DISP" & Group == 3 ~ "Seeded",
    SPP == "EUOC" & Group == 1 ~ "Seeded",
    SPP %in% c("SYCI", "BICE", "RUMA") & Group == 5 ~ "Seeded",
    TRUE ~ "Native" #label species by status
  ))

#calculate the means of the cover by group
ul2 <- ul2 %>% group_by(Plot, Group, Density, SPP, Status) %>% 
  summarize(avg = mean(Percent_Cover))

#refactor to help with graphing
ul2$Group <- factor(ul2$Group, levels = c(10, 5, 4, 3, 2, 1),
                    labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                               "Perennial Forb"))
ul2$Density <- factor(ul2$Density, levels = c("C", "L", "H"),
                      labels = c("Control", "Low", "High"))

#change names of Cheno to be a species code
ul2$SPP[ul2$SPP == "Cheno"] <- "CHEN"


ul2$SPP <- factor(ul2$SPP, levels = c('BICE',
                                      'BOMA',
                                      'DISP',
                                      'EUOC',
                                      'RUMA',
                                      'SCAC',
                                      'SCAM',
                                      'SYCI',
                                      'ASIN',
                                      'CHEN',
                                      'CYER',
                                      'POFR',
                                      'POPE',
                                      'RACY',
                                      'SAAM',
                                      'SCPU',
                                      'ALPR',
                                      'BASC',
                                      'BY',
                                      'CYDA',
                                      'LASE',
                                      'PHAU',
                                      'Tamarisk'))
ul2$Status <- factor(ul2$Status, levels = c("Seeded", "Native", "Invasive"))


cp2 <- c('#009E73',
         'darkseagreen4',
         'chartreuse3',
         'darkolivegreen3',
         'darkseagreen3',
         'mediumspringgreen',
         'green3',
         'olivedrab',
         'cadetblue4',
         'dodgerblue4',
         'lightskyblue2',
         '#0072B2',
         'azure',
         'mediumturquoise',
         'skyblue',
         'paleturquoise1',
         'tomato',
         'sienna1',
         'coral2',
         'tan2',
         'brown',
         '#D55E00',
         'goldenrod4') #colors to use in the graph

pat <- c('stripe',
         'none',
         'none') #patterns to use in graph
((ul_stack <- ul2 %>% 
    dplyr::filter(avg > 0) %>% #select species that are relevant
    ggplot(aes(fill = SPP, y = avg, x = Density)) +
    geom_bar(position = 'stack', stat = 'summary', fun = 'mean') +
    geom_col_pattern(aes(pattern = Status)) +
    scale_pattern_manual(values = pat)+
    facet_grid(~Group, scale = "free", space = "free") +
    scale_fill_manual(values = cp2,
                      labels = c('BICE',
                                 'BOMA',
                                 'DISP',
                                 'EUOC',
                                 'RUMA',
                                 'SCAC',
                                 'SCAM',
                                 'SYCI',
                                 'ASIN',
                                 'CHEN',
                                 'CYER',
                                 'POFR',
                                 'POPE',
                                 'RACY',
                                 'SAAM',
                                 'SCPU',
                                 'ALPR',
                                 'BASC',
                                 'BY',
                                 'CYDA',
                                 'LASE',
                                 'PHAU',
                                 'Tamarisk'))+ #manually add colors specified above
    labs(x = "Native Seeding Density", y = "Cumulative Proportional Cover", 
         fill = "Species", title = "(c) Utah Lake 2022") +
    theme(plot.title = element_text(size = 9),
          legend.position = "none") +
    ylim(0,1)
))


##Farmington Bay 2023####
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
  )) #label species by status (native, invasive, seeded)

#calculate the means of the cover by group
fb232 <- fb232 %>% group_by(Plot, Group, Density, SPP, Status) %>% 
  summarize(avg = mean(Percent_Cover))

#refactor to help with graphing
fb232$Group <- factor(fb232$Group, levels = c(10, 5, 4, 3, 2, 1),
                      labels = c("Control", "Annual Forb", "Bulrush", "Grass", "Rush",
                                 "Perennial Forb"))
fb232$Density <- factor(fb232$Density, levels = c("C","L", "H"),
                        labels = c('Control',"Low", "High"))

fb232$SPP[fb232$SPP == "Cheno"] <- "CHEN"#change name to species code

fb232$SPP <- factor(fb232$SPP, levels = c('BOMA',
                                          'DISP',
                                          'RUMA',
                                          'SCAC',
                                          'SCAM',
                                          'PHAU',
                                          'Typha',
                                          'RUST'))
fb232$Status <- factor(fb232$Status, levels = c("Seeded", "Native", "Invasive"))


cp3 <- c('darkseagreen4',
         'chartreuse3',
         'darkseagreen3',
         'mediumspringgreen',
         'green3',
         '#D55E00',
         'orange',
         'wheat3') #colors to use in the graph

pat <- c('stripe',
         'none',
         'none') #patterns to use in graph

((fb23_stack <- fb232 %>% 
    ggplot(aes(fill = SPP, y = avg, x = Density)) +
    geom_bar(position = 'stack', stat = 'summary', fun = 'mean') +
    geom_col_pattern(aes(pattern = Status)) +
    scale_pattern_manual(values = pat)+
    facet_grid(~Group, scale = "free", space = "free") +
    scale_fill_manual(values = cp3,
                      labels = c('BOMA',
                                 'DISP',
                                 'RUMA',
                                 'SCAC',
                                 'SCAM',
                                 'PHAU',
                                 'Typha',
                                 'RUST'))+ #manually use colors specified above
    labs(x = "Native Seeding Density", y  = "Cumulative Proportional Cover", 
         title = "(b) Farmington Bay 2023",
         fill = "Species") +
    theme(plot.title = element_text(size = 9),
          legend.position = "none") +
    ylim(0, 1)
))

all <- fb_stack/ fb23_stack/ ul_stack + plot_layout(guides = "collect")
ggsave("stacked_barchart.jpeg", width = 6, height = 8, unit = "in", dpi = 600)

## Legend ####
#make a graph of seeded species to use the legend
Species <- c("BICE", 'BOMA', 'DISP', 'EUMA', 'EUOC', 
             'RUMA', 'SCAC', 'SCAM','SYCI')
values <- c(rep(1, 9))

legend_data <- data.frame(Species, values)
cp6 <- c('#009E73','darkseagreen4','chartreuse3','green4','darkolivegreen3',
         'darkseagreen3','mediumspringgreen','green3','olivedrab') #colors to use in the graph

((seeded <- legend_data %>% 
    ggplot(aes(fill = Species, y = values, x = Species)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = cp6,
                      labels = c("*B. cernua*", '*B. maritimus*', '*D. spicata*', 
                                 '*E. maculatum*', '*E. occidentalis*', 
                                 '*R. maritimus*', '*S. acutus*', 
                                 '*S. americanus*','*S. ciliatum*'))+
    guides(fill=guide_legend(title = "Seeded Species"))+
    theme(legend.text = ggtext::element_markdown())
))

ggsave("Slegend.jpeg", dpi = 700)
#save this graph, cut out the legend, and add it to the other graph in Canva

#make a graph of native species to use the legend
Species <- c('ASIN', 'CHEN', 'CYER', 'LEFA', 'POFR', 'POPE',
             'RACY', 'SAAM', 'SARU', 'SCPU')
values <- c(rep(1, 10))

legend_data <- data.frame(Species, values)
cp6 <- c('cadetblue4', 'dodgerblue4', 'lightskyblue2', 'darkblue',
         '#0072B2', 'azure', 'mediumturquoise', 'skyblue',
         'royalblue', 'paleturquoise1') #colors to use in the graph

((native <- legend_data %>% 
    ggplot(aes(fill = Species, y = values, x = Species)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = cp6,
                      labels = c('*A. incarnata*', "*Chenopodium* spp.", 
                                 '*C. erythrorhizos*', 
                                 '*L. fusca*', '*P. fremontii*', '*P. persicaria*',
                                 '*R. cymbalaria*', '*S. amygdaloides*', 
                                 '*S. rubra*', '*S. pungens*'))+
    guides(fill=guide_legend(title = "Native Species"))+
    theme(legend.text = ggtext::element_markdown())
))

ggsave("Nlegend.jpeg", dpi = 700)
#save this graph, cut out the legend, and add it to the other graph in Canva

#make a graph of invasive species to use the legend
Species <- c("ALPR", 'BASC', 'BY', 'CYDA', 'LASE', 
             'PHAU', 'RUST', 'Tamarisk','TYPHA')
values <- c(rep(1, 9))

legend_data <- data.frame(Species, values)
cp6 <- c('tomato', 'sienna1', 'tan2', 'coral2',
         'brown', '#D55E00', 'wheat3', 'goldenrod4', 'orange'
      ) #colors to use in the graph

((invasive <- legend_data %>% 
    ggplot(aes(fill = Species, y = values, x = Species)) +
    geom_bar(position = "fill", stat = "identity") +
    scale_fill_manual(values = cp6,
                      labels = c('*A. pratensis*', "*B. scoparia*", 
                                 '*C. dactylon*', 
                                 '*E. crus-galli*', '*L. serriola*', 
                                 '*P. australis*', '*R. stenophyllus*', 
                                 '*Tamarix* spp.', '*Typha* spp.'))+
    guides(fill=guide_legend(title = "Invasive Species"))+
    theme(legend.text = ggtext::element_markdown())
))

ggsave("Ilegend.jpeg", dpi = 700)
#save this graph, cut out the legend, and add it to the other graph in Canva

#status legend
Status <- c("Seeded in Plot", rep('Not Seeded in Plot', 8))
values <- c(rep(1, 9))

legend_data <- data.frame(Status, values)

legend_data$Status <- factor(legend_data$Status,
                             levels = c("Seeded in Plot",
                                        "Not Seeded in Plot"))

pat <- c("stripe", 'none')

((status <- legend_data %>% 
    ggplot(aes(fill = Status, y = values, x = Status)) +
    geom_bar(position = "fill", stat = "identity") +
    geom_col_pattern(aes(pattern = Status))+
    scale_pattern_manual(values = pat,
                         labels = c("Seeded in Plot",
                                    'Not Seeded in Plot'))+
    guides(pattern=guide_legend(title = "Seed Input"))+
    theme(legend.text = ggtext::element_markdown())
))

ggsave("STlegend.jpeg", dpi = 700)
#save this graph, cut out the legend, and add it to the other graph in Canva

# Graph of Shannon Diversity Index####
##Farmington Bay 2022####
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

##Utah Lake 2022####
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


## Farmington Bay 2023 ####
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

# Graphs of Water Depth ####
load("wells.RData")

cp <- c("cadetblue4", "cornflowerblue", "deepskyblue", "deepskyblue4", "cyan3", "darkblue") #colors to use
##Farmington Bay 2022 ####
a <- wells_fb %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line() +
  geom_hline(yintercept = 0)+
  labs(x = "Date", y = "Water Depth (cm)", title = "(a) Farmington Bay 2022") +
  ylim(-100, 50) +
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp) + #use specified colors
  scale_x_date(limits = as.Date(c("2022-06-01", "2022-10-01"))) #use date limits so axes line up

##Utah Lake 2022####
c <- wells_ul %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  labs(x = "Date", y = "Water Depth (cm)", title = "(c) Utah Lake 2022")+
  ylim(-100, 50)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp) + #use specified colors
  scale_x_date(limits = as.Date(c("2022-06-01", "2022-10-01"))) #use date limits so axes line up

##Farmington Bay 2023 ####
b <- wells_2023 %>% 
  ggplot(aes(x = Date, y = depth_cm, color = Block))+
  geom_point()+
  geom_line()+
  geom_hline(yintercept = 0)+
  labs(x = "Date", y = "Water Depth (cm)", title = "(b) Farmington Bay 2023")+
  ylim(-100, 50)+
  theme(plot.title = element_text(size = 9)) +
  scale_color_manual(values = cp)+ #use specified colors
  scale_x_date(limits = as.Date(c("2023-06-01", "2023-10-01"))) #use date limits so axes line up

a/b/c + plot_layout(guides = "collect")
#ggsave("Water.jpeg", height = 8, width = 6, unit = "in", dpi = 600)

# Graph of SCAC Growth Over Time ####
##Farmington Bay 2022####
#refactor everything to help with graphing
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


##Farmington Bay 2023####

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
