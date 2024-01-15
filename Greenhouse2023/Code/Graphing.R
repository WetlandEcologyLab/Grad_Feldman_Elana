#Import data and packages
#All package version saved in renv.lock 
#renv::init, renv::restore
load("main_dfs.RData")
library(tidyverse)
library(viridis)
library(patchwork)
library(vegan)

#Stacked bargraphs of cover, biomass, functional group, and species####
#only need the last date of sampling
gh_final <- cover_dat%>%
  filter(cover_dat$Date == "2023-03-01") 

##Cover data####
###By functional group####
colors <- c("#C77CFF", "#7CAE00", "#00BFC4", "#F8766D") #colors to use in graph

gh_totals <- gh_final %>%
  filter(Mix != "PHAU") %>% #remove the phragmites control
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE), #add up totals by functional group
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = Phrag)

gh_tw <- gh_totals %>% #pivot so all the functional groups are in one column
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

####Phragmites present####
a <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal", "PHAU"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #refactor the order of everything for graph
  filter(Phrag_Presence != "WO") %>% #only want phrag present
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "", fill = "Group", 
       title = "*P.australis* Present")+ #make phrag italics
  facet_grid(~Mix) +
  scale_fill_manual(values = colors)+ #add in the specified colors
  theme(plot.title = ggtext::element_markdown(size = 9), #allows me to use the italics
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 10),
        legend.position = "right",
        axis.text.x = element_text(angle = 45, hjust = 0.9), #angle the x axis
        axis.title.y = element_text(size = 9))


####Phragmites absent####
b <- gh_tw %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #reorder everything for graph
  filter(Phrag_Presence != "W") %>% #only want phrag absent
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Group", 
       title = "(a) *P.australis* Absent")+
  facet_grid(~Mix) +
  scale_fill_manual(values = colors)+ #use specified colors
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))


###By species####
gh_sl <- gh_final %>% #pivot so all the species are in the same column
  pivot_longer(
    cols = 7:16,
    names_to = "Species",
    values_to = "Cover"
  )

#colors to use for graph
cp <- c("khaki3", "yellow2", "wheat2",
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

####Phragmites present####
c <- gh_sl %>% 
  filter(Phrag_Presence != "WO") %>% #only phragmites present
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% #change order of everything
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #change labels for the graph
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "", y = "", fill = "Species",
       title = "*P.australis* Present") +
  facet_grid(~Mix)+
  scale_fill_manual(values = cp, #use specified colors
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
        axis.title.y = element_text(size = 9))
guides(fill=guide_legend(ncol = 1))

####Phragmites absent####
d <- gh_sl %>% 
  filter(Phrag_Presence != "W") %>% #only phragmites absent
  mutate(Species = factor(Species, 
                          levels = c("BOMA", "SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "Phrag")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% #reorder everything
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #add labels for the graph
  ggplot(aes(fill = Species, y = Cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Species",
       title = "(b) *P.australis* Absent") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp) + #use specified colors
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9))

##Biomass data####
###By functional group ####
colors <- c("#C77CFF",  "#7CAE00", "#00BFC4", "#F8766D") #colors to use for graph 
biomass_t <- biomass_dat %>%
  filter(Mix != "PHAU") %>% #remove phragmites control
  group_by(Mix, Density, Phrag_Presence) %>%
  summarize(Grass = sum(DISP, MUAS, PUNU, na.rm = TRUE),
            Forb = sum(EUOC, EUMA, SOCA, na.rm = TRUE),
            Bulrush = sum(SCAM, SCAC, BOMA, na.rm = TRUE),
            Phragmites = PHAU) #add up all the totals by group

biomass_l <- biomass_t %>% #pivot so all the functional groups are in the same column
  tidyr::pivot_longer(
    cols = 4:7,
    names_to = "group",
    values_to = "cover"
  )

####Phragmites present####
e <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c( "Bulrush", "Forb", "Grass","Equal", "PHAU"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>%  #reorder everything and change labels
  filter(Phrag_Presence != "WO") %>% #only phrag present
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
  scale_fill_manual(values = colors) #use specified colors

#### Phragmites absent####
f <- biomass_l %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #reorder and change labels
  filter(Phrag_Presence != "W") %>% #only the phrag absent
  ggplot(aes(fill = group, y = cover, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "", y = "Relative Abundance", fill = "Group",
       title = "(c )*P.australis* Absent")+
  facet_grid(~Mix) +
  theme(plot.title = ggtext::element_markdown(size = 9),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9)) +
  scale_fill_manual(values = colors) #use specified colors

### By species ####
biomass_sl <- biomass_dat %>% #pivot so all the species in the same column
  pivot_longer(
    cols = 5:14,
    names_to = "Species",
    values_to = "Biomass"
  )

#colors to use in graphs
cp <- c("yellow2", "wheat2",
        "seagreen3", "darkgreen", "lawngreen",
        "turquoise2", "skyblue2", "royalblue2",
        "orangered3")

####Phragmites present ####
g <- biomass_sl %>% 
  filter(Phrag_Presence != "WO") %>% #only phragmites present
  mutate(Species = factor(Species, 
                          levels = c("SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU")), #change the order of species so they match other graphs
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #reorder and relabel
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(x = "Density", y = "", fill = "Species",
       title = "*P.australis* Present") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp)+ #use manual colors
  theme(plot.title = ggtext::element_markdown(size = 9), #allows me to use italics
        legend.text = ggtext::element_markdown(), #allows me to use italics
        legend.key.size = unit(.25, "cm"),
        legend.title = element_text(size = 9),
        legend.position = "none", 
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) +
  guides(fill=guide_legend(ncol = 1))

####Phragmites absent####
h <- biomass_sl %>% 
  filter(Phrag_Presence != "W") %>% #only phragmites absent
  mutate(Species = factor(Species, 
                          levels = c("SCAC", "SCAM",
                                     "DISP", "MUAS", "PUNU",
                                     "EUMA", "EUOC", "SOCA",
                                     "PHAU")),
         Mix = factor(Mix, 
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% #reorder and relabel
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  ggplot(aes(fill = Species, y = Biomass, x = Density)) +
  geom_bar(position = "fill", stat = "identity", show.legend = FALSE) +
  labs(x = "Density", y = "Relative Abundance", fill = "Species")+
  ggtitle("(d) *P.australis* Absent") +
  facet_grid(~Mix) +
  scale_fill_manual(values = cp)+  
  theme(plot.title = ggtext::element_markdown(size = 9), 
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        axis.title.y = element_text(size = 9),
        axis.title.x = element_text(size = 9)) 

(b+a) / (d+c) / (f+e) / (h+g) + plot_layout(guides = "collect")

#Reduction graphs####
##Cover reduction ####
#only need the last date of sampling
gh_final <- cover_dat%>%
  filter(cover_dat$Date == "2023-03-01") 

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

#graph it
cover <- final.df %>%
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #change the order and labels
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
  scale_color_manual(values = c("darkblue", "red3")) #use manual colors

##Biomass reduction####
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
                          labels = c("Low", "High"))) %>% #reorder and relabel
  ggplot(aes(x = Mix, y = Pos.Red, color = Density), size = 1) +
  scale_y_continuous(name = "Reduction in *P.australis* <br>Biomass", #for some reason ylim was changing the data
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
  scale_color_manual(values = c("darkblue", "red3")) #add manual colors

#combine the graphs
cover/biomass

#Raw cover data over time #### 
### Native cover ####
cover_dat %>% 
  filter(Mix != "PHAU") %>% #only native species
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% #reorder and relabel
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

###Phrag cover ####
#NAs for density and phrag_presence are throwing off the graph so I changed them
cover_dat$Density <- as.character(cover_dat$Density) #can't change while a factor
cover_dat$Phrag_Presence <- as.character(cover_dat$Phrag_Presence)
cover_dat$Density[is.na(cover_dat$Density)] <- "Control" #changed name
cover_dat$Phrag_Presence[is.na(cover_dat$Phrag_Presence)] <- "Control"
cover_dat$Density <- as.factor(cover_dat$Density) #made a factor again
cover_dat$Phrag_Presence <- as.factor(cover_dat$Phrag_Presence)

cover_dat %>% 
  filter(Phrag_Presence != "WO") %>% #only phragmites present because graphing phrag
  mutate(Mix = factor(Mix,
                      levels = c('PHAU',"Bulrush", "Forb", "Grass", "Equal"),
                      labels = c("Control", "Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("Control", "L", "H"),
                          labels = c("Control", "Low", "High"))) %>% #reorder and relabel
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

# Raw biomass data over time ####
##Native biomass####
biomass_dat %>%
  group_by(Mix, Density, Phrag_Presence, Replicate) %>% 
  summarize(PHAU = PHAU, 
            Total_Native = sum(EUMA,SOCA, EUOC, SCAC, #total all the natives together
                               SCAM, DISP, MUAS, PUNU,BOMA, na.rm = TRUE)) %>% 
  filter(Mix != "PHAU")%>% #only natives
  mutate(Mix = factor(Mix,
                      levels = c("Bulrush", "Forb", "Grass", "Equal"))) %>% 
  mutate(Density = factor(Density,
                          levels = c("L", "H"),
                          labels = c("Low", "High"))) %>% 
  mutate(Phrag_Presence = factor(Phrag_Presence,
                                 levels = c("WO", "W"),
                                 labels = c("Absent", "Present"))) %>% #reorder and relabel
  ggplot(aes(x = Mix, y = Total_Native, color = Density)) +
  facet_grid(~factor(Phrag_Presence)) +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun = mean, geom = "point") +
  stat_summary(aes(group = interaction(Mix, Density, Phrag_Presence)),
               fun.data = mean_se, geom = "errorbar", width = 0) +
  labs(x = "Seed Mix", y = "Total Native Biomass")+
  scale_color_manual(values = c("darkblue", "red3"))+
  coord_cartesian(ylim = c(0, 90))

##Phrag Biomass####
#changing the NA values to C so they don't throw off graph
biomass_dat$Density <- as.character(biomass_dat$Density) #cant change while a factor
biomass_dat$Density[is.na(biomass_dat$Density)] <- "C" #rename
biomass_dat$Density <- factor(biomass_dat$Density, #make a factor and change labels
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

#Shannon Diversity Index Graphs ####
#Import data
load("main_dfs.RData")

##Cover ####
#only want the final cover
final.dat <- cover_dat %>% 
  filter(Date == "2023-03-01")

#make a unique identifier (combined mix, density, presence, and replicate)
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

#Now calculate the indices
final.dat3 <- dplyr::select(final.dat3, -c("Tub", 'Date', 'Total', 'Notes'))
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#Plot
#change order of phrag presence and also labels
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                   labels = c("Absent", "Present"))
final.dat$Density <- factor(final.dat$Density, levels = c("L", "H"),
                            labels = c("Low", "High"))
final.dat$Mix <- factor(final.dat$Mix, 
                        levels = c("Bulrush", "Forb", "Grass", "Equal"))

((a <- final.dat %>% 
    filter(Mix != "PHAU") %>% #remove the phau control
    ggplot(aes(x = Mix, y = shannon, color = Density)) +
    stat_summary(aes(group = interaction(Density, Mix)),
                 fun = mean, geom = "point", size = 2,
                 position = position_dodge(width = .5)) +
    stat_summary(aes(group = interaction(Density, Mix), width = 0),
                 fun.data = mean_se, geom = "errorbar",
                 position = position_dodge(width = .5)) +
    facet_wrap(~Phrag_Presence) +
    ylab("Mean Shannon Diversity Index") +
    ggtitle("(a)") +
    scale_color_manual(values = c("darkblue", "red3"))+
    theme(plot.title = element_text(size = 9),
          legend.position = "none",
          axis.text.x = element_text(angle = 45, hjust = 0.9)) +
    ylim(0, 2)
))

## Biomass ####
final.dat <- biomass_dat #keep this one aside so I can manipulate it later
#make a unique identifier
final.dat2 <- final.dat %>% 
  unite(col = "Tub",
        c('Mix', 'Density', 'Phrag_Presence', 'Replicate'))

#make all the NAs into 0s
final.dat2[is.na(final.dat2)] <- 0

#name the rows
final.dat3 <- final.dat2
row.names(final.dat3) <- final.dat3$"Tub"

#Calculate the diversity indices
final.dat3 <- dplyr::select(final.dat3, -"Tub")
div <- diversity(final.dat3, "shannon")
final.dat$shannon <- div

#Plot
#change order and labels
final.dat$Phrag_Presence <- factor(final.dat$Phrag_Presence, levels = c("WO", "W"),
                                   labels = c("Absent", "Present"))

final.dat$Density <- factor(final.dat$Density, levels = c("L", "H"),
                            labels = c("Low", "High"))
final.dat$Mix <- factor(final.dat$Mix, 
                        levels = c("Bulrush", "Forb", "Grass", "Equal"))

((b <- final.dat %>% 
    filter(Mix != "PHAU") %>% #remove the phau control plots
    ggplot(aes(x = Mix, y = shannon, color = Density)) +
    stat_summary(aes(group = interaction(Density, Mix)),
                 fun = mean, geom = "point", size = 2,
                 position = position_dodge(width = .5)) +
    stat_summary(aes(group = interaction(Density, Mix), width = 0),
                 fun.data = mean_se, geom = "errorbar",
                 position = position_dodge(width = .5)) +
    ylab("")+
    ggtitle("(b)") +
    facet_wrap(~Phrag_Presence) +
    scale_color_manual(values = c("darkblue", "red3"))+ #change legend labels
    theme(plot.title = element_text(size = 9),
          axis.text.x = element_text(angle = 45, hjust = 0.9),
          legend.position = "right") +
    ylim(0, 2)
))

# Combine graphs ####
a+b