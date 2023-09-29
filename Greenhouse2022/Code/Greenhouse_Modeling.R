#Load objects
load("main_dfs.RData")
library(tidyverse)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(patchwork)

greenhouse$Density <- as.factor(greenhouse$Density)
greenhouse$Phrag_Presence <- factor(greenhouse$Phrag_Presence, levels = c("WO", "W"),
                                    labels = c("Absent", "Present"))
greenhouse$Species <- as.factor(greenhouse$Species)

biomass$Phrag_Presence <- factor(biomass$Phrag_Presence, levels = c("WO", "W"),
                                    labels = c("Absent", "Present"))

#check <- greenhouse %>%
#filter(Date_Cleaned == "2022-05-16",
#       Cover.Native < 5.0)
#Species that did not really grow: JUTO LWO 1, JUGE LW 1, SYCI HW 1, SCAM LWO 1, 
#JUTO LW 2, SCAM LWO 2, SCAM LW 2, JUTO HW 2, JUGE LWO 2, JUTO HWO 3, BOMA LW 3, JUTO LW 3,
#JUTO LWO 3, SCAM LWO 3, BOMA HW 3, BOMA LWO 3, BOMA HWO 2, JUGE LW 3

#Graphs to look quickly at species####

#how each species changes over time by density and presence of phrag - up close for each species
greenhouse %>%
  filter(!is.na(Density), Species == "SYCI") %>% #everything that is not NA for density 
  ggplot(aes(x = Date, y = Cover.Native, col = Block, group = Block)) +
  geom_point() + geom_line() +
  facet_wrap(~Density + Phrag_Presence)
#will need to do this individually for each species

#how each species changes over time by density and presence of phrag - up close for each species
biomass %>%
  filter(!is.na(Density), Species == "RUMA") %>% #everything that is not NA for density 
  ggplot(aes(x = Density, y = Native.Biomass, col = Block, group = Block)) +
  geom_point() + geom_line() +
  facet_wrap(~Density + Phrag_Presence)
#will need to do this individually for each species

#Example of how to model####
#use glmmtmb because will eventually use beta 

#this is for the type of test - needed for Type III test - also required you to use Anova () in car package
options(contrasts = c("contr.sum", "contr.poly"))

##Example
#model for a single species - the mean of native height WO and W, and Date
henu <- greenhouse %>%
  filter(Species == "HENU", !is.na(Density),
         Date == "2022-05-16" | Date == "2022-05-17")

henu.m1 <- glmmTMB(log(Height.Native) ~ Phrag_Presence * Density #* for interaction
                   + (1|Block),
                   data = henu,
                   family = gaussian
)
summary(henu.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(henu.m1, plot = T) #not a great fit, but only 12 obs
plotResiduals(henu.m1, form= henu$Phrag_Presence) #unequal variances, median is below so distribution is skewed
#some evidence that the residuals are not normal and the residuals are not even
#we can take the log to address this - but it didn't help in this case

library(car)
Anova(henu.m1) #no evidence of significant differences
emmip(henu.m1, Phrag_Presence~Density, CIs = T)
#looks like there is an interaction, but not once we add confidence intervals

#so appears that there is no difference on the last date between density or when there is or is not phrag
#but there isn't much power
 
#Model to run for Height ####
#Use this to go through all the species - results saved on google doc 

mdf <- greenhouse %>%
  filter(Species == "EPCI", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Height.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian)

summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(mdf.m1, plot = T) #not a great fit, but only 12 obs
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

#Model to run for Cover ####
mdf <- greenhouse %>%
  filter(Species == "SOCA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
                  )


#summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks


#simulateResiduals(mdf.m1, plot = T) 
#plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#A lot of the residual estimates look really bad but I checked with Susan and she said it is fine
#There is not a lot of variation in the data, so there are not a lot of residuals to be checked
#The tests and estimates themselves seem reasonable so it is okay

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)
emmip(mdf.m1, Density ~ Phrag_Presence, CIs = T)

#Model to run for biomass ####
mdf <- biomass %>%
  filter(Species == "BICE", !is.na(Density))

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)

#summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks

#simulateResiduals(mdf.m1, plot = T) 
#plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

#library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

emmeans(mdf.m1, pairwise ~ Density|Phrag_Presence)
emmeans(mdf.m1, pairwise ~ Phrag_Presence|Density)


# Graphing ####
##Cover ####
###BICE####
mdf <- greenhouse %>%
  filter(Species == "BICE", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m1)

emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data1

BICE <- ggplot(data = data1, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = 1.5) +
  ggtitle("BICE") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low")) +
  ylim(0, 1.2)

###DISP####
mdf <- greenhouse %>%
  filter(Species == "DISP", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m2 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m2)

emm <- emmeans(mdf.m2, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data2
str_2 <- gsub(" ", "", data2$.group)

DISP <- ggplot(data = data2, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", hjust = -.1, vjust = .7) +
  ggtitle("DISP") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###EPCI####
mdf <- greenhouse %>%
  filter(Species == "EPCI", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m3 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m3)

emm <- emmeans(mdf.m3, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data3

EPCI <- ggplot(data = data3, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = 2) +
  ggtitle("EPCI") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###EUOC####
mdf <- greenhouse %>%
  filter(Species == "EUOC", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m4 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m4)

emm <- emmeans(mdf.m4, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data4
str_4 <- gsub(" ", "", data4$.group)

EUOC <- ggplot(data = data4, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_4,  y = response), 
            position = position_jitter(seed=2),
            color = "black", hjust = -.5) +
  ggtitle("EUOC") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###EUMA####
mdf <- greenhouse %>%
  filter(Species == "EUMA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m5 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m5)

emm <- emmeans(mdf.m5, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data5 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data5

EUMA <- ggplot(data = data5, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=1)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 1)) +
  labs(x="", y = "Model Predicted <br> Proportional Cover", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=1),
            color = "black", vjust = 1.5) +
  ggtitle("EUMA") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###HENU####
mdf <- greenhouse %>%
  filter(Species == "HENU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m6 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m6)

emm <- emmeans(mdf.m6, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data6<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data6
str_6 <- gsub(" ", "", data6$.group)

HENU <- ggplot(data = data6, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_6,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = 1.5) +
  ggtitle("HENU") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###JUAR####
mdf <- greenhouse %>%
  filter(Species == "JUAR", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m7 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m7)

emm <- emmeans(mdf.m7, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data7<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data7
str_7 <- gsub(" ", "", data7$.group)

JUAR <- ggplot(data = data7, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_7,  y = response), 
            position = position_jitter(seed=5),
            color = "black", vjust = -1.5) +
  ggtitle("JUAR") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###MUAS####
mdf <- greenhouse %>%
  filter(Species == "MUAS", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m8 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m8)

emm <- emmeans(mdf.m8, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data8<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data8

MUAS <- ggplot(data = data8, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=1)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 1)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=1),
            color = "black", hjust = -.1, vjust = 1) +
  ggtitle("MUAS") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###PUNU####
mdf <- greenhouse %>%
  filter(Species == "PUNU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m9 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m9)

emm <- emmeans(mdf.m9, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data9<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data9
str_9 <- sub(" ", "", data9$.group)

PUNU <- ggplot(data = data9, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_9,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = 1.5) +
  ggtitle("PUNU") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###RUMA####
mdf <- greenhouse %>%
  filter(Species == "RUMA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m10 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m10)

emm <- emmeans(mdf.m10, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data10<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data10

RUMA <- ggplot(data = data10, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = 1.5) +
  ggtitle("RUMA") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###SCAC####
mdf <- greenhouse %>%
  filter(Species == "SCAC", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m11 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m11)

emm <- emmeans(mdf.m11, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data11<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data11
str_11 <- gsub(" ", "", data11$.group)

SCAC <- ggplot(data = data11, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_11,  y = response), 
            position = position_jitter(seed=5),
            color = "black", vjust = -1.5) +
  ggtitle("SCAC") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###SCPU####
mdf <- greenhouse %>%
  filter(Species == "SCPU", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m12 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)
Anova(mdf.m12)

emm <- emmeans(mdf.m12, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data12 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data12

SCPU <- ggplot(data = data12, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = -1.5) +
  ggtitle("SCPU") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###SOCA####
mdf <- greenhouse %>%
  filter(Species == "SOCA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m13 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                   + (1|Block),
                   data = mdf,
                   family = beta_family #change the family to beta
)
Anova(mdf.m13)

emm <- emmeans(mdf.m13, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data13 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data13
str_13 <- gsub(" ", "", data13$.group)

SOCA <- ggplot(data = data13, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=1)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 1)) +
  labs(x="*P. australis* Presence", y = "", color = "Density") +
  geom_text(aes(label = str_13,  y = response), 
            position = position_jitter(seed=1),
            color = "black", hjust = -.5) +
  ggtitle("SOCA") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 1.2)

###ALL TOGETHER####
BICE + DISP + EPCI + EUOC + EUMA + HENU + 
  JUAR + MUAS + PUNU + RUMA + SCAC + SCPU + SOCA +
  guide_area() +
  plot_layout(guides = "collect")

ggsave("two-way_model-means_cover.jpeg")

##Biomass####
###BICE####
mdf <- biomass %>%
  filter(Species == "BICE", !is.na(Density))

mdf.m1 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m1)

emm <- emmeans(mdf.m1, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type= "response")
data1 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data1

BICE_b <- ggplot(data = data1, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", hjust = -.1, vjust = .7) +
  ggtitle("BICE") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###DISP####
mdf <- biomass %>%
  filter(Species == "DISP", !is.na(Density))

mdf.m2 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m2)

emm <- emmeans(mdf.m2, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data2 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data2

DISP_b <- ggplot(data = data2, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = -1) +
  ggtitle("DISP") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###EPCI####
mdf <- biomass %>%
  filter(Species == "EPCI", !is.na(Density))

mdf.m3 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m3)

emm <- emmeans(mdf.m3, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data3 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data3
str_3 <- gsub(" ", "", data3$.group)

EPCI_b <- ggplot(data = data3, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = 1.1, hjust = -.05) +
  ggtitle("EPCI") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###EUOC####
mdf <- biomass %>%
  filter(Species == "EUOC", !is.na(Density))

mdf.m4 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m4)

emm <- emmeans(mdf.m4, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data4 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data4
str_4 <- gsub(" ", "", data4$.group)

EUOC_b <- ggplot(data = data4, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=6)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 6)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_4,  y = response), 
            position = position_jitter(seed=6),
            color = "black", vjust = -1, hjust = -.05) +
  ggtitle("EUOC") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###EUMA####
mdf <- biomass %>%
  filter(Species == "EUMA", !is.na(Density))

mdf.m5 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m5)

emm <- emmeans(mdf.m5, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data5 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data5

EUMA_b <- ggplot(data = data5, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "Model Predicted <br> Biomass (g)", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = -1) +
  ggtitle("EUMA") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.title.y = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###HENU####
mdf <- biomass %>%
  filter(Species == "HENU", !is.na(Density))

mdf.m6 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m6)

emm <- emmeans(mdf.m6, pairwise ~ Density * Phrag_Presence, adjust = "tukey", type = "response")
data6<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data6

HENU_b <- ggplot(data = data6, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = -1) +
  ggtitle("HENU") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###JUAR####
mdf <- biomass %>%
  filter(Species == "JUAR", !is.na(Density))

mdf.m7 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m7)

emm <- emmeans(mdf.m7, pairwise ~ Phrag_Presence * Density, adjust = "tukey", type = "response")
data7<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data7

JUAR_b <- ggplot(data = data7, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = -1) +
  ggtitle("JUAR") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###MUAS####
mdf <- biomass %>%
  filter(Species == "MUAS", !is.na(Density))

mdf.m8 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m8)

emm <- emmeans(mdf.m8, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data8<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data8

MUAS_b <- ggplot(data = data8, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = -1) +
  ggtitle("MUAS") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###PUNU####
mdf <- biomass %>%
  filter(Species == "PUNU", !is.na(Density))

mdf.m9 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m9)

emm <- emmeans(mdf.m9, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data9<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data9
str_9 <- gsub(" ", "", data9$.group)

PUNU_b <- ggplot(data = data9, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = str_9,  y = response), 
            position = position_jitter(seed=4),
            color = "black", hjust = -1) +
  ggtitle("PUNU") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###RUMA####
mdf <- biomass %>%
  filter(Species == "RUMA", !is.na(Density))

mdf.m10<- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m10)

emm <- emmeans(mdf.m10, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data10<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data10

RUMA_b <- ggplot(data = data10, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = 1.5) +
  ggtitle("RUMA") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###SCAC####
mdf <- biomass %>%
  filter(Species == "SCAC", !is.na(Density))

mdf.m11 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m11)

emm <- emmeans(mdf.m11, pairwise ~ Density*Phrag_Presence, adjust = "tukey", type = "response")
data11<- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data11

SCAC_b <- ggplot(data = data11, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=5)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 5)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=5),
            color = "black", vjust = -1) +
  ggtitle("SCAC") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###SCPU####
mdf <- biomass %>%
  filter(Species == "SCPU", !is.na(Density))

mdf.m12 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m12)

emm <- emmeans(mdf.m12, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data12 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data12

SCPU_b <- ggplot(data = data12, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=4)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 4)) +
  labs(x="", y = "", color = "Density") +
  geom_text(aes(label = .group,  y = response), 
            position = position_jitter(seed=4),
            color = "black", vjust = -1) +
  ggtitle("SCPU") +
  theme(axis.title.x = ggtext::element_markdown(),
        plot.title = element_text(size = 9),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        legend.position = "none") +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###SOCA####
mdf <- biomass %>%
  filter(Species == "SOCA", !is.na(Density))

mdf.m13 <- glmmTMB(sqrt(Native.Biomass) ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = gaussian
)
Anova(mdf.m13)

emm <- emmeans(mdf.m13, pairwise ~ Phrag_Presence*Density, adjust = "tukey", type = "response")
data13 <- multcomp::cld(emm$emmeans, alpha = 0.1, Letters = letters)
data13
dat_list <- c("a", " ab", " ab", "b")

SOCA_b <- ggplot(data = data13, aes(x = Phrag_Presence, y = response, color = Density)) +
  geom_point(size=2, position = position_jitter(seed=2)) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5, position = position_jitter(seed = 2)) +
  labs(x="*P. australis* Presence", y = "", color = "Density") +
  geom_text(aes(label = dat_list,  y = response), 
            position = position_jitter(seed=2),
            color = "black", vjust = -1) +
  ggtitle("SOCA") +
  theme(axis.title.x = ggtext::element_markdown(),
        axis.text.x = element_text(angle = 25, hjust = 0.9),
        plot.title = element_text(size = 9)) +
  scale_color_manual(values = c("red3", "darkblue"), labels = c("High", "Low"))+
  ylim(0, 120)

###ALL TOGETHER####
BICE_b + DISP_b + EPCI_b + EUOC_b + EUMA_b + HENU_b + 
  JUAR_b + MUAS_b + PUNU_b + RUMA_b + SCAC_b + SCPU_b + SOCA_b +
  guide_area() +
  plot_layout(guides = "collect")

ggsave("two-way_model-means_biomass.jpeg", height = 1737, width = 1675, units = 
        "px")

##All all together####
