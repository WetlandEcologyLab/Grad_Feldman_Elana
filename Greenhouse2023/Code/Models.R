#Load all the data and packages
#All package version saved in renv.lock 
#renv::init, renv::restore
load("main_dfs.RData")
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)
library(car)
library(multcomp)
library(patchwork)
options(contrasts = c("contr.sum", "contr.poly")) #run this in case we want to run a type-iii anova

#Native cover~Phrag Presence * Density * Seed Mix####

#just need the last date
mdf <- cover_dat %>%
  dplyr::filter(!is.na(Density), #removes all PHAU controls
                Date == "2023-03-01")

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                                 labels = c("Low", "High")
)

mdf.m1<- glmmTMB(Total ~ Phrag_Presence * Density * Mix,
                 data = mdf,
                 family = beta_family)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Density)

Anova(mdf.m1) 
#looks like only Density is significant

emmip(mdf.m1, Mix~Density|Phrag_Presence, CIs = T)
emmip(mdf.m1, Phrag_Presence~Density|Mix, CIs = T)

emmeans(mdf.m1, pairwise~Density, type = "response", adjust = 'tukey')
#high density is significantly higher

## Graphing the model means####
emm1 <- emmeans(mdf.m1, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data1 <- multcomp::cld(emm1, alpha = 0.1, Letters = letters)

((a <- ggplot(data = data1, aes(x = Density, y = response, color = Density)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5) +
    labs(x="Native Seeding Density", y = "Model Predicted <br> Proportional Native Cover",
         title = '(a)') +
    ylim(.7, 1) +
    geom_text(aes(label = .group,  y = response),
              nudge_x = 0.2, color = "black") +
    scale_color_manual(values = c("darkblue", "red3")) +
    theme(legend.position = "none",
          axis.title.y = ggtext::element_markdown(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 9)) +
    coord_cartesian(ylim = c(0, 1))
))

#Phrag Cover~Density * Seed Mix####

#just need the last date
mdf <- cover_dat %>%
  dplyr::filter(!is.na(Phrag), #removes all the native plants
                Date == "2023-03-01")

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m2<- glmmTMB(Phrag ~ Density * Mix,
                 data = mdf,
                 family = beta_family)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
plotResiduals(mdf.m2, form= mdf$Density)

Anova(mdf.m2) 
#Density is significant, mix is marginally significant

emmip(mdf.m2, Density~Mix, CIs = T)

emmeans(mdf.m2, pairwise~Density, type = "response", adjust = "tukey")
#low significantly higher than high

emmeans(mdf.m2, pairwise~Mix, type = "response")
#nothing significant with the Tukey test

## Graphing the model means####
emm2 <- emmeans(mdf.m2, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data2 <- multcomp::cld(emm2, alpha = 0.1, Letters = letters)

((c <- ggplot(data = data2, aes(x = Density, y = response, color = Density)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin = (response - SE),
                      ymax = (response+SE)),
                  width=0, size=0.5) +
    labs(x="Native Seeding Density", y = "Model Predicted <br> Proportional *P.australis* Cover",
         title = "(c)") +
    geom_text(aes(label = .group,  y = response),
              nudge_x = 0.2, color = "black") +
    ylim(0, .1) +
    scale_color_manual(values = c("darkblue", "red3"))+
    theme(plot.title = element_text(size = 9),
          axis.title.y = ggtext::element_markdown(size = 10),
          axis.title.x = element_text(size = 10),
          legend.position = "none")
))

# Native biomass~Phrag Presence * Density * Seed Mix####
mdf <- biomass_dat %>%
  group_by(Mix, Density, Phrag_Presence, Replicate) %>% 
  summarize(PHAU = PHAU, #separate all the data into PHAU and total native categories
            Total_Native = sum(EUMA,SOCA, EUOC, SCAC, #get a sum of total cover
                               SCAM, DISP, MUAS, PUNU,BOMA, na.rm = TRUE)) %>% 
  dplyr::filter(!is.na(Density))


#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m3<- glmmTMB(Total_Native ~ Phrag_Presence * Density * Mix,
                 data = mdf,
                 family = gaussian)

summary(mdf.m3)
simulateResiduals(mdf.m3, plot = T) 
plotResiduals(mdf.m3, form= mdf$Density)

Anova(mdf.m3) 
#looks like density is significant and mix is marginally significant

emmip(mdf.m3, Mix~Density|Phrag_Presence, CIs = T)
emmip(mdf.m3, Phrag_Presence~Density|Mix, CIs = T)

emmeans(mdf.m3, pairwise~Density, type = "response")
#high significantly higher than the low

emmeans(mdf.m3, pairwise~Mix, type = "response")
#no differences when using the tukey

## Graphing the model means####
emm3 <- emmeans(mdf.m3, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data3 <- multcomp::cld(emm3, alpha = 0.1, Letters = letters)

((b <- ggplot(data = data3, aes(x = Density, y = emmean, color = Density)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin = (emmean - SE),
                      ymax = (emmean+SE)),
                  width=0, size=0.5) +
    labs(x="Native Seeding Density", y = "Model Predicted <br> Native Biomass (g)",
         title = '(b)') +
    ylim(30, 100) +
    geom_text(aes(label = .group,  y = emmean),
              nudge_x = 0.2, color = "black")+
    scale_color_manual(values = c("darkblue", "red3"))+
    theme(legend.position = "none",
          axis.title.y = ggtext::element_markdown(size = 10),
          axis.title.x = element_text(size = 10),
          plot.title = element_text(size = 9))+
    coord_cartesian(ylim = c(0, 80))
))


# Phrag biomass~Density * Seed Mix####
mdf <- biomass_dat %>%
  dplyr::filter(!is.na(PHAU)) #removes all the natives

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m4<- glmmTMB(PHAU ~ Density * Mix,
                 data = mdf,
                 family = gaussian)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
plotResiduals(mdf.m4, form= mdf$Density)

Anova(mdf.m4) 
#only Density is significant

emmip(mdf.m4, Mix~Density, CIs = T)

emmeans(mdf.m4, pairwise~Density, type = "response")
#low is higher than high

## Graphing the model means####
emm5 <- emmeans(mdf.m4, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data5 <- multcomp::cld(emm5, alpha = 0.1, Letters = letters)

((d <- ggplot(data = data5, aes(x = Density, y = emmean, color = Density)) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin = (emmean - SE),
                      ymax = (emmean+SE)),
                  width=0, size=0.5) +
    labs(x="Native Seeding Density", y = "Model Predicted <br> *P. australis* Biomass (g)",
         title = '(d)') +
    ylim(0, 10) +
    geom_text(aes(label = .group,  y = emmean),
              nudge_x = 0.2, color = "black") +
    scale_color_manual(values = c("darkblue", "red3")) +
    theme(plot.title = element_text(size = 9),
          axis.title.y = ggtext::element_markdown(size = 10),
          axis.title.x = element_text(size = 10),
          legend.position = "none")
))



