load("main_dfs.RData")
library(tidyverse)
library(glmmTMB) #allows us to use a beta distribution
library(DHARMa)
library(emmeans)
library(car)
library(multcomp)
library(patchwork)
options(contrasts = c("contr.sum", "contr.poly"))

#Native cover ####

mdf <- cover_dat %>%
  dplyr::filter(!is.na(Density),
                Date == "2023-03-01")

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                                 labels = c("Low", "High")
)

mdf.m1<- glmmTMB(Total ~ Phrag_Presence * Density * Mix  #* for interaction
                 + (1|Replicate),
                 data = mdf,
                 family = beta_family)

summary(mdf.m1)
simulateResiduals(mdf.m1, plot = T) 
#not the best
plotResiduals(mdf.m1, form= mdf.m1$Density)
#not working like usual, not sure why 

Anova(mdf.m1) 
#looks like only Density is significant, which makes sense with the graphs
#p-value = 6.175e-14
emmip(mdf.m1, Mix~Density|Phrag_Presence, CIs = T)
#looks like high is always higher, regardless of phrag_presence or mix
emmip(mdf.m1, Phrag_Presence~Density|Mix, CIs = T)
#same as above, almost some interactions but not with the CIs

emmeans(mdf.m1, pairwise~Density, type = "response")
#high is around 95% and low is around 80%, p-value = <0.0001
#don't know if I really need this since no interactions

## Making the graph ####
library(multcomp)
emm1 <- emmeans(mdf.m1, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data1 <- multcomp::cld(emm1, alpha = 0.1, Letters = letters)

ggplot(data = data1, aes(x = Density, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted Cover (%)") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

ggsave("model_means_native_cover.jpeg")

#Phrag Cover ####
mdf <- cover_dat %>%
  dplyr::filter(!is.na(Phrag),
                Date == "2023-03-01")
#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m2<- glmmTMB(Phrag ~ Density * Mix  #* for interaction
                 + (1|Replicate),
                 data = mdf,
                 family = beta_family)

summary(mdf.m2)
simulateResiduals(mdf.m2, plot = T) 
#look pretty good
plotResiduals(mdf.m2, form= mdf.m1$Density)
#not working like usual, not sure why 

Anova(mdf.m2) 
#looks like only Density is significant (p = 4.47e-05), mix is marginally significant (p = 0.08227)

emmip(mdf.m2, Density~Mix, CIs = T)
#looks like low generally has more phrag biomass
#forb is the same for both high and low, bulrush and equal have some overlap of CI, grass is much higher in low

emmeans(mdf.m2, pairwise~Density, type = "response")
#high is about 4% and low is 8%, p-value = 0.0005

emmeans(mdf.m2, pairwise~Mix, type = "response")
#nothing significant with the Tukey test, only thing almost significant is the forb/grass

## Making the graph ####
emm2 <- emmeans(mdf.m2, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data2 <- multcomp::cld(emm2, alpha = 0.1, Letters = letters)

ggplot(data = data2, aes(x = Density, y = response * 100)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = 100*(response - SE),
                    ymax = 100*(response+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted Cover (%)") +
  geom_text(aes(label = .group,  y = response * 100),
            nudge_x = 0.2)

ggsave("model_means_phrag_cover_density.jpeg")

# Native biomass ####
mdf <- biomass_dat %>%
  group_by(Mix, Density, Phrag_Presence, Replicate) %>% 
  summarize(PHAU = PHAU,
            Total_Native = sum(EUMA,SOCA, EUOC, SCAC, #get a sum of total cover
                               SCAM, DISP, MUAS, PUNU,BOMA, na.rm = TRUE)) %>% 
  dplyr::filter(!is.na(Density))


#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m3<- glmmTMB(Total_Native ~ Phrag_Presence * Density * Mix  #* for interaction
                 + (1|Replicate),
                 data = mdf,
                 family = gaussian)

summary(mdf.m3)
simulateResiduals(mdf.m3, plot = T) 
#looks great
plotResiduals(mdf.m3, form= mdf.m3$Density)
#not working like usual, not sure why 

Anova(mdf.m3) 
#looks like density is significant and mix is marginally significant
emmip(mdf.m3, Mix~Density|Phrag_Presence, CIs = T)
#looks like high is always higher, regardless of phrag_presence or mix
emmip(mdf.m3, Phrag_Presence~Density|Mix, CIs = T)
#same as above, almost some interactions but not with the CIs

emmeans(mdf.m3, pairwise~Density, type = "response")
#high is 69.5 and low is 38.1, p-value = <.0001

emmeans(mdf.m3, pairwise~Mix, type = "response")
#only difference significant with the tukey is equal - grass (p = 0.0818)
#equal had less total biomass than grass
#slight trend with equal - forb and maybe bulrush - equal

## Making the graph ####
emm3 <- emmeans(mdf.m3, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data3 <- multcomp::cld(emm3, alpha = 0.1, Letters = letters)

ggplot(data = data3, aes(x = Density, y = emmean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted Biomass (g)") +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2)

ggsave("model_means_native_biomass_density.jpeg")

emm4 <- emmeans(mdf.m3, pairwise~Mix, CIs = T, type = 'response', adjust = 'tukey')
data4 <- multcomp::cld(emm4, alpha = 0.1, Letters = letters)

ggplot(data = data4, aes(x = Mix, y = emmean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Seeded Mix", y = "Model Predicted Biomass (g)") +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2)

ggsave("model_means_native_biomass_mix.jpeg")


# Phrag biomass ####
mdf <- biomass_dat %>%
  dplyr::filter(!is.na(PHAU))

#change order of density and also labels for graphing
mdf$Density <- factor(mdf$Density, levels = c("L", "H"),
                      labels = c("Low", "High")
)

mdf.m4<- glmmTMB(PHAU ~ Density * Mix  #* for interaction
                 + (1|Replicate),
                 data = mdf,
                 family = gaussian)

summary(mdf.m4)
simulateResiduals(mdf.m4, plot = T) 
#look pretty good
plotResiduals(mdf.m4, form= mdf.m4$Density)
#not working like usual, not sure why 

Anova(mdf.m4) 
#looks like only Density is significant
emmip(mdf.m4, Mix~Density, CIs = T)
#looks like low is almost always higher than high
#lots of overlap with the mixes

emmeans(mdf.m4, pairwise~Density, type = "response")
#high is 1.95, low is 6.9 (p = 0.0002)

## Making the graph ####
emm5 <- emmeans(mdf.m4, pairwise~Density, CIs = T, type = 'response', adjust = 'tukey')
data5 <- multcomp::cld(emm5, alpha = 0.1, Letters = letters)

ggplot(data = data5, aes(x = Density, y = emmean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted Biomass (g)") +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2)

ggsave("model_means_phrag_biomass_density.jpeg")

# Combining graphs ####
##Native graphs ####
a <- ggplot(data = data1, aes(x = Density, y = response, color = Density)) +
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
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9)) +
  coord_cartesian(ylim = c(0, 1))

b <- ggplot(data = data3, aes(x = Density, y = emmean, color = Density)) +
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
        axis.title.y = ggtext::element_markdown(),
        plot.title = element_text(size = 9))+
  coord_cartesian(ylim = c(0, 80))

c <- ggplot(data = data4, aes(x = Mix, y = emmean)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Seeded Mix", y = "Model Predicted <br> Native Biomass (g)",
       title = '(c)') +
  ylim(30, 100) +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2)+
  theme(plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown())+
  coord_cartesian(ylim = c(0, 80))
(a + b) / c
ggsave("native_cover_biomass_density_mix_tog.jpeg")

##Phrag graphs ####
a <- ggplot(data = data2, aes(x = Density, y = response, color = Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (response - SE),
                    ymax = (response+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted Proportional *P.australis* Cover",
       title = "(a)") +
  geom_text(aes(label = .group,  y = response),
            nudge_x = 0.2, color = "black") +
  ylim(0, .1) +
  scale_color_manual(values = c("darkblue", "red3"))+
  theme(plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none")

b <- ggplot(data = data5, aes(x = Density, y = emmean, color = Density)) +
  geom_point(size=2) +
  geom_errorbar(aes(ymin = (emmean - SE),
                    ymax = (emmean+SE)),
                width=0, size=0.5) +
  labs(x="Native Seeding Density", y = "Model Predicted *P. australis* Biomass (g)",
       title = '(b)') +
  ylim(0, 10) +
  geom_text(aes(label = .group,  y = emmean),
            nudge_x = 0.2, color = "black") +
  scale_color_manual(values = c("darkblue", "red3")) +
  theme(plot.title = element_text(size = 9),
        axis.title.y = ggtext::element_markdown(size = 10),
        axis.title.x = element_text(size = 10),
        legend.position = "none")

a + b
ggsave("phrag_cover_biomass_density_tog.jpeg")
