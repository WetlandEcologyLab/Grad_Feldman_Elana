#Load objects
load("main_dfs.RData")
library(magrittr)
library(dplyr)
library(ggplot2)

##Model to run for cover
mdf <- greenhouse %>%
  filter(Species == "RUMA", !is.na(Density),
         Date_Cleaned == "2022-05-16")

mdf.m1 <- glmmTMB(Cover.Native ~ Phrag_Presence * Density #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family #change the family to beta
)

summary(mdf.m1)
#model specification probably okay because 12 obs and 3 blocks


simulateResiduals(mdf.m1, plot = T) 
plotResiduals(mdf.m1, form= mdf$Phrag_Presence)

library(car)
Anova(mdf.m1) 
emmip(mdf.m1, Phrag_Presence~Density, CIs = T)

