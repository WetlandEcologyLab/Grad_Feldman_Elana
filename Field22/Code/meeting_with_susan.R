fb2 <-fb %>%
  dplyr::select(Block, Plot, Group, Density, Date, Total.Cover, BOMA, SCAM, SCAC) %>%  #remove unnecessary columns
  filter(Date == "2022-09-16") %>% 
  group_by(Block, Plot, Group, Density) %>% 
  mutate(bulrush = sum(BOMA, SCAM, SCAC, na.rm= TRUE))
with(fb2, table(Group, Density, useNA = "ifany"))
fb3 <- fb2 %>%
  mutate(tGroup = if_else(Group == "10", "0", "1"))
with(fb3, table(Group, tGroup))

# Control versus notControl----
m1 <- glmmTMB(bulrush ~ tGroup
          + (1|Block),
          data = fb3,
          family = gaussian)
summary(m1)
car::Anova(m1)
#comparing the control to the average of all the others
emmeans(m1, ~ tGroup)
#some differences between the control versus everything else
#suggests that the bulrush for the control is less than over the average of the treatments
#at least one of the treatments is having an effect

# Two way factorial without interaction; no control----
m2 <- glmmTMB(bulrush ~ Group + Density,
          data = filter(fb3, tGroup == "1"), #only retain the treatments
          family = gaussian)
summary(m2)
car::Anova(m2)
#strong suggestion that there is no effect of density but there is an effect of group
car::Anova(m2, type = 3)
emmip(m2, Group ~ Density) #lines are parallel because no interaction term
#bulrush tends to be higher at low density
#group 4 has more bulrush
emmip(m2, Group ~ Density, type = "response", CIs = T)
emmeans(m2, ~ Group + Density)
emmeans(m2, pairwise~ Group)

# Two way factorial without interaction plus control----
##not working
m3 <- glmmTMB(bulrush ~ tGroup + tGroup/Group + tGroup/Density
              + (1|Block), #nesting of the fixed effects factors
          data = fb3,
          family = gaussian)
summary(m3)
car::Anova(m3, type = 2)
emmeans(m3, ~ Group + Density)

# Two way factorial with interaction; no control----
m4 <- glmmTMB(bulrush ~ Group * Density,
          data = filter(fb3, tGroup == "1"),
          family = gaussian)
summary(m4)
car::Anova(m4) #no interaction
car::Anova(m4, type = 3)
emmip(m4, Group ~ Density) #lines no longer parallel, no statistical support that they are not parallel (interaction)
emmip(m4, Group ~ Density, type = "response", CIs = T)
emmeans(m4, pairwise~ Group )

# Two way factorial with interaction plus control----
m5 <- glmmTMB(bulrush ~ tGroup + tGroup/Group * tGroup/Density,
          data = fb3,
          family = gaussian)
summary(m5)
emmip(m5, Group ~ Density)
emmip(m5, Group ~ Density, type = "response", CIs = T)
emmeans(m5, ~ Group + Density)






mdf.m1 <- glmmTMBmTMB(bulrush ~ Group * Density #* for interaction
                  + (1|Block),
                  data = fb2,
                  family = gaussian, 
                  control = glmmTMBmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)



mdf$gd <- factor(mdf$Group:mdf$Density) #compares every combination of treatment and control
mdf.m2 <- glmmTMBmTMB(Invasive.Cover ~ gd #* for interaction
                  + (1|Block),
                  data = mdf,
                  family = beta_family, #because cover
                  control = glmmTMBmTMBControl(optimizer = optim, 
                                           optArgs = list(method="BFGS"))
)


#Variability among the control
fb3$gd <- factor(fb3$Group:fb3$Density)
table(fb3$gd)
table(fb3$gd, fb3$bulrush)
ggplot(fb3, aes(x = gd, y = Total.Cover, group = Block, color = as.factor(Block))) +
  geom_point() +
  geom_line()
#control takes two different values
#if there were no variability, we wouldn't need to do anything with it

#when we look at Total.Cover, we see a decisive block effect
#variability in the control that is consistent with the block
fb3$tGroup <- as.factor(fb3$tGroup)
unique(fb3$tGroup:fb3$Group)
m3 <- glmmTMB(bulrush ~ gd
              + (1|Block), #nesting of the fixed effects factors
              data = fb3,
              family = gaussian)
summary(m3)
car::Anova(m3, type = 2)
#testing whether all 11 means are equal versus at least one is different
#this is saying that all the means are the same
emmeans(m3, ~ Group + Density)
emmeans(m3, specs = trt.vs.ctrlk~gd,ref = 3) 
#using dunnett and not showing that there is any difference
#so we see that group4 is different 
#could stop here if I can adequately justify it
#or the next step is to account for the control in the other comparisons

#Plot the control against the treatment
fb_c <- filter(fb3, Group == 10) %>% 
  rename(Total.Cover_C = Total.Cover,
         Bulrush_C = bulrush) %>% 
  dplyr::select(Block, Total.Cover_C, Bulrush_C)
fb_t <- filter(fb3, Group != 10)
test <- left_join(fb_t, fb_c, by = c("Block"))

ggplot(test, aes(x = Total.Cover_C, y = Total.Cover, color = gd, group = gd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  geom_line() +
  facet_wrap(~gd)
#there's not a real treatment effect on total cover
#not much difference between the control and any given treatment, which was the same as the dunnett
#sort of just wobbling around the line

ggplot(test, aes(x = Bulrush_C, y = bulrush, color = gd, group = gd)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1) +
  #geom_line() +
  geom_smooth(method = "lm") +
  facet_wrap(~gd)
#ideally we would want a cloud of points relative to teh black line but we only have six points
#4H and 4L have negative regression lines, which might suggest that bulrush cover increases
#for all plots as cover increases except in those two plots where the bulrush cover decreases
#but the regressions are not significantly different from 0
#on ones where observed data is parallel to the regression line, that would suggest we could substract
#the control data and use that as the response 
#in general, they are sitting on the reference line so are not different from the control
#if we had more data, that might suggest an absolute change - just compute the difference
#could also compute the ratio but that doesn't work if you have a 0 
#all of this might be confounded because we actually want a beta
#but if we stay on gaussian and add a log transformation, that would be equivalent to the log transformation of the ratio
#but can't take the log if I have 0s, would need to add a small increment - could also do that with the ratio
#if I subtract, I couldn't use a beta or a log transform - but differences do start to get more normal
#in this case, given the scale and cover and 0s, a ratio is problematic, I can do an absolute difference but I can't log or beta so it might not fit the assumptions
#if I use the control as a covariate, then the model is looking at my graph here, but we need to figure out if everything fits the same rules or if we fit different lines
#could theoretically use the covariate where bulrush_c is the covariate and see if it interacts with group or density, which could be 10 different regression lines and tehre's not enough data for that 
#best that i can do might be put in the covariate by itself without an interaction
#look at covariate by itself or with an interaction wtih group 
#center the covariate
