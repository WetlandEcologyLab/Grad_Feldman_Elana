
library(readxl)
library(tidyverse)
library(emmeans)

gh <- read_xlsx("Dupuy-greenhouse16.xlsx")

gh16 <- gh %>%
  mutate(tGroup = if_else(TRT == "ctrl", "0", "1"))

with(gh16, table(TRT, tGroup))

ggplot(gh16,
       aes(x = TRT, y = All)) +
  geom_jitter() +
  facet_wrap(~ TIME)

options(contrasts = c("contr.sum", "contr.poly"))

# Control versus notControl----
m1 <- glm(All ~ tGroup,
          data = gh16,
          family = poisson)
summary(m1)
car::Anova(m1)
emmeans(m1, ~ tGroup)

# Two way factorial without interaction; no control----
m2 <- glm(All ~ TRT + TIME,
          data = filter(gh16, tGroup == "1"),
          family = poisson)
summary(m2)
car::Anova(m2)
car::Anova(m2, type = 3)
emmip(m2, TRT ~ TIME)
emmip(m2, TRT ~ TIME, type = "response", CIs = T)
emmeans(m2, ~ TRT + TIME)

# Two way factorial without interaction plus control----
m3 <- glm(All ~ tGroup + tGroup/TRT + tGroup/TIME,
          data = gh16,
          family = poisson)
summary(m3)
car::Anova(m3, type = 2)
emmeans(m3, ~ TRT + TIME)

# Two way factorial with interaction; no control----
m4 <- glm(All ~ TRT * TIME,
          data = filter(gh16, tGroup == "1"),
          family = poisson)
summary(m4)
car::Anova(m4)
car::Anova(m4, type = 3)
emmip(m4, TRT ~ TIME)
emmip(m4, TRT ~ TIME, type = "response", CIs = T)
emmeans(m4, ~ TRT + TIME)

# Two way factorial with interaction plus control----
m5 <- glm(All ~ tGroup + tGroup/TRT * tGroup/TIME,
          data = gh16,
          family = poisson)
summary(m5)
emmip(m5, TRT ~ TIME)
emmip(m5, TRT ~ TIME, type = "response", CIs = T)
emmeans(m5, ~ TRT + TIME)

# Probably easiest to do m1 and m4, at least if design is balanced



ggplot(gh16,
       aes(x = interaction(TRT, TIME), y = All)) +
  geom_boxplot()
