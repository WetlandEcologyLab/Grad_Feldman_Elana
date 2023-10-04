# Quantile regression

library(quantreg)
library(tidyverse)
#load("r_values_table.RData")
values <- read.csv("r_values.csv")
load("main_dfs.RData")

#For this analysis, we want to find the quantiles of phrag growth by growth rate

#Let's clean up the r values sheet we need to use
dat <- values %>% 
  select(1:4, 7, 10, 13, 16, 17) %>% 
  pivot_longer(4:7,
               names_to = "Tub",
               values_to = "r_value") %>% 
  separate(col = "Tub",
           into = c("Density", "Phrag_Presence"),
           sep = 1) 

#First, we need to make a table that includes both of these values
dat2 <- greenhouse %>%
  filter(Date_Cleaned == "2022-05-16",
         Phrag_Presence == "W") %>%
  select(Species, Density, Phrag_Presence, Block, Cover.Phrag) %>%
  left_join(dat, by = c("Species", "Density", "Phrag_Presence"))

#Now make the calculations to see the relationship between the two
#Choose the quantiles by changing the tau

rqfit <- rq(Cover.Phrag ~ r_value, tau = c(.05, .25, .5, .75, .95),data = dat2)
summary(rqfit) 

#Now graph them
#base r graph
color <- c("#ffcccc", "#ff9999", "#ff6666", "#ff3333", "#ff0000")
library(gridExtra)
#jpeg("quantile_regression.jpeg", height = 400, width = 400)
plot(Cover.Phrag ~ r_value, data = dat2,
     xlab = "Growth rate (r)",
     ylab = substitute(paste("Proportional ", italic("Phragmites "), "cover"))
)
for (j in 1:ncol(rqfit$coefficients)) {
  abline(coef(rqfit)[, j], col = color[j])
}
legend(x = "topright", legend = c(0.05, 0.25, 0.5, 0.75, 0.95), 
       col = color, lty = 1, title = "Quantiles")
#dev.off()

#built in function to show the change in quantile coefficients and the confidence intervals
#red lines is the least squares estimate and confidence intervals
#black dots are slope coefficient at the given quantile
#jpeg("qreg_ci.jpeg", height = 400, width = 500)
plot(summary(rqfit), parm = "r_value",
     xlab = "Quantile", 
     ylab = "Slope Coefficient")
#dev.off()

#ggplot graph 
color <- c("#1e90ff", "#192bc2")
dat2 %>% 
  ggplot(aes(x = r_value, y = Cover.Phrag))+
  geom_point() +
  geom_quantile(quantiles = c(0.5, 0.95),
                aes(color = factor(..quantile..)),
                size = 1) +
  xlab("Intrinsic Rate of Growth (r)") +
  ylab("Proportional *P.australis* Cover") +
  labs(color = "Quantiles") +
  theme(axis.title.y = ggtext::element_markdown()) +
  scale_color_manual(values = color) +
  ylim(0, 0.5)
ggsave("quant_reg_ggplot.jpeg")
