# * * Comparison of yst differences between CO and other treatments----
emm_y.t_p <- emmeans(m1.best, pairwise ~ yst.f | treatment, by = "sp_phase.f")
# contrast-methods in emmeans, or "Contrast families"
# # Comparison of yst difference for all treatment pairs
test <- contrast(emm_y.t_p, interaction = c(yst.f="consec", treatment="pairwise"), 
                 by="sp_phase.f", adjust="tukey")
test[[1]]
# Comparison of yst difference between CO and each other treatment
test.adj <- contrast(emm_y.t_p, interaction = c(yst.f="consec", treatment="trt.vs.ctrl1"), 
                     by="sp_phase.f", adjust="tukey")
test.adj[[1]]

# * * Comparison of yst differences between CO and other treatments----
emm_y.t_p <- emmeans(mdf.m1, pairwise ~ Density | Phrag_Presence, by = "Species")
# contrast-methods in emmeans, or "Contrast families"
# # Comparison of yst difference for all treatment pairs
test <- contrast(emm_y.t_p, interaction = c(Density="consec", Phrag_Presence="pairwise"), 
                 by="Species", adjust="tukey")
test[[1]]
# Comparison of yst difference between CO and each other treatment
test.adj <- contrast(emm_y.t_p, interaction = c(yst.f="consec", treatment="trt.vs.ctrl1"), 
                     by="sp_phase.f", adjust="tukey")
test.adj[[1]]



