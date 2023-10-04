
# for the likelihood profiles, just choose a couple species to look at - technically should do both models for all
#try bounding results in the nll functions if changing the timesteps doesn't work

#for graphs:
#could cut it down to 1 process error line by running the model with the projected r and the mean of the three replicates
#could put all the species on the same panel by making three different graphs (one for each model) and a different line for all species

####################################################
#Graph the species to determine that they are following logistic growth
####################################################
load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)


####################################################
# Make a new dataframe that is in a better format
####################################################
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "HENU" | Species == "RUMA" | Species == "EPCI" | 
           Species == "EUMA" | Species == "SYCI" | Species == "SOCA" |
           Species == "MUAS" | Species == "PUNU" | Species == "DISP" | Species == "EUOC") %>%
  filter(Density == "L" & Phrag_Presence == "W")%>%
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Species) #put same species next to each other

#save this for graphing later
native.dat2 <- greenhouse %>%
  filter(Species == "HENU" | Species == "RUMA" | Species == "EPCI" | 
           Species == "EUMA" | Species == "SYCI" | Species == "SOCA" |
           Species == "MUAS" | Species == "PUNU" | Species == "DISP"| Species == "EUOC") %>%
  filter(Density == "L" & Phrag_Presence == "W")%>%
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Species) #put same species next to each other

native.dat$ID_Col <- with(native.dat, paste0(Species, Block))

#make vectors to keep track of the species
#each species gets a number 1 - 13, so I repeat those numbers 3times for the 3 blocks
species.vec <- rep(1:10, each = 3)

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 30, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 30, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 30, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 30, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

###############################################################
#State Prediction Funcions: 
#Model with single and multi r and process vs observation error
###############################################################
#Single obs error
single.func<-function(r,obs, n0){
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  Nout[,1]<-n0*(1+r*(1-n0/.995))
  for(i in 2:ts){
    for(j in 1:ntubs){
      Nout[j, i]<-Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
    }
  } 
  return(Nout)
}

#test
single.func(.5, native.mat, n0 = .1)

#Multi obs error
multi.func<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, obs, species.vec, n0){
rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10)

dims <- dim(obs)
ntubs <- dims[1]
ts <- dims[2]

Nout <- matrix(0, nrow = ntubs, ncol = ts)
for(i in 1:ntubs){
  Nout[i,1] <- n0 * (1 + (rvec[species.vec[i]] * (1 - (n0/.995))))
}


for(i in 2:ts){
  for(j in 1:ntubs){
    Nout[j, i]<-Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
  }
}

return(Nout)

}

#test
multi.func(r1 = .5, r2 = .6, r3 = .4, r4 = .6, r5 = .6, 
           r6 = .5, r7 = .5, r8 = .5, r9 = .5, r10 = .5,
           obs = native.mat, species.vec = species.vec, n0 = 0.0001)

#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
# multi.func.p<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13, obs,n0){
#   rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, r12, r13)
# 
#   dims <- dim(obs)
#   ntubs <- dims[1]
#   ts <- dims[2]
# 
#   Nout <- matrix(0, nrow = ntubs, ncol = ts)
#   for(i in 1:ntubs){
#     Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
#   }
# 
#   for(i in 2:ts){
#     for(j in 1:ntubs){
#           if(!is.na(obs[i-1])) {
#       Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[i]]*(1-obs[j, i-1]/.995))
#           }
#           if(is.na(obs[i-1])){ #if it is an NA, do off the last predicted
#             Nout[i] <- Nout[i-1]*(1+rvec[species.vec[i]]*(1-obs[j, i-1]/.995))
#           }
#     }
#   }
#   return(Nout)
# }
# # 
#test
# multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, r5 = .9, r6 = .2, r7 = .7, r8 = .6,
#              r9 = .9, r10 = .1, r11 = .1, r12 = .8, r13 = .9,
#              obs = native.mat, n0 = .1)

############################
#NLL Function
############################
#Single obs error
nll.single.func<-function(lr,obs,lsd, ln0){ #logitK so between 0 and 1
  r<-exp(lr)
  sd <- exp(lsd)
  n0 <- exp(ln0)
  
  predN<-single.func(r=r,obs=obs, n0 = n0)
  
  obs2 <- obs[!is.na(obs)]
  predN2 <- predN[!is.na(obs)]

param <- dampack::beta_params(mean = predN2, sigma = sd)
alpha <- param[1]
alpha <- unlist(alpha)
beta <- param[2]
beta <- unlist(beta)

likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)

  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(likes)) 
  return(nll)
}

#test
nll.single.func(lr = log(0.4), obs = native.mat, lsd = log(0.05), ln0 = log(0.001))

#could try boundingfrom the fourth lecture
#could use plogis or qlogis to find the logit of all my cover data and then run that with a normal distribution
#dnorm of logit(obs)

#Multi obs error
nll.multi.func<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9, lr10, 
                         obs, ln0, lsd, species.vec){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  r5 <- exp(lr5)
  r6 <- exp(lr6)
  r7 <- exp(lr7)
  r8 <- exp(lr8)
  r9 <- exp(lr9)
  r10 <- exp(lr10)
  
  sd<-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6, r7=r7,r8=r8, r9=r9, r10=r10,
                    obs=obs,species.vec = species.vec, n0 = n0)
  
  obs2 <- obs[!is.na(obs)]
  predN2 <- predN[!is.na(obs)]
  
  param <- dampack::beta_params(mean = predN2, sigma = sd)
  alpha <- param[1]
  alpha <- unlist(alpha)
  beta <- param[2]
  beta <- unlist(beta)
  
  likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(likes)) 
  return(nll)
}

#test
nll.multi.func(lr1 = log(.5), lr2 = log(.5), lr3 = log(.4), lr4 = log(.6),
               lr5 = log(.4), lr6 = log(.4), lr7 = log(.7), lr8 = log(.3), 
               lr9 = log(.4), lr10 = log(.4),
               lsd = log(.05), ln0 = log(0.0001),
               obs = native.mat, species.vec = species.vec)


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
# nll.multi.func.p<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9, 
#                            lr10, lr11, lr12, lr13, 
#                            obs,ln0, lsd){
#   r1<-exp(lr1)
#   r2<-exp(lr2)
#   r3<-exp(lr3)
#   r4<-exp(lr4)
#   r5<-exp(lr5)
#   r6<-exp(lr6)
#   r7<-exp(lr7)
#   r8<-exp(lr8)
#   r9<-exp(lr9)
#   r10<-exp(lr10)
#   r11<-exp(lr11)
#   r12<-exp(lr12)
#   r13<-exp(lr13)
#   n0<-exp(ln0)
#   sd<-exp(lsd)
#   
#   obs2 <- obs[!is.na(obs)]
#   predN2 <- predN[!is.na(obs)]
#   
#   predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6,
#                       r7=r7,r8=r8,r9=r9,r10=r10,r11=r11,r12=r12,r13=r13,
#                       n0=n0,obs=obs)
      
      # predN[predN == 0] <- 0.025
      # predN[predN == 1] <- .995
      # lastobs <-0
      # likes <- 0
      # for(j in 1:nrow(obs))
      # for(i in 1:ncol(obs)){
      #   if(!is.na(obs[j,i])){
      #     tbtwn <- i - lastobs
      #     likes <- c(liks,dnorm(x = qlogis(obs[j,i]), mean = qlogis(predN[j,i]), sd = sqrt(tbtwn*s^2)))
      #     lastobs <- i
      #   }
      # }
      # likes <- dnorm(x = qlogis(obs[!is.na(obs)]), mean = qlogis(predN), )
#   
#   param <- dampack::beta_params(mean = predN2, sigma = sd)
#   alpha <- param[1]
#   alpha <- unlist(alpha)
#   beta <- param[2]
#   beta <- unlist(beta)
#   
#   likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
#   
  # nll<--1*sum(log(likes[-1])) #remove the first 0 because you can't take the log of 0
  # return(nll)
# }
# 
# #test
# nll.multi.func.p(lr = log(.4), lK = log(1), lN0 = log(.01), obs = native.mat, lsd = log(.05))
# 

#####################
#Find MLE parameters
#####################

library(bbmle)

#Single obs error
# Create list of starting guesses for the single model
start.list<-list(lr=log(.1),
                 lsd = log(0.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat)

# Find MLE parameter estimates
fit_s<-mle2(minuslogl=nll.single.func,start=start.list,data=data.list)
# store MLE parameter estimates
cfs_s<-exp(coef(fit_s)) 
cfs_s

#Multi obs error
# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lr5=log(.2),
                 lr6 = log(.1),
                 lr7 = log(.1),
                 lr8 = log(.1),
                 lr9 = log(.1),
                 lr10 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_m<-mle2(minuslogl=nll.multi.func,start=start.list,data=data.list)
# store MLE parameter estimates
cfs_m<-exp(coef(fit_m)) 
cfs_m

# #Multi process error
# # Find MLE parameter estimates
# fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
# cfs_mp<-exp(coef(fit_mp)) 

##############################################################
# Predict historical dynamics from MLE parameter estimates
##############################################################

pred_s<-single.func(r=cfs_s[1],obs=native.mat, n0 = cfs_s[3])
pred_m <- multi.func(r1 = cfs_m[1], r2 = cfs_m[2], r3 = cfs_m[3], r4 = cfs_m[4],r5 = cfs_m[5], 
                     r6 = cfs_m[6], r7 = cfs_m[7], r8 = cfs_m[8], r9 = cfs_m[9], r10 = cfs_m[10],
                     obs = native.mat, species.vec = species.vec, n0 = cfs_m[11])
# pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
#                       r4 = cfs_mp[4], r5 = cfs_mp[5], r6 = cfs_mp[6],
#                       r7 = cfs_mp[7], r8 = cfs_mp[8], r9 = cfs_mp[9],
#                       r10 = cfs_mp[10], r11 = cfs_mp[11], r12 = cfs_mp[12],
#                       r13 = cfs_mp[13], n0 = cfs_mp[14], obs = native.mat)

########################################################
# Plot to compare
########################################################

par(mfrow = c(1,2), mar = c(4, 4, 2, 2))

plot(native.mat, pred_s, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_s))) 
mtext("R2 = 0.3", side=3)

plot(native.mat, pred_m, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_m))) 
mtext("R2 = 0.87", side=3)


# plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_mp))) 
# mtext("R2 = 0.77", side=3)

#################################
#Compare the two models using AIC
#################################
# Calculate AIC for each model
AICs<--2*logLik(fit_s)+2*length(cfs_s)
AICm<--2*logLik(fit_m)+2*length(cfs_m)
#AICmp<--2*logLik(fit_mp)+2*length(cfs_mp)

# Calculate delta AIC for each model
dAICs<-AICs-min(AICs,AICm)
dAICm<-AICm-min(AICs,AICm)

# dAICs<-AICs-min(AICs,AICm, AICmp)
# dAICm<-AICm-min(AICs,AICm, AICmp)
# dAICmp<-AICm-min(AICs,AICm, AICmp)

# Calculate relative likelihood for each model
rAICs<-exp(-.5*dAICs)
rAICm<-exp(-.5*dAICm)
# rAICmp<-exp(-.5*dAICmp)

# Calculate AIC weights for each model
AICws<-rAICs/sum(rAICs,rAICm)
AICwm<-rAICm/sum(rAICm,rAICs)

# AICws<-rAICs/sum(rAICs,rAICm,rAICmp)
# AICwm<-rAICm/sum(rAICm,rAICs,rAICmp)
# AICwmp<-rAICmp/sum(rAICm,rAICs,rAICmp)

#plot the AIC weight
par(mar = c(4, 4, 2, 2))
allAIC<-c(AICws,AICwm)
allAICw<-allAIC[order(allAIC)]
plot(allAICw,xaxt="n",ylab="AIC weight",
     xlab="Model",pch=16,las=1)
axis(side=1,at=seq(1, 2),labels=c("Single", "Multi")[order(allAIC)])

# allAIC<-c(AICws,AICwm,AICwmp)
# allAICw<-allAIC[order(allAIC)]
# plot(allAICw,xaxt="n",ylab="AIC weight",
#      xlab="Model",pch=16,las=1)
# axis(side=1,at=seq(1,3),labels=c("Single", "Multi", "MultiP")[order(allAIC)])

##############################################
#Find the 95% CI for rs with the better model
##############################################
##RUMA - highest r
# Create vector of alues to examine
r8vec <- seq(.05, .2, length.out=100)

# Estimate nll for models with r5 fixed at each value in the vector above
nlls.r8 <- rep(NA, length(r8vec)) #place to store the nlls

#loop through all values of r5vec, fit the model with r5 fixed, store NLL

# Estimate parameters with mle2
for (i in 1:length(r8vec)) {
  fittmp <- mle2(minuslogl = nll.multi.func,
                 start = list(lr1 = log(cfs_m[1]),
                              lr3 = log(cfs_m[3]),
                              lr4 = log(cfs_m[4]),
                              lr2 = log(cfs_m[2]),
                              lr6 = log(cfs_m[6]),
                              lr7 = log(cfs_m[7]),
                              lr5 = log(cfs_m[5]),
                              lr9 = log(cfs_m[9]),
                              lr10 = log(cfs_m[10]),
                              ln0=log(cfs_m[11]),
                              lsd = log(cfs_m[12])),
                 fixed  = list(lr8 = log(r8vec[i])),
                 data = list(obs = native.mat, species.vec = species.vec),
                 method="SANN")
  
  nlls.r8[i] <- -1*logLik(fittmp)
  print(i)
}

# Plot difference between fixed-s0 nll values and MLE nll value
plot(r8vec, nlls.r8 - -1*logLik(fit_m), ylab = "NLL - MLE", xlab = "alpha", ylim = c(0,3))

# Find values of r where the nll is 1.92 greater than the MLE nll estimate
abline(h=1.92)

target.nll<-min(nlls.r8)+1.92 

#lower limit
r8.test<-r8vec[1:which.min(abs(r8vec-cfs_m[8]))]
nll.test<-nlls.r8[1:which.min(abs(r8vec-cfs_m[8]))]
lwr.r8<-approx(y=r8.test,x=nll.test,xout=target.nll)$y

#upper limit
r8.test<-r8vec[which.min(abs(r8vec-cfs_m[8])):length(r8vec)]
nll.test<-nlls.r8[which.min(abs(r8vec-cfs_m[8])):length(nlls.r8)]
upr.r8<-approx(y=r8.test,x=nll.test,xout=target.nll)$y

lwr.r8
upr.r8

##DISP - lowest r
# Create vector of values to examine
r1vec <- seq(.01, .09, length.out=100)

# Estimate nll for models with r5 fixed at each value in the vector above
nlls.r1 <- rep(NA, length(r1vec)) #place to store the nlls

#loop through all values of r5vec, fit the model with r5 fixed, store NLL

# Estimate parameters with mle2
for (i in 1:length(r1vec)) {
  fittmp <- mle2(minuslogl = nll.multi.func,
                 start = list(lr4 = log(cfs_m[4]),
                              lr3 = log(cfs_m[3]),
                              lr6 = log(cfs_m[6]),
                              lr2 = log(cfs_m[2]),
                              lr5 = log(cfs_m[5]),
                              lr7 = log(cfs_m[7]),
                              lr8 = log(cfs_m[8]),
                              lr9 = log(cfs_m[9]),
                              lr10 = log(cfs_m[10]),
                              ln0=log(cfs_m[11]),
                              lsd = log(cfs_m[12])),
                 fixed  = list(lr1 = log(r1vec[i])),
                 data = list(obs = native.mat, species.vec = species.vec),
                 method="SANN")
  
  nlls.r1[i] <- -1*logLik(fittmp)
  print(i)
}

#Plot difference between fixed-s0 nll values and MLE nll value
plot(r1vec, nlls.r1 - -1*logLik(fit_m), ylab = "NLL - MLE", xlab = "alpha", ylim = c(0, 2))
#added ylim to see it better but not sure if that's good or not

# Find values of r where the nll is 1.92 greater than the MLE nll estimate
abline(h=1.92)

target.nll<-min(nlls.r1)+1.92 

#lower limit
r1.test<-r1vec[1:which.min(abs(r1vec-cfs_m[1]))]
nll.test<-nlls.r1[1:which.min(abs(r1vec-cfs_m[1]))]
lwr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

#upper limit
r1.test<-r1vec[which.min(abs(r1vec-cfs_m[1])):length(r1vec)]
nll.test<-nlls.r1[which.min(abs(r1vec-cfs_m[1])):length(nlls.r1)]
upr.r1<-approx(y=r1.test,x=nll.test,xout=target.nll)$y

lwr.r1
upr.r1

##EUOC - middle species
# Create vector of values to examine
r4vec <- seq(0.02, .14, length.out=100)

# Estimate nll for models with r5 fixed at each value in the vector above
nlls.r4 <- rep(NA, length(r4vec)) #place to store the nlls

#loop through all values of r5vec, fit the model with r5 fixed, store NLL

# Estimate parameters with mle2
for (i in 1:length(r4vec)) {
  fittmp <- mle2(minuslogl = nll.multi.func,
                 start = list(lr1 = log(cfs_m[1]),
                              lr3 = log(cfs_m[3]),
                              lr6 = log(cfs_m[6]),
                              lr2 = log(cfs_m[2]),
                              lr5 = log(cfs_m[5]),
                              lr7 = log(cfs_m[7]),
                              lr8 = log(cfs_m[8]),
                              lr9 = log(cfs_m[9]),
                              lr10 = log(cfs_m[10]),
                              ln0=log(cfs_m[11]),
                              lsd = log(cfs_m[12])),
                 fixed  = list(lr4 = log(r4vec[i])),
                 data = list(obs = native.mat, species.vec = species.vec),
                 method="SANN")
  
  nlls.r4[i] <- -1*logLik(fittmp)
  print(i)
}

#Plot difference between fixed-s0 nll values and MLE nll value
plot(r4vec, nlls.r4 - -1*logLik(fit_m), ylab = "NLL - MLE", xlab = "alpha")

# Find values of r where the nll is 1.92 greater than the MLE nll estimate
abline(h=1.92)

target.nll<-min(nlls.r4)+1.92 

#lower limit
r4.test<-r4vec[1:which.min(abs(r4vec-cfs_m[4]))]
nll.test<-nlls.r4[1:which.min(abs(r4vec-cfs_m[4]))]
lwr.r4<-approx(y=r4.test,x=nll.test,xout=target.nll)$y

#upper limit
r4.test<-r4vec[which.min(abs(r4vec-cfs_m[4])):length(r4vec)]
nll.test<-nlls.r4[which.min(abs(r4vec-cfs_m[4])):length(nlls.r4)]
upr.r4<-approx(y=r4.test,x=nll.test,xout=target.nll)$y

lwr.r4
upr.r4

##n0
# Create vector of values to examine
r11vec <- seq(0.01, .05, length.out=100)

# Estimate nll for models with r5 fixed at each value in the vector above
nlls.r11 <- rep(NA, length(r11vec)) #place to store the nlls

#loop through all values of r5vec, fit the model with r5 fixed, store NLL

# Estimate parameters with mle2
for (i in 1:length(r11vec)) {
  fittmp <- mle2(minuslogl = nll.multi.func,
                 start = list(lr1 = log(cfs_m[1]),
                              lr3 = log(cfs_m[3]),
                              lr6 = log(cfs_m[6]),
                              lr2 = log(cfs_m[2]),
                              lr5 = log(cfs_m[5]),
                              lr7 = log(cfs_m[7]),
                              lr8 = log(cfs_m[8]),
                              lr9 = log(cfs_m[9]),
                              lr10 = log(cfs_m[10]),
                              lr4=log(cfs_m[4]),
                              lsd = log(cfs_m[12])),
                 fixed  = list(ln0 = log(r11vec[i])),
                 data = list(obs = native.mat, species.vec = species.vec),
                 method="SANN")
  
  nlls.r11[i] <- -1*logLik(fittmp)
  print(i)
}

#Plot difference between fixed-s0 nll values and MLE nll value
plot(r11vec, nlls.r11 - -1*logLik(fit_m), ylab = "NLL - MLE", xlab = "alpha")

# Find values of r where the nll is 1.92 greater than the MLE nll estimate
abline(h=1.92)

target.nll<-min(nlls.r11)+1.92 

#lower limit
r11.test<-r11vec[1:which.min(abs(r11vec-cfs_m[11]))]
nll.test<-nlls.r11[1:which.min(abs(r11vec-cfs_m[11]))]
lwr.r11<-approx(y=r11.test,x=nll.test,xout=target.nll)$y

#upper limit
r11.test<-r11vec[which.min(abs(r11vec-cfs_m[11])):length(r11vec)]
nll.test<-nlls.r11[which.min(abs(r11vec-cfs_m[11])):length(nlls.r11)]
upr.r11<-approx(y=r11.test,x=nll.test,xout=target.nll)$y

lwr.r11
upr.r11

#######
#Plots
#######
#initial plot of data for logistic regression
# 
# cover_native <- greenhouse %>%
#   filter(Species != "PHAU") %>%
#   ggplot(aes(x = Date_Cleaned, y = Cover.Native, color = Phrag_Presence, shape = Density)) +
#   #using the means of the blocks
#   stat_summary(aes(group = interaction(Density, Phrag_Presence)),
#                fun = mean, geom = "point", size = 2) +
#   #error bars added
#   stat_summary(aes(group = interaction(Density, Phrag_Presence), width = .5),
#                fun.data = mean_se, geom = "errorbar") +
#   #add a line to connect the dates
#   stat_summary(aes(group = interaction(Density, Phrag_Presence)),
#                fun = mean, geom = "line") +
#   facet_wrap(~Species) +
#   theme(legend.position = 'bottom',
#         axis.text.x = element_text(angle = 45, hjust = 0.9)) +
#   labs(x = "Date", y = "Native Cover (%)", color = "Phragmites Presence", shape = "Density") +
#   scale_color_hue(labels = c('Present', 'Absent')) + #change the legend labels
#   scale_shape(labels = c("High", "Low"))
# 
# cover_native

col.vec <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99", "#756bb1", "#fdbf6f",
             "#ff7f00", "#cab2d6", "#6a3d9a", "#feb24c", "#b15928", "gray")
species <- c("Distichlis spicata", "Epilobium ciliatum", "Eutrochium maculatum",
             "Euthamia occidentalis","Helianthus nuttallii", 
             "Muhlenbergia asperifolia", "Puccinellia nuttalliana", "Rumex maritimus",
             "Solidago canadensis",
             "Symphyotrichum ciliatum")#vector of the names of the species in order, spelled correctly
names.short <- c("D. spicata", "E. ciliatum", "E. maculatum",
                 "E. occidentalis","H. nuttallii", 
                 "M. asperifolia", "P. nuttalliana", "R. maritimus",
                  "S. canadensis",
                 "S. ciliatum")

native.mean <- native.dat2 %>%
  group_by(Species, Date_Cleaned) %>%
  summarise(cover.mean = mean(Cover.Native, na.rm = TRUE)) #find mean cover by date and species

par(mar = c(4, 4, 2, 8), xpd = TRUE)
plot(x = native.mean$Date_Cleaned[1:4], y = native.mean$cover.mean[1:4], col = col.vec[1], 
     type = "l", ylim = c(0, 1), ylab = "Mean Cover", xlab = "Date")
lines(x = native.mean$Date_Cleaned[5:8], y = native.mean$cover.mean[5:8], col = col.vec[2])
lines(x = native.mean$Date_Cleaned[9:12], y = native.mean$cover.mean[9:12], col = col.vec[3])
lines(x = native.mean$Date_Cleaned[13:16], y = native.mean$cover.mean[13:16], col = col.vec[4])
lines(x = native.mean$Date_Cleaned[17:20], y = native.mean$cover.mean[17:20], col = col.vec[5])
lines(x = native.mean$Date_Cleaned[21:24], y = native.mean$cover.mean[21:24], col = col.vec[6])
lines(x = native.mean$Date_Cleaned[25:28], y = native.mean$cover.mean[25:28], col = col.vec[7])
lines(x = native.mean$Date_Cleaned[29:32], y = native.mean$cover.mean[29:32], col = col.vec[8])
lines(x = native.mean$Date_Cleaned[33:36], y = native.mean$cover.mean[33:36], col = col.vec[9])
lines(x = native.mean$Date_Cleaned[37:40], y = native.mean$cover.mean[37:40], col = col.vec[10])
lines(x = native.mean$Date_Cleaned[41:44], y = native.mean$cover.mean[41:44], col = col.vec[11])
lines(x = native.mean$Date_Cleaned[45:48], y = native.mean$cover.mean[45:48], col = col.vec[12])
lines(x = native.mean$Date_Cleaned[49:52], y = native.mean$cover.mean[49:52], col = col.vec[13])

legend("topright", inset = c(-0.45, 0), col = col.vec, pch = 16,legend = species,
       cex = .65)

#different R values
m_values <- data.frame(cfs_m[1:10]) #make the r values a dataframe
colnames(m_values) <- "r" #change column name
m_values$species <- species #add the column of names
m_values$species.short <- names.short
ordered_m <- arrange(m_values, desc(r))


par(mar = c(6, 4, 2,2))
plot(ordered_m$r, ylab = "Growth rate (r)", las=1,  xaxt = "n", xlab="", ylim = c(0, .5))
axis(1, at = c(1:10), labels = FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.15, labels = ordered_m$species.short, srt = 70, pos = 3, xpd = TRUE)
#we can see that they all have different r values
#some of them are super fast growers
#some are medium growers - look into whether this matches the observations or related to some growing too fast
#Some aren't really growing much at all 


#r value by percent phrag
phrag.per <- greenhouse %>%
  filter(Species == "HENU" | Species == "RUMA" | Species == "EPCI" | 
           Species == "EUMA" | Species == "SYCI" | Species == "SOCA" |
           Species == "MUAS" | Species == "PUNU" | Species == "DISP" | Species == "EUOC") %>%
  filter(Density == "L" & Phrag_Presence == "W" & Date_Cleaned == "2022-05-16")%>%
  select(Species, Cover.Phrag) %>% #select what we need for the calculation
  group_by(Species) %>% #group by species
  summarise(avg.phrag = mean(Cover.Phrag)) #make a new column showing the avg phrag coer

phrag.per$Species <- m_values$species #make the species names the same
phrag.per$r_m <- m_values$r #add the r values


par(mar = c(4, 4, 2, 8), xpd = TRUE)
plot(y = phrag.per$avg.phrag, x = phrag.per$r_m, xlab = "r", ylab="Final Phragmites Cover",
     col = col.vec, pch = 16)

legend("topright", inset = c(-0.48, 0), col = col.vec, pch = 16,legend = phrag.per$Species,
       cex = .7)



plot(pred_s[1,],type = "l", col = "red", ylim = c(0,1), lwd = 3, ylab = "Cover", xlab = "Day")
lines(pred_m[1,], col = "blue")
lines(pred_m[4,], col = "blue")
lines(pred_m[7,], col = "blue")
lines(pred_m[10,], col = "blue")
lines(pred_m[13,], col = "blue")
lines(pred_m[16,], col = "blue")
lines(pred_m[19,], col = "blue")
lines(pred_m[22,], col = "blue")
lines(pred_m[25,], col = "blue")
lines(pred_m[28,], col = "blue")
legend("topleft", col = c("red", "blue"), pch = 16,legend = c("Single", "Multiple"))


#different models with the data
par(mfrow = c(1, 3))
plot(native.mat[1,], ylim = c(0, 1),
     xlab = "Date", ylab = "Cover", main = "Distichlis spicata")
points(native.mat[2,])
points(native.mat[3,])
lines(pred_s[1,], col = "red", type = "l")
lines(pred_m[1,], col = "blue")

plot(native.mat[10,], ylim = c(0, 1), xlab = "Date", ylab = "", main = "Euthamia occidentalis")
points(native.mat[11,])
points(native.mat[12,])
lines(pred_s[10,], col = "red")
lines(pred_m[10,], col = "blue")


plot(native.mat[22,], ylim = c(0, 1), xlab = "Date", ylab = "", main = "Rumex maritimus")
points(native.mat[23,])
points(native.mat[24,])
lines(pred_s[22,], col = "red")
lines(pred_m[22,], col = "blue")




plot(native.mat[4,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Epilobium ciliatum")
points(native.mat[5,])
points(native.mat[6,])
lines(native.mat[4,], col = "black")
lines(native.mat[5,])
lines(native.mat[6,])
lines(pred_s[4,], col = "red")
lines(pred_m[4,], col = "blue")
lines(pred_m[5,], col = "blue")
lines(pred_m[6,], col = "blue")

legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))

plot(native.mat[7,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Eupatorium maculatum")
points(native.mat[8,])
points(native.mat[9,])
lines(native.mat[7,])
lines(native.mat[8,])
lines(native.mat[9,])
lines(pred_s[7,], col = "red")
lines(pred_m[7,], col = "blue")
lines(pred_m[8,], col = "blue")
lines(pred_m[9,], col = "blue")
lines(pred_mp[7,], col = "green")
lines(pred_mp[8,], col = "green")
lines(pred_mp[9,], col = "green")
legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))


plot(native.mat[13,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Helianthus nuttalii")
points(native.mat[14,])
points(native.mat[15,])
lines(native.mat[13,])
lines(native.mat[14,])
lines(native.mat[15,])
lines(pred_s[13,], col = "red")
lines(pred_m[13,], col = "blue")
lines(pred_m[14,], col = "blue")
lines(pred_m[15,], col = "blue")
lines(pred_mp[13,], col = "green")
lines(pred_mp[14,], col = "green")
lines(pred_mp[15,], col = "green")
legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))

plot(native.mat[16,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Juncus arcticus")
points(native.mat[17,])
points(native.mat[18,])
lines(native.mat[16,])
lines(native.mat[17,])
lines(native.mat[18,])
lines(pred_s[16,], col = "red")
lines(pred_m[16,], col = "blue")
lines(pred_m[17,], col = "blue")
lines(pred_m[18,], col = "blue")
lines(pred_mp[16,], col = "green")
lines(pred_mp[17,], col = "green")
lines(pred_mp[18,], col = "green")
legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))

plot(native.mat[19,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Muhlenbergia asperifolia")
points(native.mat[20,])
points(native.mat[21,])
lines(native.mat[19,])
lines(native.mat[20,])
lines(native.mat[21,])
lines(pred_s[19,], col = "red")
lines(pred_m[19,], col = "blue")
lines(pred_m[20,], col = "blue")
lines(pred_m[21,], col = "blue")
lines(pred_mp[19,], col = "green")
lines(pred_mp[20,], col = "green")
lines(pred_mp[21,], col = "green")



plot(native.mat[25,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Rumex maritimus")
points(native.mat[26,])
points(native.mat[27,])
lines(native.mat[25,])
lines(native.mat[26,])
lines(native.mat[27,])
lines(pred_s[25,], col = "red")
lines(pred_m[25,], col = "blue")
lines(pred_m[26,], col = "blue")
lines(pred_m[27,], col = "blue")
lines(pred_mp[25,], col = "green")
lines(pred_mp[26,], col = "green")
lines(pred_mp[27,], col = "green")
legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))

plot(native.mat[28,], ylim = c(0, 1), xlab = "Date", ylab = "Cover (%)", main = "Schoenoplectus acutus")
points(native.mat[29,])
points(native.mat[30,])
lines(native.mat[28,])
lines(native.mat[29,])
lines(native.mat[30,])
lines(pred_s[28,], col = "red")
lines(pred_m[28,], col = "blue")
lines(pred_m[29,], col = "blue")
lines(pred_m[30,], col = "blue")
lines(pred_mp[28,], col = "green")
lines(pred_mp[29,], col = "green")
lines(pred_mp[30,], col = "green")
legend("top", ncol=4, col=c("black", "red", "blue", "green"), lwd=2, legend=c("Obs", "Single", "Multi O", "Multi P"))
