#try with an individual species####

load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)

# Model with all treatments ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:4, each = 3)

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
#                  lr4 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

## Predict historical dynamics from MLE parameter estimates####


# pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
#                       r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

# Model with only density ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:2, each = 6)



#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)

  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)
# 


##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

## Predict historical dynamics from MLE parameter estimates####
pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
                      n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

# Model with only phrag ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:2, each = 6)


#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

# #test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)
# 


##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

## Predict historical dynamics from MLE parameter estimates####
pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
                      n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

# Model with nothing ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCPU") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Phrag_Presence, Density) #put likes together

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")


#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r = .7,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05))



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# # Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

## Predict historical dynamics from MLE parameter estimates####

pred_n<-multi.func.p(r = cfs_n[1],
                      n0 = cfs_n[2], obs = native.mat)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))

# AIC calculations ####
# Calculate AIC for each model
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)


# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd


#plot the AIC weight
# par(mar = c(4, 4, 2, 2))
# allAIC<-c(AICwmp, AICwnd, AICwnp, AICwn)
# allAICw<-allAIC[order(allAIC)]
# plot(allAICw,xaxt="n",ylab="AIC weight",
#      xlab="Model",pch=16,las=1)
# axis(side=1,at=seq(1, 4),labels=c("All", "Phrag Only", "Density Only", "None")[order(allAIC)])
# 
# 

# BICE models ####
##Graph the species to determine that they are following logistic growth####

load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)

# Model with all treatments ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, each = 4)
species.vec2 <- rep(2, each = 2)
species.vec3 <- rep(3:4, each = 3)
species.vec <- c(species.vec1, species.vec2, species.vec3)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
#                  lr4 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)
# 


##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

## Predict historical dynamics from MLE parameter estimates####


# pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
#                       r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

# Model with only density ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

## Predict historical dynamics from MLE parameter estimates####


pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
                      n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))
#R2 = .70

# Model with only phrag ####
## Make a new dataframe that is in a better format####

native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, 7)
species.vec2 <- rep(2, 5)
species.vec <- c(species.vec1, species.vec2)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

# #test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)
# 


##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

## Predict historical dynamics from MLE parameter estimates####


# pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
#                       n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

# Model with nothing ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis

native.dat <- greenhouse %>%
  filter(Species == "BICE") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r = .7,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05))
# 


##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

## Predict historical dynamics from MLE parameter estimates####

# pred_n<-multi.func.p(r = cfs_n[1],
#                       n0 = cfs_n[2], obs = native.mat)
# plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))

# AIC calculations ####
# Calculate AIC for each model
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)


# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd

# SCAM models ####
##Graph the species to determine that they are following logistic growth####

load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)

# Model with all treatments ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, each = 2)
species.vec2 <- rep(2, each = 4)
species.vec3 <- rep(3:4, each = 3)
species.vec <- c(species.vec1, species.vec2, species.vec3)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4,  obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  r3<-exp(lr3)
  r4<-exp(lr4)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
#                  lr4 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lr3 = log(.1),
                 lr4 = log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

## Predict historical dynamics from MLE parameter estimates####


# pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
#                       r4 = cfs_mp[4], n0 = cfs_mp[5], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))

# Model with only density ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec <- rep(1:2, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error

multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)
# 

##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

## Predict historical dynamics from MLE parameter estimates####


# pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2],
#                       n0 = cfs_np[3], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

# Model with only phrag ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the treatment
#1 = HW, 2 = HWO, 3 = LW, 4 = LWO
species.vec1 <- rep(1, each = 5)
species.vec2 <- rep(2, each = 7)
species.vec <- c(species.vec1, species.vec2)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)



##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, obs,n0, species.vec){
  rvec <- c(r1, r2)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+rvec[species.vec[i]]*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+rvec[species.vec[j]]*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+rvec[species.vec[j]]*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

# #test
# multi.func.p(r1 = .7, r2 = .9, species.vec = species.vec,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, species.vec,
                           obs,ln0, lsd){
  r1<-exp(lr1)
  r2<-exp(lr2)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,
                      obs=obs,species.vec = species.vec, n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.1),
                 lr2 = log(.2),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# # Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

## Predict historical dynamics from MLE parameter estimates####


# pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2],
#                       n0 = cfs_nd[3], obs = native.mat, species.vec = species.vec)
# plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
# abline(0, 1) # Add 1:1 line on figure indicating perfect fit
# summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

# Model with nothing ####
## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species == "SCAM") %>% 
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Density, Phrag_Presence) #put likes together

#change the block for some
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[11,2] <- 4
native.dat[15,2] <- 4
native.dat[19,2] <- 4
native.dat[23,2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))

#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")


#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 12, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 12, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 12, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 12, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)



##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r, obs, n0){
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0*(1+r*(1-n0/.995))
  }
  
  for(i in 2:ts){
    for(j in 1:ntubs){
      if(!is.na(obs[j, i-1])) {
        Nout[j, i]<-obs[j, i-1]*(1+r*(1-obs[j, i-1]/.995))
      }
      if(is.na(obs[j, i-1])){ #if it is an NA, do off the last predicted
        Nout[j, i] <- Nout[j, i-1]*(1+r*(1-Nout[j, i-1]/.995))
      }
    }
  }
  return(Nout)
}

#test
# multi.func.p(r = .7,
#              obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr,
                           obs,ln0, lsd){
  r<-exp(lr)
  
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r=r,
                      obs=obs,n0 = n0)
  
  # obs2 <- obs[!is.na(obs)]
  # predN2 <- predN[!is.na(obs)]
  
  # param <- dampack::beta_params(mean = predN2, sigma = sd)
  # alpha <- param[1]
  # alpha <- unlist(alpha)
  # beta <- param[2]
  # beta <- unlist(beta)
  
  predN[predN==0]<-.01
  predN[predN==1]<-.99
  # print(obs)
  # print(predN)
  liks<-0
  
  for(j in 1:nrow(obs)){
    lastobs <- 0
    for(i in 1:ncol(obs)){
      if(!is.na(obs[j, i])){
        tbtwn<-i-lastobs
        # print(tbtwn)
        liks<-c(liks, dnorm(x=qlogis(obs[j, i]),mean=qlogis(predN[j, i]),sd=sqrt(tbtwn*s^2)))
        lastobs<-i
        # print(liks)
      }
    }
  }
  
  
  
  # likes <- dbeta(x=obs2,shape1 = alpha, shape2 = beta)
  # likes <- dnorm(x=qlogis(obs2), mean=plogis(predN2), sd = sd)
  
  nll<--1*sum(log(liks[-1]))  
  return(nll)
}

#test
# nll.multi.func.p(lr = log(.4),
#                  ln0 = log(.01), obs = native.mat, lsd = log(.05))



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr=log(.1),
                 lsd = log(.05),
                 ln0 = log(0.001))
# Create list of observed data for model
data.list<-list(obs=native.mat, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

## Predict historical dynamics from MLE parameter estimates####

pred_n<-multi.func.p(r = cfs_n[1],
                     n0 = cfs_n[2], obs = native.mat)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))
#R2 = .25

# AIC calculations ####
# Calculate AIC for each model
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)


# Calculate delta AIC for each model
dAICmp <- AICmp-min(AICmp, AICnd, AICnp, AICn)
dAICnd <- AICnd-min(AICmp, AICnd, AICnp, AICn)
dAICnp <- AICnp-min(AICmp, AICnd, AICnp, AICn)
dAICn <- AICn-min(AICmp, AICnd, AICnp, AICn)

# Calculate relative likelihood for each model
rAICmp <- exp(-.5*dAICmp)
rAICnd <- exp(-.5*dAICnd)
rAICnp <- exp(-.5*dAICnp)
rAICn <- exp(-.5*dAICn)

# Calculate AIC weights for each model
AICwmp <- rAICmp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)

AICwmp
AICwn
AICwnp
AICwnd

#Results for each species on the Graph Summaries page 