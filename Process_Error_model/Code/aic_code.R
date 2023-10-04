# Model with all species and all four treatments ####

load("main_dfs.RData")

library(ggplot2)
library(magrittr)
library(dplyr)
library(gtools)


## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species != "PHAU") %>%
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Species,Density, Phrag_Presence) #put same species next to each other

#but I have to change the name of the extra BICE and SCAM
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat[683, 2] <- 4
native.dat[687, 2] <- 4
native.dat[691, 2] <- 4
native.dat[695, 2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the species
#I need to do the numbers differently here because BICE has 16 and SCAM has 8 
#species go hw, hwo, lw, lwo
species.vec1 <- rep(1, 4)
species.vec2 <- rep(2, 2)
species.vec3 <- rep(3:56, each = 3)
species.vec4 <- rep(57, 2)
species.vec5 <- rep(58, 4)
species.vec6 <- rep(59:72, each = 3)
species.vec <- c(species.vec1, species.vec2, species.vec3, species.vec4,
                 species.vec5, species.vec6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 216, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 216, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 216, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 216, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)

#make a vector of two different possible ln0 values for the high producers and the low producers
#low will be BOMA, JUAR, JUGE, JUTO, SCAC, SCAM, SCPU
ln0.vec <- c(rep(1, 12), rep(2, 12), rep(1, 60), rep(2, 36), rep(1, 36),
             rep(2, 36), rep(1, 24))


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, 
                       r12, r13, r14, r15, r16, r17, r18,
                       r19, r20, r21, r22, r23, r24, r25, 
                       r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36,
                       r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, 
                       r48, r49, r50, r51, r52, r53, r54,
                       r55, r56, r57, r58, r59, r60, r61, 
                       r62, r63, r64, r65, r66, r67, r68, r69, r70, r71, r72,
                       obs,n01, n02, species.vec, ln0.vec){
  rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
            r11, r12, r13, r14, r15, r16, r17, r18, 
            r19, r20, r21, r22, r23, r24, r25, r26, r27, r28,
            r29, r30, r31, r32, r33, r34, r35, r36,
            r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, 
            r48, r49, r50, r51, r52, r53, r54,
            r55, r56, r57, r58, r59, r60, r61, 
            r62, r63, r64, r65, r66, r67, r68, r69, r70, r71, r72)
  
  n0 <- c(n01, n02)
  
  dims <- dim(obs)
  ntubs <- dims[1]
  ts <- dims[2]
  
  Nout <- matrix(0, nrow = ntubs, ncol = ts)
  for(i in 1:ntubs){
    Nout[i,1]<-n0[ln0.vec[i]]*(1+rvec[species.vec[i]]*(1-n0[ln0.vec[i]]/.995))
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
multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, r5 = .9, 
             r6 = .2, r7 = .7, r8 = .6,r9 = .9, r10 = .1, 
             r11 = .1, r12 = .1, r13 = .1, r14 = .1, 
             r15 = .1, r16 = .1, r17 = .1, r18 = .1, r19 =.1, 
             r20=.1, r21=.1, r22=.1, r23=.1, r24=.1, r25=.1,
             r26=.1, r27=.1, r28=.1, r29=.1,r30=.1, r31=.1, 
             r32=.1, r33=.1, r34=.1, r35=.1, r36=.1, r37=.1, 
             r38=.1, r39=.1, r40=.1, r41=.1, r42=.1, r43=.1, r44=.1, 
             r45=.1, r46=.1, r47=.1, r48=.1, r49=.1, r50=.1, r51=.1, 
             r52=.1, r53=.1, r54=.1,r55=.1, r56=.1, r57=.1, r58=.1, 
             r59=.1, r60=.1, r61=.1, r62=.1, r63=.1, r64=.1, r65=.1, 
             r66=.1, r67=.1, r68=.1, r69=.1, r70=.1, r71=.1, r72=.1,
             species.vec = species.vec,
             obs = native.mat, n01 = .1, n02 = 0.01, ln0.vec = ln0.vec)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9,
                           lr10, lr11, lr12, lr13, lr14, lr15, lr16, lr17, lr18,
                           lr19, lr20, lr21, lr22, lr23, lr24, lr25, lr26, lr27,
                           lr28, lr29, lr30, lr31, lr32, lr33, lr34, lr35, lr36,
                           lr37, lr38, lr39, lr40, lr41, lr42, lr43, lr44, lr45,
                           lr46, lr47, lr48, lr49, lr50, lr51, lr52, lr53, lr54,
                           lr55, lr56, lr57, lr58, lr59, lr60, lr61, lr62, lr63,
                           lr64, lr65, lr66, lr67, lr68, lr69, lr70, lr71, lr72,
                           species.vec, ln0.vec, 
                           obs,ln01, ln02, lsd){
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
  r11 <- exp(lr11)
  r12 <- exp(lr12)
  r13 <- exp(lr13)
  r14 <- exp(lr14)
  r15 <- exp(lr15)
  r16 <- exp(lr16)
  r17 <- exp(lr17)
  r18 <- exp(lr18)
  r19 <- exp(lr19)
  r20 <- exp(lr20)
  r21 <- exp(lr21)
  r22 <- exp(lr22)
  r23 <- exp(lr23)
  r24 <- exp(lr24)
  r25 <- exp(lr25)
  r26 <- exp(lr26)
  r27 <- exp(lr27)
  r28 <- exp(lr28)
  r29 <- exp(lr29)
  r30 <- exp(lr30)
  r31 <- exp(lr31)
  r32 <- exp(lr32)
  r33 <- exp(lr33)
  r34 <- exp(lr34)
  r35 <- exp(lr35)
  r36 <- exp(lr36)
  r37<-exp(lr37)
  r38<-exp(lr38)
  r39<-exp(lr39)
  r40<-exp(lr40)
  r41 <- exp(lr41)
  r42 <- exp(lr42)
  r43 <- exp(lr43)
  r44 <- exp(lr44)
  r45 <- exp(lr45)
  r46 <- exp(lr46)
  r47 <- exp(lr47)
  r48 <- exp(lr48)
  r49 <- exp(lr49)
  r50 <- exp(lr50)
  r51 <- exp(lr51)
  r52 <- exp(lr52)
  r53 <- exp(lr53)
  r54 <- exp(lr54)
  r55 <- exp(lr55)
  r56 <- exp(lr56)
  r57 <- exp(lr57)
  r58 <- exp(lr58)
  r59 <- exp(lr59)
  r60 <- exp(lr60)
  r61 <- exp(lr61)
  r62 <- exp(lr62)
  r63 <- exp(lr63)
  r64 <- exp(lr64)
  r65 <- exp(lr65)
  r66 <- exp(lr66)
  r67 <- exp(lr67)
  r68 <- exp(lr68)
  r69 <- exp(lr69)
  r70 <- exp(lr70)
  r71 <- exp(lr71)
  r72 <- exp(lr72)
  
  s <-exp(lsd)
  n01 <- exp(ln01)
  n02 <- exp(ln02)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6, r7=r7,r8=r8, 
                      r9=r9, r10=r10,r11=r11, r12=r12, r13=r13, r14=r14, 
                      r15=r15, r16=r16, r17=r17, r18=r18, r19=r19, r20=r20,
                      r21=r21, 
                      r22=r22, r23=r23, r24=r24, r25=r25,r26=r26,
                      r27=r27, r28=r28, r29=r29, r30=r30, r31=r31, r32=r32,
                      r33=r33, r34=r34, r35=r35, r36=r36,
                      r37, r38, r39, r40, r41, r42, r43, r44, r45, r46, r47, 
                      r48, r49, r50, r51, r52, r53, r54,
                      r55, r56, r57, r58, r59, r60, r61, 
                      r62, r63, r64, r65, r66, r67, r68, r69, r70, r71, r72,
                      obs=obs,species.vec = species.vec, n01 = n01, n02 = n02, ln0.vec = ln0.vec)
  
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
nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
                 lr4 = log(.4),lr5 = log(.4),lr6 = log(.4),
                 lr7 = log(.4),lr8 = log(.4),lr9 = log(.4), lr10 = log(.4), 
                 lr11 = log(.4), lr12 = log(.4), lr13 = log(.4), lr14 = log(.4), lr15 = log(.4), 
                 lr16 = log(.4), lr17 = log(.4), lr18 = log(.4), lr19 = log(.4),
                 lr20 = log(.4), lr21 = log(.4), lr22 = log(.4), lr23 = log(.4),
                 lr24 = log(.4), lr25 = log(.4),lr26 = log(.4), lr27 = log(.4),
                 lr28 = log(.4), lr29 = log(.4), lr30 = log(.4), lr31 = log(.4),
                 lr32 = log(.4), lr33 = log(.4), lr34 = log(.4), lr35 = log(.4),lr36 = log(.4),
                 lr37 = log(.4), lr38 = log(.4),lr39 = log(.4),
                 lr40 = log(.4),lr41 = log(.4),lr42 = log(.4),
                 lr43 = log(.4),lr44 = log(.4),lr45 = log(.4), lr46 = log(.4), 
                 lr47 = log(.4), lr48 = log(.4), lr49 = log(.4), lr50 = log(.4), lr51 = log(.4), 
                 lr52 = log(.4), lr53 = log(.4), lr54 = log(.4), lr55 = log(.4),
                 lr56 = log(.4), lr57 = log(.4), lr58 = log(.4), lr59 = log(.4),
                 lr60 = log(.4), lr61 = log(.4),lr62 = log(.4), lr63 = log(.4),
                 lr64 = log(.4), lr65 = log(.4), lr66 = log(.4), lr67 = log(.4),
                 lr68 = log(.4), lr69 = log(.4), lr70 = log(.4), lr71 = log(.4),lr72 = log(.4),
                 ln01 = log(.01), ln02 = log(0.001), ln0.vec = ln0.vec,
                 obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.2),
                 lr2 = log(.3),
                 lr3 = log(.3),
                 lr4 = log(.2),
                 lr5=log(.03),
                 lr6 = log(.02),
                 lr7 = log(.001),
                 lr8 = log(.03),
                 lr9 = log(.09),
                 lr10 = log(.07),
                 lr11 = log(.03),
                 lr12 = log(.08),
                 lr13 = log(.3),
                 lr14 = log(.2),
                 lr15 = log(.2),
                 lr16 = log(.2),
                 lr17 = log(.25),
                 lr18 = log(.2),
                 lr19 = log(.16),
                 lr20 = log(.2),
                 lr21 = log(.14),
                 lr22 = log(.14),
                 lr23 = log(.1),
                 lr24 = log(.06),
                 lr25 = log(.3),
                 lr26 = log(.2),
                 lr27 = log(.15),
                 lr28 = log(.2),
                 lr29 = log(.0002),
                 lr30 = log(.03),
                 lr31 = log(.07),
                 lr32 = log(.002),
                 lr33 = log(.01),
                 lr34 = log(.0009),
                 lr35 = log(.03),
                 lr36 = log(.02),
                 lr37=log(.03),
                 lr38= log(.007),
                 lr39 = log(.02),
                 lr40= log(.001),
                 lr41=log(.1),
                 lr42= log(.2),
                 lr43= log(.01),
                 lr44= log(.1),
                 lr45= log(.09),
                 lr46 = log(.06),
                 lr47 = log(.06),
                 lr48 = log(.002),
                 lr49 = log(.3),
                 lr50 = log(.3),
                 lr51 = log(.2),
                 lr52 = log(.2),
                 lr53 = log(.008),
                 lr54 = log(.05),
                 lr55 = log(.03),
                 lr56 = log(.0006),
                 lr57 = log(.04),
                 lr58 = log(.03),
                 lr59 = log(.02),
                 lr60 = log(.003),
                 lr61 = log(.05),
                 lr62 = log(.008),
                 lr63 = log(.001),
                 lr64 = log(.04),
                 lr65 = log(.2),
                 lr66 = log(.2),
                 lr67 = log(.1),
                 lr68 = log(.00001),
                 lr69 = log(.2),
                 lr70 = log(.2),
                 lr71 = log(.15),
                 lr72 = log(.2),
                 lsd = log(.3),
                 ln01 = log(0.02),
                 ln02 = log(0.02))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, ln0.vec = ln0.vec, method = "SANN")

# Find MLE parameter estimates
fit_mp<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_mp<-exp(coef(fit_mp)) 
cfs_mp

## Predict historical dynamics from MLE parameter estimates####


pred_mp<-multi.func.p(r1 = cfs_mp[1], r2 = cfs_mp[2], r3 = cfs_mp[3],
                      r4 = cfs_mp[4], r5 = cfs_mp[5], r6 = cfs_mp[6],
                      r7 = cfs_mp[7], r8 = cfs_mp[8], r9 = cfs_mp[9],
                      r10 = cfs_mp[10], r11 = cfs_mp[11], r12 = cfs_mp[12],
                      r13 = cfs_mp[13], r14 = cfs_mp[14], r15 = cfs_mp[15],
                      r16 = cfs_mp[16], r17 = cfs_mp[17], r18 = cfs_mp[18],
                      r19 = cfs_mp[19], r20 = cfs_mp[20], r21 = cfs_mp[21],
                      r22 = cfs_mp[22], r23 = cfs_mp[23], r24 = cfs_mp[24],
                      r25 = cfs_mp[25], r26 = cfs_mp[26], r27 = cfs_mp[27],
                      r28 = cfs_mp[28], r29 = cfs_mp[29], r30 = cfs_mp[30],
                      r31 = cfs_mp[31], r32 = cfs_mp[32], r33 = cfs_mp[33],
                      r34 = cfs_mp[34], r35 = cfs_mp[35], r36 = cfs_mp[36],
                      r37 = cfs_mp[37], r38 = cfs_mp[38], r39 = cfs_mp[39],
                      r40 = cfs_mp[40], r41 = cfs_mp[41], r42 = cfs_mp[42],
                      r43 = cfs_mp[43], r44 = cfs_mp[44], r45 = cfs_mp[45],
                      r46 = cfs_mp[46], r47 = cfs_mp[47], r48 = cfs_mp[48],
                      r49 = cfs_mp[49], r50 = cfs_mp[50], r51 = cfs_mp[51],
                      r52 = cfs_mp[52], r53 = cfs_mp[53], r54 = cfs_mp[54],
                      r55 = cfs_mp[55], r56 = cfs_mp[56], r57 = cfs_mp[57],
                      r58 = cfs_mp[58], r59 = cfs_mp[59], r60 = cfs_mp[60],
                      r61 = cfs_mp[61], r62 = cfs_mp[62], r63 = cfs_mp[63],
                      r64 = cfs_mp[64], r65 = cfs_mp[65], r66 = cfs_mp[66],
                      r67 = cfs_mp[67], r68 = cfs_mp[68], r69 = cfs_mp[69],
                      r70 = cfs_mp[70], r71 = cfs_mp[71], r72 = cfs_mp[72],
                      n01 = cfs_mp[73], n02 = cfs_mp[74], 
                      obs = native.mat, species.vec = species.vec, ln0.vec = ln0.vec)
plot(native.mat, pred_mp, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_mp)))


# Model without phrag, only density ####

## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species != "PHAU") %>%
  select(Species, Block, Density, Phrag_Presence, Date_Cleaned, Cover.Native)  %>%
  arrange(Species,Density, Phrag_Presence) #put same species next to each other

#but I have to change the name of the extra BICE and SCAM
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat[683, 2] <- 4
native.dat[687, 2] <- 4
native.dat[691, 2] <- 4
native.dat[695, 2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Density, Phrag_Presence, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the species
#I need to do the numbers differently here because BICE has 16 and SCAM has 8 
#species go hw, hwo, lw, lwo

species.vec <- rep(1:36, each = 6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 216, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 216, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 216, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 216, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, 
                       r12, r13, r14, r15, r16, r17, r18,
                       r19, r20, r21, r22, r23, r24, r25, 
                       r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36,
                       obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
            r11, r12, r13, r14, r15, r16, r17, r18, 
            r19, r20, r21, r22, r23, r24, r25, r26, r27, r28,
            r29, r30, r31, r32, r33, r34, r35, r36)
  
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
multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, r5 = .9, 
             r6 = .2, r7 = .7, r8 = .6,r9 = .9, r10 = .1, 
             r11 = .1, r12 = .1, r13 = .1, r14 = .1, 
             r15 = .1, r16 = .1, r17 = .1, r18 = .1, r19 =.1, 
             r20=.1, r21=.1, r22=.1, r23=.1, r24=.1, r25=.1,
             r26=.1, r27=.1, r28=.1, r29=.1,r30=.1, r31=.1, 
             r32=.1, r33=.1, r34=.1, r35=.1, r36=.1,
             species.vec = species.vec,
             obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9,
                           lr10, lr11, lr12, lr13, lr14, lr15, lr16, lr17, lr18,
                           lr19, lr20, lr21, lr22, lr23, lr24, lr25, lr26, lr27,
                           lr28, lr29, lr30, lr31, lr32, lr33, lr34, lr35, lr36,
                           species.vec,
                           obs,ln0,lsd){
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
  r11 <- exp(lr11)
  r12 <- exp(lr12)
  r13 <- exp(lr13)
  r14 <- exp(lr14)
  r15 <- exp(lr15)
  r16 <- exp(lr16)
  r17 <- exp(lr17)
  r18 <- exp(lr18)
  r19 <- exp(lr19)
  r20 <- exp(lr20)
  r21 <- exp(lr21)
  r22 <- exp(lr22)
  r23 <- exp(lr23)
  r24 <- exp(lr24)
  r25 <- exp(lr25)
  r26 <- exp(lr26)
  r27 <- exp(lr27)
  r28 <- exp(lr28)
  r29 <- exp(lr29)
  r30 <- exp(lr30)
  r31 <- exp(lr31)
  r32 <- exp(lr32)
  r33 <- exp(lr33)
  r34 <- exp(lr34)
  r35 <- exp(lr35)
  r36 <- exp(lr36)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6, r7=r7,r8=r8, 
                      r9=r9, r10=r10,r11=r11, r12=r12, r13=r13, r14=r14, 
                      r15=r15, r16=r16, r17=r17, r18=r18, r19=r19, r20=r20,
                      r21=r21, 
                      r22=r22, r23=r23, r24=r24, r25=r25,r26=r26,
                      r27=r27, r28=r28, r29=r29, r30=r30, r31=r31, r32=r32,
                      r33=r33, r34=r34, r35=r35, r36=r36,
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
nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
                 lr4 = log(.4),lr5 = log(.4),lr6 = log(.4),
                 lr7 = log(.4),lr8 = log(.4),lr9 = log(.4), lr10 = log(.4), 
                 lr11 = log(.4), lr12 = log(.4), lr13 = log(.4), lr14 = log(.4), lr15 = log(.4), 
                 lr16 = log(.4), lr17 = log(.4), lr18 = log(.4), lr19 = log(.4),
                 lr20 = log(.4), lr21 = log(.4), lr22 = log(.4), lr23 = log(.4),
                 lr24 = log(.4), lr25 = log(.4),lr26 = log(.4), lr27 = log(.4),
                 lr28 = log(.4), lr29 = log(.4), lr30 = log(.4), lr31 = log(.4),
                 lr32 = log(.4), lr33 = log(.4), lr34 = log(.4), lr35 = log(.4),lr36 = log(.4),
                 ln0 = log(.01),
                 obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.2),
                 lr2 = log(.3),
                 lr3 = log(.3),
                 lr4 = log(.2),
                 lr5=log(.03),
                 lr6 = log(.02),
                 lr7 = log(.001),
                 lr8 = log(.03),
                 lr9 = log(.09),
                 lr10 = log(.07),
                 lr11 = log(.03),
                 lr12 = log(.08),
                 lr13 = log(.3),
                 lr14 = log(.2),
                 lr15 = log(.2),
                 lr16 = log(.2),
                 lr17 = log(.25),
                 lr18 = log(.2),
                 lr19 = log(.16),
                 lr20 = log(.2),
                 lr21 = log(.14),
                 lr22 = log(.14),
                 lr23 = log(.1),
                 lr24 = log(.06),
                 lr25 = log(.3),
                 lr26 = log(.2),
                 lr27 = log(.15),
                 lr28 = log(.2),
                 lr29 = log(.0002),
                 lr30 = log(.03),
                 lr31 = log(.07),
                 lr32 = log(.002),
                 lr33 = log(.01),
                 lr34 = log(.0009),
                 lr35 = log(.03),
                 lr36 = log(.02),
                 lsd = log(.3),
                 ln0 = log(0.02))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_np<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")
# # store MLE parameter estimates
cfs_np<-exp(coef(fit_np)) 
cfs_np

## Predict historical dynamics from MLE parameter estimates####


pred_np<-multi.func.p(r1 = cfs_np[1], r2 = cfs_np[2], r3 = cfs_np[3],
                      r4 = cfs_np[4], r5 = cfs_np[5], r6 = cfs_np[6],
                      r7 = cfs_np[7], r8 = cfs_np[8], r9 = cfs_np[9],
                      r10 = cfs_np[10], r11 = cfs_np[11], r12 = cfs_np[12],
                      r13 = cfs_np[13], r14 = cfs_np[14], r15 = cfs_np[15],
                      r16 = cfs_np[16], r17 = cfs_np[17], r18 = cfs_np[18],
                      r19 = cfs_np[19], r20 = cfs_np[20], r21 = cfs_np[21],
                      r22 = cfs_np[22], r23 = cfs_np[23], r24 = cfs_np[24],
                      r25 = cfs_np[25], r26 = cfs_np[26], r27 = cfs_np[27],
                      r28 = cfs_np[28], r29 = cfs_np[29], r30 = cfs_np[30],
                      r31 = cfs_np[31], r32 = cfs_np[32], r33 = cfs_np[33],
                      r34 = cfs_np[34], r35 = cfs_np[35], r36 = cfs_np[36],
                      n0 = cfs_np[37],
                      obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_np, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_np)))

# Model without density, only phrag ####

## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species != "PHAU") %>%
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Species, Phrag_Presence, Density) #put same species next to each other

#but I have to change the name of the extra BICE and SCAM
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat[695, 2] <- 4
native.dat[699, 2] <- 4
native.dat[703, 2] <- 4
native.dat[707, 2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the species
#I need to do the numbers differently here because BICE has 16 and SCAM has 8 
#species go hw, hwo, lw, lwo
species.vec1 <- rep(1, 7)
species.vec2 <- rep(2, 5)
species.vec3 <- rep(3:28, each = 6)
species.vec4 <- rep(29, 5)
species.vec5 <- rep(30, 7)
species.vec6 <- rep(31:36, each = 6)
species.vec <- c(species.vec1, species.vec2, species.vec3, species.vec4,
                 species.vec5, species.vec6)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 216, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 216, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 216, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 216, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, 
                       r12, r13, r14, r15, r16, r17, r18,
                       r19, r20, r21, r22, r23, r24, r25, 
                       r26, r27, r28, r29, r30, r31, r32, r33, r34, r35, r36,
                       obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
            r11, r12, r13, r14, r15, r16, r17, r18, 
            r19, r20, r21, r22, r23, r24, r25, r26, r27, r28,
            r29, r30, r31, r32, r33, r34, r35, r36)
  
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
multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, r5 = .9, 
             r6 = .2, r7 = .7, r8 = .6,r9 = .9, r10 = .1, 
             r11 = .1, r12 = .1, r13 = .1, r14 = .1, 
             r15 = .1, r16 = .1, r17 = .1, r18 = .1, r19 =.1, 
             r20=.1, r21=.1, r22=.1, r23=.1, r24=.1, r25=.1,
             r26=.1, r27=.1, r28=.1, r29=.1,r30=.1, r31=.1, 
             r32=.1, r33=.1, r34=.1, r35=.1, r36=.1,
             species.vec = species.vec,
             obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9,
                           lr10, lr11, lr12, lr13, lr14, lr15, lr16, lr17, lr18,
                           lr19, lr20, lr21, lr22, lr23, lr24, lr25, lr26, lr27,
                           lr28, lr29, lr30, lr31, lr32, lr33, lr34, lr35, lr36,
                           species.vec,
                           obs,ln0,lsd){
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
  r11 <- exp(lr11)
  r12 <- exp(lr12)
  r13 <- exp(lr13)
  r14 <- exp(lr14)
  r15 <- exp(lr15)
  r16 <- exp(lr16)
  r17 <- exp(lr17)
  r18 <- exp(lr18)
  r19 <- exp(lr19)
  r20 <- exp(lr20)
  r21 <- exp(lr21)
  r22 <- exp(lr22)
  r23 <- exp(lr23)
  r24 <- exp(lr24)
  r25 <- exp(lr25)
  r26 <- exp(lr26)
  r27 <- exp(lr27)
  r28 <- exp(lr28)
  r29 <- exp(lr29)
  r30 <- exp(lr30)
  r31 <- exp(lr31)
  r32 <- exp(lr32)
  r33 <- exp(lr33)
  r34 <- exp(lr34)
  r35 <- exp(lr35)
  r36 <- exp(lr36)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6, r7=r7,r8=r8, 
                      r9=r9, r10=r10,r11=r11, r12=r12, r13=r13, r14=r14, 
                      r15=r15, r16=r16, r17=r17, r18=r18, r19=r19, r20=r20,
                      r21=r21, 
                      r22=r22, r23=r23, r24=r24, r25=r25,r26=r26,
                      r27=r27, r28=r28, r29=r29, r30=r30, r31=r31, r32=r32,
                      r33=r33, r34=r34, r35=r35, r36=r36,
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
nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
                 lr4 = log(.4),lr5 = log(.4),lr6 = log(.4),
                 lr7 = log(.4),lr8 = log(.4),lr9 = log(.4), lr10 = log(.4), 
                 lr11 = log(.4), lr12 = log(.4), lr13 = log(.4), lr14 = log(.4), lr15 = log(.4), 
                 lr16 = log(.4), lr17 = log(.4), lr18 = log(.4), lr19 = log(.4),
                 lr20 = log(.4), lr21 = log(.4), lr22 = log(.4), lr23 = log(.4),
                 lr24 = log(.4), lr25 = log(.4),lr26 = log(.4), lr27 = log(.4),
                 lr28 = log(.4), lr29 = log(.4), lr30 = log(.4), lr31 = log(.4),
                 lr32 = log(.4), lr33 = log(.4), lr34 = log(.4), lr35 = log(.4),lr36 = log(.4),
                 ln0 = log(.01),
                 obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.2),
                 lr2 = log(.2),
                 lr3 = log(.02),
                 lr4 = log(.02),
                 lr5=log(.1),
                 lr6 = log(.1),
                 lr7 = log(.2),
                 lr8 = log(.2),
                 lr9 = log(.2),
                 lr10 = log(.2),
                 lr11 = log(.1),
                 lr12 = log(.1),
                 lr13 = log(.2),
                 lr14 = log(.2),
                 lr15 = log(.001),
                 lr16 = log(.001),
                 lr17 = log(.001),
                 lr18 = log(.001),
                 lr19 = log(.01),
                 lr20 = log(.01),
                 lr21 = log(.1),
                 lr22 = log(.1),
                 lr23 = log(.06),
                 lr24 = log(.06),
                 lr25 = log(.2),
                 lr26 = log(.2),
                 lr27 = log(.001),
                 lr28 = log(.001),
                 lr29 = log(.001),
                 lr30 = log(.001),
                 lr31 = log(.02),
                 lr32 = log(.02),
                 lr33 = log(.01),
                 lr34 = log(.01),
                 lr35 = log(.2),
                 lr36 = log(.2),
                 lsd = log(.3),
                 ln0 = log(0.02))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_nd<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# # store MLE parameter estimates
cfs_nd<-exp(coef(fit_nd)) 
cfs_nd

## Predict historical dynamics from MLE parameter estimates####


pred_nd<-multi.func.p(r1 = cfs_nd[1], r2 = cfs_nd[2], r3 = cfs_nd[3],
                      r4 = cfs_nd[4], r5 = cfs_nd[5], r6 = cfs_nd[6],
                      r7 = cfs_nd[7], r8 = cfs_nd[8], r9 = cfs_nd[9],
                      r10 = cfs_nd[10], r11 = cfs_nd[11], r12 = cfs_nd[12],
                      r13 = cfs_nd[13], r14 = cfs_nd[14], r15 = cfs_nd[15],
                      r16 = cfs_nd[16], r17 = cfs_nd[17], r18 = cfs_nd[18],
                      r19 = cfs_nd[19], r20 = cfs_nd[20], r21 = cfs_nd[21],
                      r22 = cfs_nd[22], r23 = cfs_nd[23], r24 = cfs_nd[24],
                      r25 = cfs_nd[25], r26 = cfs_nd[26], r27 = cfs_nd[27],
                      r28 = cfs_nd[28], r29 = cfs_nd[29], r30 = cfs_nd[30],
                      r31 = cfs_nd[31], r32 = cfs_nd[32], r33 = cfs_nd[33],
                      r34 = cfs_nd[34], r35 = cfs_nd[35], r36 = cfs_nd[36],
                      n0 = cfs_nd[37],
                      obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_nd, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_nd)))

# Model with only species ####

## Make a new dataframe that is in a better format####

#only select columns that I need for the analysis
native.dat <- greenhouse %>%
  filter(Species != "PHAU") %>%
  select(Species, Block, Phrag_Presence, Density, Date_Cleaned, Cover.Native)  %>%
  arrange(Species, Phrag_Presence, Density) #put same species next to each other

#but I have to change the name of the extra BICE and SCAM
native.dat$Block <- as.numeric(native.dat$Block)
native.dat[3,2] <- 4
native.dat[7,2] <- 4
native.dat[11,2] <- 4
native.dat[15,2] <- 4

native.dat[695, 2] <- 4
native.dat[699, 2] <- 4
native.dat[703, 2] <- 4
native.dat[707, 2] <- 4

native.dat$ID_Col <- with(native.dat, paste0(Species, Phrag_Presence, Density, Block))
#rearrange so date is column
native.dat <- reshape2::dcast(native.dat, ID_Col ~ Date_Cleaned, value.var = "Cover.Native")

#make vectors to keep track of the species
#I need to do the numbers differently here because BICE has 16 and SCAM has 8 
#species go hw, hwo, lw, lwo
species.vec <- rep(1:18, each = 12)

#now make a matrix
native.mat <- as.matrix(native.dat[,-1]) #make it a matrix, without the ID_Col
native.mat[is.na(native.mat)] <- 0 #make all NAs 0

native.mat[native.mat == 0] <- 0.025 #get rid of 0s

#add the extra days so that it becomes a daily timestep
first <- matrix(NA, nrow = 216, ncol=17)
second <- matrix(native.mat[,1])
third <- matrix(NA, nrow = 216, ncol = 6)
fourth <- matrix(native.mat[,2])
fifth <- matrix(NA, nrow = 216, ncol = 6)
sixth <- matrix(native.mat[,3])
seventh <- matrix(NA, nrow = 216, ncol = 6)
eighth <- matrix(native.mat[,4])

native.mat <- cbind(first, second, third, fourth, fifth, sixth, seventh, eighth)


##State Prediction Funcions: ####
#Model with single and multi r and process vs observation error


#predict the mean (expected value) of every timestep when you don't have an observation- see below
#Multi process error
multi.func.p<-function(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, r11, 
                       r12, r13, r14, r15, r16, r17, r18,
                       obs,n0, species.vec){
  rvec <- c(r1, r2, r3, r4, r5, r6, r7, r8, r9, r10, 
            r11, r12, r13, r14, r15, r16, r17, r18)
  
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
multi.func.p(r1 = .7, r2 = .9, r3 = .9, r4 = .7, r5 = .9, 
             r6 = .2, r7 = .7, r8 = .6,r9 = .9, r10 = .1, 
             r11 = .1, r12 = .1, r13 = .1, r14 = .1, 
             r15 = .1, r16 = .1, r17 = .1, r18 = .1,
             species.vec = species.vec,
             obs = native.mat, n0 = .1)


##NLL Function####


#Multi process error - for the process error model, I just need to predict a number based on the previous timstep
#I would calculate the timestep between recorded values similar to how we did with the spatial models
#then the mean is your last observed value
#variance is the variance of the last timestepx the number of time steps because the variance increases linearly with the number of timesteps out 
#tbefore putting it into dnorm, you calculate the sd from the variance (sqrt(timesteps x variance))
#qlogis of the predicted N 
nll.multi.func.p<-function(lr1, lr2, lr3, lr4, lr5, lr6, lr7, lr8, lr9,
                           lr10, lr11, lr12, lr13, lr14, lr15, lr16, lr17, lr18,
                           species.vec,
                           obs,ln0,lsd){
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
  r11 <- exp(lr11)
  r12 <- exp(lr12)
  r13 <- exp(lr13)
  r14 <- exp(lr14)
  r15 <- exp(lr15)
  r16 <- exp(lr16)
  r17 <- exp(lr17)
  r18 <- exp(lr18)
  
  s <-exp(lsd)
  n0 <- exp(ln0)
  
  predN<-multi.func.p(r1=r1,r2=r2,r3=r3,r4=r4,r5=r5,r6=r6, r7=r7,r8=r8, 
                      r9=r9, r10=r10,r11=r11, r12=r12, r13=r13, r14=r14, 
                      r15=r15, r16=r16, r17=r17, r18=r18,
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
nll.multi.func.p(lr1 = log(.4), lr2 = log(.4),lr3 = log(.4),
                 lr4 = log(.4),lr5 = log(.4),lr6 = log(.4),
                 lr7 = log(.4),lr8 = log(.4),lr9 = log(.4), lr10 = log(.4), 
                 lr11 = log(.4), lr12 = log(.4), lr13 = log(.4), lr14 = log(.4), lr15 = log(.4), 
                 lr16 = log(.4), lr17 = log(.4), lr18 = log(.4),
                 ln0 = log(.01),
                 obs = native.mat, lsd = log(.05), species.vec = species.vec)



##Find MLE parameters####


library(bbmle)

# Create list of starting guesses for the multi model
start.list<-list(lr1=log(.2),
                 lr2 = log(.2),
                 lr3 = log(.02),
                 lr4 = log(.02),
                 lr5=log(.1),
                 lr6 = log(.1),
                 lr7 = log(.2),
                 lr8 = log(.2),
                 lr9 = log(.2),
                 lr10 = log(.2),
                 lr11 = log(.1),
                 lr12 = log(.1),
                 lr13 = log(.2),
                 lr14 = log(.2),
                 lr15 = log(.001),
                 lr16 = log(.001),
                 lr17 = log(.001),
                 lr18 = log(.001),
                 lsd = log(.3),
                 ln0 = log(0.02))
# Create list of observed data for model
data.list<-list(obs=native.mat, species.vec = species.vec, method = "SANN")

# Find MLE parameter estimates
fit_n<-mle2(minuslogl=nll.multi.func.p,start=start.list,data=data.list, method = "SANN")

# # store MLE parameter estimates
cfs_n<-exp(coef(fit_n)) 
cfs_n

## Predict historical dynamics from MLE parameter estimates####
pred_n<-multi.func.p(r1 = cfs_n[1], r2 = cfs_n[2], r3 = cfs_n[3],
                      r4 = cfs_n[4], r5 = cfs_n[5], r6 = cfs_n[6],
                      r7 = cfs_n[7], r8 = cfs_n[8], r9 = cfs_n[9],
                      r10 = cfs_n[10], r11 = cfs_n[11], r12 = cfs_n[12],
                      r13 = cfs_n[13], r14 = cfs_n[14], r15 = cfs_n[15],
                      r16 = cfs_n[16], r17 = cfs_n[17], r18 = cfs_n[18],
                      n0 = cfs_n[37],
                      obs = native.mat, species.vec = species.vec)
plot(native.mat, pred_n, xlab = "Observed", ylab = "Predicted",pch = 16, las = 1, ylim = c(0,1))
abline(0, 1) # Add 1:1 line on figure indicating perfect fit
summary(lm(as.vector(native.mat) ~ as.vector(pred_n)))


# AIC Calculations ####
#fit_mp = the model with all four treatments
#fit_nd = model without densoty
#fit_np = model without phrag
#fit_n = model without density or phrag


# Calculate AIC for each model
AICmp <- AIC(fit_mp)
AICnd <- AIC(fit_nd)
AICnp <- AIC(fit_np)
AICn <- AIC(fit_n)

#AICmp<--2*logLik(fit_mp)+2*length(cfs_mp)

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
AICwnd <- rAICnd/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwnp <- rAICnp/sum(rAICmp,rAICnd, rAICnp, rAICn)
AICwn <- rAICn/sum(rAICmp,rAICnd, rAICnp, rAICn)

#plot the AIC weight
par(mar = c(4, 4, 2, 2))
allAIC<-c(AICwmp, AICwnd, AICwnp, AICwn)
allAICw<-allAIC[order(allAIC)]
plot(allAICw,xaxt="n",ylab="AIC weight",
     xlab="Model",pch=16,las=1)
axis(side=1,at=seq(1, 4),labels=c("All", "No Density", "No Phrag", "None")[order(allAIC)])


