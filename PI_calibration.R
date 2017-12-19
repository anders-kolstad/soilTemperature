# TOP ####


# -------------------------------------------------------#

# Calibration of biomass models for point intercept data #

# -------------------------------------------------------#


# Raw data:
# 40 vegetation samples (50x50cm frames) that have been subject to point intercept
# with 49 pins, and subsequently cut at moss level, dried, sorted into growth form class 
# (see) below, and weighed. We also sampled the biomass inside the moss layer of a 10cm 
# diameter circle inside each frame and recorded the moss depth.

# The growth form classes are:

# Large herbs and ferns
# Small herbs and ferns
# Narrow leaved grass (Avenella flexuosa)
# Broad leaved grasses
# Calluna vulgaris
# Empetrum nigrum
# Broad leaved shrubs (Vaccinium spp)


# Packages ####
library(readxl)
library(dplyr)
library(lattice)  #Needed for multi-panel graphs
library(R2jags)
library(ggplot2)
library(ggthemes)
library(gridExtra)
setwd("M:\\Anders L Kolstad\\HIGHSTATS\\AllData")
source(file = "HighstatLibV10.R")  
source(file = "MCMCSupportHighstatV4.R")




# Load data ####
setwd("M:\\Anders L Kolstad\\systherb data\\PI_calibration")
dat <- read_excel("PI calibration SustHerb 2017.xlsx", 
                  sheet = "Sheet1")





# Housekeeping ####
dat$within_moss_DW_g <- as.numeric(dat$within_moss_DW_g)
dat$moss_depth <- as.numeric(dat$`Moss depth cm`)
dat <- dat[1:40,]
dat$NLS_DW_g <- rowSums(dat[,c("Calluna_DW_g", "Empetrum_DW_g")], na.rm = T)
dat$NLS_DW_g[dat$NLS_DW_g == 0] <- NA




# Standardise the number of hits as the avergae per pin:
std_grp <- c("Tall_herbs", "Short_herbs", "Narrow_leaved_shrubs",
             "Broad_leaved_shrubs", "Narrow_leaved_grasses", "Broad_leaved_grasses"
)
dat2 <- dat
dat2[,std_grp] <- dat2[,std_grp]/49
#head(dat2[,std_grp])
#head(dat[,std_grp])
dat <- dat2
rm(dat2)
# OBS, Calluna and Empetrum are still not standardised, but NLS is.


# Scale 'within moss' to be same area unit (0.25m2)
# area samples is
#pi*5*5 # = 78.53982 cm2 
#(pi*5*5)/10000 # = 0.007853982 m2
#(pi*5*5)/10000/0.25 # 0.03141593 per 50x50cm quadrat
dat$wMoss_DW <- dat$within_moss_DW_g/((pi*5*5)/10000/0.25)

# Standardise all weights to be per 1m-sq
abun_grp2 <- c("BLG_DW_g", "NLG_DW_g",
               "BLS_DW_g", "NLS_DW_g", 
               "TH_DW_g", "SH_DW_g", "wMoss_DW")
dat2 <- dat
dat2[,abun_grp2] <- dat2[,abun_grp2]*4
head(dat2[,abun_grp2])
head(dat[,abun_grp2])
dat <- dat2
rm(dat2)


# sum all species:
dat$total_hits <- rowSums(dat[,std_grp], na.rm = TRUE)
abun_grp2 <- c("BLG_DW_g", "NLG_DW_g",
               "BLS_DW_g", "NLS_DW_g", 
               "TH_DW_g", "SH_DW_g")
dat$total_bm <- rowSums(dat[,abun_grp2], na.rm = TRUE)


# -------------------------------------------------------#

# Data exploration ####

# -------------------------------------------------------#

barplot(table(dat$Site))
barplot(table(dat$Treatment))




# look at the NUMBER OF HITS (frequency) for each group:

freq_grp <- c("Tall_herbs", "Short_herbs", "Narrow_leaved_shrubs",
              "Broad_leaved_shrubs", "Narrow_leaved_grasses", "Broad_leaved_grasses",
              "Calluna vulgaris", "Empetrum nigrum")

par(mfrow = c(3,3), mar = c(2,2,7,2))
for(i in unique(freq_grp)){
  plot(select(dat, i), main = i, type = "h")
}
title("Number of hits", side = 3, line = -2, outer = TRUE)







# look at the BIOMASS (abundance) for each group:
abun_grp <- c("within_moss_DW_g", "BLG_DW_g", "NLG_DW_g",
              "BLS_DW_g", "NLS_DW_g", "TH_DW_g", "SH_DW_g",
              "Calluna_DW_g", "Empetrum_DW_g")
par(mfrow = c(3,3), mar = c(2,2,7,2))
for(i in unique(abun_grp)){
  plot(select(dat, i), main = i, type = "h")
}
title("Dry weights",  line = -2, outer = TRUE)








# -------------------------------------------------------#

# Relationships ####

# -------------------------------------------------------#


par(mfrow = c(3,3), mar = c(5,5,7,2))
plot(dat$within_moss_DW_gmsq, dat$`Moss depth cm`, main = "Within moss", xlab = "", ylab = "") # one outlier #  ylab = "depth (cm)"
plot(dat$BLG_DW_g, dat$Broad_leaved_grasses,  main = "BLG", xlab = "", ylab = "") # saturates - non-linear
plot(dat$NLG_DW_g, dat$Narrow_leaved_grasses,  main = "NLG", xlab = "", ylab = "") # saturates - non-linear
plot(dat$BLS_DW_g, dat$Broad_leaved_shrubs,  ylab = "# hits", main = "BLS", xlab = "") # quite good
plot(dat$TH_DW_g, dat$Tall_herbs,  main = "Tall herbs", xlab = "", ylab = "") # quite good
plot(dat$SH_DW_g, dat$Short_herbs, xlab = "", ylab = "", main = "Short/small herbs") # quite good, discontinous
plot(dat$Calluna_DW_g, dat$`Calluna vulgaris`, xlab = "", ylab = "", main = "Calluna vulgaris") # quite good, few points
plot(dat$Empetrum_DW_g, dat$`Empetrum nigrum`, xlab = "Dry weight", ylab = "", main = "Empetrum nigrum") # quite good
plot(dat$NLS_DW_g, dat$Narrow_leaved_shrubs, xlab = "", ylab = "", main = "NLS") # quite good

# Remove outliers, based on lab notes
dat2 <- filter(dat,
               Plot != 31 & Plot != 27)
par(mfrow = c(3,3), mar = c(5,5,7,2))
plot(dat2$within_moss_DW_gmsq, dat2$`Moss depth cm`, main = "Within moss", xlab = "", ylab = "") # one outlier #  ylab = "depth (cm)"
plot(dat2$BLG_DW_g, dat2$Broad_leaved_grasses,  main = "BLG", xlab = "", ylab = "") # saturates - non-linear
plot(dat2$NLG_DW_g, dat2$Narrow_leaved_grasses,  main = "NLG", xlab = "", ylab = "") # saturates - non-linear
plot(dat2$BLS_DW_g, dat2$Broad_leaved_shrubs,  ylab = "# hits", main = "BLS", xlab = "") # quite good
plot(dat2$TH_DW_g, dat2$Tall_herbs,  main = "Tall herbs", xlab = "", ylab = "") # quite good
plot(dat2$SH_DW_g, dat2$Short_herbs, xlab = "", ylab = "", main = "Short/small herbs") # quite good, discontinous
plot(dat2$Calluna_DW_g, dat2$`Calluna vulgaris`, xlab = "", ylab = "", main = "Calluna vulgaris") # quite good, few points
plot(dat2$Empetrum_DW_g, dat2$`Empetrum nigrum`, xlab = "Dry weight", ylab = "", main = "Empetrum nigrum") # quite good
plot(dat2$NLS_DW_g, dat2$Narrow_leaved_shrubs, xlab = "", ylab = "", main = "NLS") # quite good

par(mfrow = c(2,2))
plot(dat2$total_bm, dat2$total_hits, xlab = "Total DW", ylab = "Mean Hits per pin", main = "All species") # 
plot(dat2$within_moss_DW_gmsq, dat2$`Moss depth cm`, main = "Within moss", xlab = "Total DW", ylab = "Moss Depth (cm)") # one outlier #  ylab = "depth (cm)"
boxplot(dat2$total_bm, xlab = "Total DW", ylab = "Mean Hits per pin", main = "All species", ylim=c(0,140)) # 
boxplot(dat2$within_moss_DW_gmsq, main = "Within moss", xlab = "Total DW", ylab = "Moss Depth (cm)",ylim=c(0,140)) # one outlier #  ylab = "depth (cm)"
# Considerable amount of biomass inside the moss layer. 





par(mfrow = c(1,1), mar = c(5,5,7,2))
plot(dat2$Broad_leaved_grasses, dat2$BLG_DW_g,  main = "BLG",  pch = 20) # saturates - non-linear

# Strictly positive values. Increasing variation. 
# -> GLM, gamma distribution with identiy link


# The gamma GLM is given by:
#   Y[i]      ~ Gamma(mu[i], r)
#   E(Y[i])   = mu[i]
#   var(Y[i]) = mu[i]^2 / r
#   mu[i]     = exp(eta[i])   # for log-link!
#   eta[i]    = Covariates


#In JAGS just do this:
  # Y[i] ~  dgamma(r, lambda[i])
  # lambda[i]  <- r / mu[i]
  # log(mu[i]) <- eta[i]  
  # eta[i]     <- inprod(beta[],X[i,])

#As a mathematical equation you now have:
#   Y[i]      ~ Gamma(mu[i], r)
#   E(Y[i])   = mu[i]
#   var(Y[i]) = mu[i]^2 / r
#   mu[i]     = exp(eta[i])
#   eta[i]    = X * beta + a



# Makeing all the datasets with simpler names:
setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
names(dat2)


BLG <- filter(dat2, Broad_leaved_grasses != 0 & BLG_DW_g != 0)
BLG$std.Broad_leaved_grasses <- scale(BLG$Broad_leaved_grasses)  # scale continous covariates
BLG$c.Broad_leaved_grasses <- BLG$Broad_leaved_grasses-mean(BLG$Broad_leaved_grasses) # center continous covariates
BLG2 <- select(BLG, DW = BLG_DW_g,    # tidy dtaframe
               Hits = Broad_leaved_grasses,
               c.Hits = c.Broad_leaved_grasses,
               std.Hits =std.Broad_leaved_grasses)
BLG <- as.data.frame(BLG2)
save(BLG, file = "BLG.rda")


NLG <- filter(dat2, Narrow_leaved_grasses != 0 & NLG_DW_g != 0)
NLG <- select(NLG, DW = NLG_DW_g,    # tidy dtaframe
               Hits = Narrow_leaved_grasses)
NLG <- as.data.frame(NLG)
save(NLG, file = "NLG.rda")


BLS <- filter(dat2, Broad_leaved_shrubs != 0 & BLS_DW_g != 0)
BLS <- select(BLS, DW = BLS_DW_g,    # tidy dtaframe
              Hits = Broad_leaved_shrubs)
BLS <- as.data.frame(BLS)
save(BLS, file = "BLS.rda")


NLS <- filter(dat2, Narrow_leaved_shrubs != 0 & NLS_DW_g != 0)
NLS <- select(NLS, DW = NLS_DW_g,    # tidy dtaframe
              Hits = Narrow_leaved_shrubs)
NLS <- as.data.frame(NLS)
save(NLS, file = "NLS.rda")



TH <- filter(dat2, Tall_herbs != 0 & TH_DW_g != 0)
TH <- select(TH, DW = TH_DW_g,    # tidy dataframe
              Hits = Tall_herbs)
TH <- as.data.frame(TH)
save(TH, file = "TH.rda")


SH <- filter(dat2, Short_herbs != 0 & SH_DW_g != 0)
SH <- select(SH, DW = SH_DW_g,    # tidy dataframe
             Hits = Short_herbs)
SH <- as.data.frame(SH)
save(SH, file = "SH.rda")




# BLG ####
##Step 1: Fit the model.
#Use MCMC to estimate the parameters 




setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/BLG.rda")
names(BLG)
head(BLG)
summary(BLG)

#1. Bundle data

X_BLG <- model.matrix(~ Hits , data = BLG)   # keeping the intercept, but remember model is only valid of #hits >0
K <- ncol(X_BLG) 
X_BLG <- as.matrix(X_BLG)
X_BLG
K


# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = BLG$DW, 
                 X    = X_BLG,
                 N    = nrow(BLG),
                 K    = K       )
JAGS.data

# Step 3: Formulate JAGS modelling code - identity link
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("BLG.txt")
cat("
model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 200)}       # diffuse priors

    #1B. Priors random effects 
    # not applicable

    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)                                 # diffuse prior

    #2. Likelihood
    for (i in 1:N) {
      Y[i]        ~ dgamma(r, mu.eff[i])
      mu.eff[i]  <- r / mu[i]
      mu[i] <- eta[i] 
      eta[i] <- inprod(beta[], X[i,]) }
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
        VarY[i] <- mu[i]^2 / r
        PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
        }
         }
",fill = TRUE)
sink()








# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = runif(2, 0, 40),
    r     = runif(1, 0, 10))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "r",    # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_BLG <- jags(data       = JAGS.data,
           inits      = inits,
           parameters = params,
           model      = "BLG.txt",
           n.thin     = 10,
           n.chains   = 3,
           n.burnin   = 4000,
           n.iter     = 5000)

J_BLG  <- update(J_BLG, n.iter = 200000, n.thin = 10)  
out <- J_BLG$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c(colnames(X_BLG), "Gamma variance parameter r")
MyBUGSChains(out, 
             c(uNames("beta", K),  "r"),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # based on original covaraites
MyBUGSHist(out, 
           c(uNames("beta", K), "r"),
            PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Ok

plot(y = E1, 
     x = BLG$Hits)
abline(h = 0, lty = 2)
# Ok, same as above, just differently scaled axis





#----------------------##
# Model interpretation ####

# Calculate the fitted values.
# 1. Get the betas from the MCMC iterations
# 2. Define a grid of covariate values
# 3. Calculate the predicted values on this 
#    grid for each MCMC iteration.
# 4. Calculate the 95% credible intervals.
# 5. Plot the whole thing


# 1. Get the betas
beta.mcmc <- out$sims.list$beta  #betas
dim(beta.mcmc)  #60,000 iterations saved.
# Using these we can puzzle together the pieces.



#2A. Define a grid of covariate values
#   without extrapolation
range(BLG$Hits)

MyData <- data.frame(Hits = seq(from = min(BLG$Hits), 
                                    to = max(BLG$Hits), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    # removed exp function because there's not log-link


# Idea plot fitted values agains original value and calculate R2
plot(out$mean$mu,BLG$DW)
plot(sort(out$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out$mean$mu,BLG$DW)   # not used I don't think
(r2 <- var(out$mean$mu)/var(BLG$DW))
resids <- out$mean$mu-BLG$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))
# ref: Stan Development Team. 2016. rstanarm: Bayesian applied regression modeling via Stan. R package version 2.13.1.
# Done manually, as the package is for Stan


# Why don't we take the 2.5% and 97.5% values
# at each of the artificial covariate values?
# And plot these instead of the 3,000 lines? 
# We wrote  a small support function that does this:

L <- GetCIs(mu.mcmc)
L



# GetCIs(): Support file to calculate the posterior
# mean, se and 2.5 and 97.5 quantiles for 
# fitted values. The input needs to be 
# an observation - by - MCMC iteration object.

# Each row in L is an artificial SDI value
# The first column is the posterior mean of 
# the MCMC iterations at that specific fitted 
# value.
# The second row is the SE, the third and fourth 
# are the 2.5 and 97.5% quartiles and are used for 
# the 95% credible interval. 

# Now plot the data and draw lines for the mean,
# and 2.5% and 97.5% values:

MyData2 <- cbind(MyData,L)
head(MyData2)

# Back-standardize covariate
#MyData2$BLG_hits <- MyData$c.Hits + mean(BLG$c.Hits)






p <- ggplot() +
      geom_point(data = BLG, 
                    aes(x = Hits, 
                        y = DW))+
  geom_line(data = MyData2, 
                   aes(x = Hits, 
                       y = mean))+
  geom_ribbon(data = MyData2,
                     aes(x = Hits, 
                         ymax = up, 
                         ymin = lo), 
                     alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Broad Leaved Grasses")+
  annotate("text", x= 1, y= 120,                    # -Inf, Inf,   hjust = -2.5, vjust = 2,
           label = paste("R2 = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p

#compare with lm R2:
summary(lm(BLG$DW~BLG$Hits))
#Multiple R-squared:  0.866 - no wonder as lm maximises R2. 

est <- print(OUT1, digits = 5) 
est[2]
pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=35, y=5000,
           label = paste("mean = ", round(est[2], 4), sep = " "),
            size = 5)+
  xlab("Estimated slope")+
  xlim(10, 40)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=3, y=7500,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 4)+
  theme_few()+theme(text = element_text(size=15))
pHist_I



library(gridExtra)
grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))

ggsave(filename = "plots/BLG_model.tiff", height = 10, width = 10,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1)))




# -------------------------#
# NLG ------------------####
# -------------------------#

##Step 1: Fit the model.
#Use MCMC to estimate the parameters 




setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/NLG.rda")
names(NLG)

plot(NLG$Hits, NLG$DW)
identify(NLG$Hits, NLG$DW)
NLG[3,]
#     DW      Hits
# 3 0.28 0.3265306
# its from plot 3 and removing it is justifiable based on field/lab notes
NLG <- NLG[-3,]
summary(NLG)


#1. Bundle data

X_NLG <- model.matrix(~ Hits , data = NLG)   # keeping the intercept, but remember model is only valid of #hits >0
K <- ncol(X_NLG) 
X_NLG <- as.matrix(X_NLG)
X_NLG
K

# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = NLG$DW, 
                  X    = X_NLG,
                  N    = nrow(NLG),
                  K    = K       )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("NLG.txt")
cat("
    model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 100)}   
    
    #1B. Priors random effects 
    # not applicable
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- eta[i] 
    eta[i] <- inprod(beta[], X[i,]) }
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()








# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = runif(2, 0, 20),
    r     = runif(1, 0, 10))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "r",    # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_NLG <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "NLG.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_NLG  <- update(J_NLG, n.iter = 200000, n.thin = 10)  
out <- J_NLG$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing 
MyNames <- c(colnames(X_NLG), "Gamma variance parameter r")
MyBUGSChains(out, 
             c(uNames("beta", K),  "r"),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT1, digits = 5)  
MyBUGSHist(out, 
           c(uNames("beta", K), "r"),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Ok

plot(y = E1, 
     x = NLG$Hits)
abline(h = 0, lty = 2)
# Ok, same as above, just differently scaled axis





#----------------------##
# Model interpretation ####

# Calculate the fitted values.
# 1. Get the betas from the MCMC iterations
# 2. Define a grid of covariate values
# 3. Calculate the predicted values on this 
#    grid for each MCMC iteration.
# 4. Calculate the 95% credible intervals.
# 5. Plot the whole thing


# 1. Get the betas
beta.mcmc <- out$sims.list$beta  #betas
dim(beta.mcmc)  #60000 iterations saved 
# Using these we can puzzle together the pieces.



#2A. Define a grid of covariate values
#   without extrapolation
range(NLG$Hits)

MyData <- data.frame(Hits = seq(from = min(NLG$Hits), 
                                to = max(NLG$Hits), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    # removed exp function because there's not log-link


# Idea plot fitted values agains original value and calculate R2
plot(out$mean$mu,NLG$DW)
plot(sort(out$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out$mean$mu,NLG$DW)   # not used I don't think
(r2 <- var(out$mean$mu)/var(NLG$DW))
resids <- out$mean$mu-NLG$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))   # Quite low! 0.63

L <- GetCIs(mu.mcmc)
L



MyData2 <- cbind(MyData,L)
head(MyData2)




p <- ggplot() +
  geom_point(data = NLG, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Narrow Leaved Grasses")+
  annotate("text", x= 1, y= 30,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p

#compare with lm R2:
summary(lm(NLG$DW~0+NLG$Hits))   # R2 = 0.92 !!
results <- print(OUT1, digits = 5)  

plot(NLG$Hits, NLG$DW)
abline(lm(NLG$DW~0+NLG$Hits))
abline(a = results[1], b= results[2])

# Perhaps I can use log-link?
# LOG-LINK model #####
plot(sort(NLG$DW))

setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("NLG_L.txt")
cat("
    model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 20)}   
    
    #1B. Priors random effects 
    # not applicable
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    log(mu[i]) <- eta[i]                       #added log-link 
    eta[i] <- inprod(beta[], X[i,]) }
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()

J_NLG_L <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "NLG_L.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_NLG_L  <- update(J_NLG_L, n.iter = 200000, n.thin = 10)  
out2 <- J_NLG_L$BUGSoutput
print(out2, digits = 3)  


MyBUGSChains(out2, 
             c(uNames("beta", K),  "r"),
             PanelNames = MyNames)
# Mixing is good


# Step 8: Numerical output
OUT2    <- MyBUGSOutput(out2, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # based on original covaraites


# Model validation:

E1 <- out2$mean$PRes
F1 <- out2$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Ok - but non-continous range of covariate



# 1. Get the betas
beta.mcmc2 <- out2$sims.list$beta  #betas
dim(beta.mcmc2)  #60,000 iterations saved     
# Using these we can puzzle together the pieces.



#2A. Define a grid of covariate values
#   without extrapolation
range(NLG$Hits)

MyData_L <- data.frame(Hits = seq(from = min(NLG$Hits), 
                                to = max(NLG$Hits), length = 50))
head(MyData_L)

#B. Convert the covariate values into an X matrix
Xp_L <- model.matrix(~ Hits, 
                   data = MyData_L) 



# Get R-squared value and compare with identity-link model
par(mfrow=c(2,1))
plot(out2$mean$mu,NLG$DW, main="log-link")
abline(a=0, b=1)
plot(out$mean$mu,NLG$DW, main="identity link")
abline(a=0, b=1)

resids2 <- out2$mean$mu-NLG$DW
(r2x2 <- var(out2$mean$mu)/(var(out2$mean$mu)+var(resids2)))   # Good
r2x # identity-link R2 = 0.63
r2x2  #log-link R2     = 0.87

# lets plot both

#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc2 <- Xp %*% t(beta.mcmc2)     # model matrix * est.betas
mu.mcmc2  <- exp(eta.mcmc2)      # note exp because of log-link

# Why don't we take the 2.5% and 97.5% values
# at each of the artificial covariate values?
# And plot these instead of the 3,000 lines? 
# We wrote  a small support function that does this:

L2 <- GetCIs(mu.mcmc2)
L2


MyData2_L <- cbind(MyData_L,L2)
head(MyData2_L)



p2 <- ggplot() +
  geom_point(data = NLG, 
             aes(x = Hits, 
                 y = DW))+
  #identity-link
  #geom_line(data = MyData2, 
  #          aes(x = Hits, 
  #              y = mean))+
  #geom_ribbon(data = MyData2,
  #            aes(x = Hits, 
  #                ymax = up, 
  #                ymin = lo), 
  #            alpha = 0.5)+
  # log-link
  geom_line(data = MyData2_L, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2_L,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.2)+
  xlab("Average number of\nhits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  #ggtitle("Narrow Leaved Grasses")+
  #annotate("text", x= 1, y= 15,                   
  #         label = paste("Identity-link R-sq = ", round(r2x, 2), sep = " "),
  #         size = 5)+
  annotate("text", x= 1.5, y= 130,                   
           label = paste("R-sq = ", round(r2x2, 2), sep = " "), size = 5)+
  annotate("text", x= 1.5, y= 150,                   
           label = "Log-link model", size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
grid.arrange(p, p2, nrow = 2)




# Field notes leave no indication that could justify taking out data points
# The log model has higher R2. Both models seem to be very influence by two high y-values
# Is it more likely that the three high x values are because of overcounting or undercounting.
# Likely with high biomass the counting gets sloppy and you count less hits.
# In pracice what it means is that the predicted values are quite similar up until
# ~ 4 hits.
par(mfrow=c(1,1))
plot(sort(out2$mean$mu), type = "b")
points(sort(out$mean$mu), pch=20)


# Staying with the Identity link model


est <- print(OUT1, digits = 5) 
est[2]
pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x = 9, y=8000,  
           label = paste("mean = ", round(est[2], 4), sep = " "),
           size = 5)+
  xlab("Estimated slope")+
  xlim(2, 11)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=2, y= 8000,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 3.5)+
  theme_few()+theme(text = element_text(size=15))
pHist_I



grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, p2, ncol = 3, nrow = 1))

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/plots")
ggsave(filename = "NLG_model.tiff", height = 10, width = 12,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, p2, ncol = 3, nrow = 1)))





# What is the R-sq if I remove the three extreme values?

# Truncated model #####


plot(sort(NLG$DW))
NLG2 <- NLG[NLG$DW<30,]
plot(sort(NLG2$DW))
rm(NLG)
#1. Bundle data

X_NLG2 <- model.matrix(~ Hits , data = NLG2)   
K <- ncol(X_NLG2) 
X_NLG <- as.matrix(X_NLG2)
X_NLG
K

# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = NLG2$DW, 
                  X    = X_NLG2,
                  N    = nrow(NLG2),
                  K    = K       )
JAGS.data


setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("NLG2.txt")
cat("
    model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 20)}   
    
    #1B. Priors random effects 
    # not applicable
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- eta[i]                        
    eta[i] <- inprod(beta[], X[i,]) }
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()



# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = runif(2, 0, 20),
    r     = runif(1, 0, 10))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "r",    # gamma parameter
            "mu" ,
            "PRes") 

J_NLG2 <- jags(data       = JAGS.data,
                inits      = inits,
                parameters = params,
                model      = "NLG2.txt",
                n.thin     = 10,
                n.chains   = 3,
                n.burnin   = 4000,
                n.iter     = 5000)

J_NLG2  <- update(J_NLG2, n.iter = 200000, n.thin = 10)  
out3 <- J_NLG2$BUGSoutput
print(out2, digits = 3)  

MyNames <- c(colnames(X_NLG2), "Gamma variance parameter r")

MyBUGSChains(out3, 
             c(uNames("beta", K),  "r"),
             PanelNames = MyNames)
# Mixing is good


# Step 8: Numerical output
OUT3    <- MyBUGSOutput(out3, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT3, digits = 5)  # slope is less, intercept is higher



# R-sq
NLG2$mu <- out3$mean$mu
NLG2$resid <- NLG2$mu - NLG2$DW
(r2x3 <- var(NLG2$mu)/(var(NLG2$mu)+var(NLG2$resid)))   # something's strange
(r2x4 <- var(NLG2$mu)/var(NLG2$DW))   # even less
cor.test(NLG2$DW, NLG2$mu)



# 1. Get the betas
beta.mcmc3 <- out3$sims.list$beta  #betas
dim(beta.mcmc3)  #60,000 iterations saved     


#2A. Define a grid of covariate values
#   without extrapolation
range(NLG2$Hits)

MyData3 <- data.frame(Hits = seq(from = min(NLG2$Hits), 
                                  to = max(NLG2$Hits), length = 50))
head(MyData3)

#B. Convert the covariate values into an X matrix
Xp_2 <- model.matrix(~ Hits, 
                     data = MyData3) 

#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc3 <- Xp_2 %*% t(beta.mcmc3)     # model matrix * est.betas
mu.mcmc3  <- eta.mcmc3      

L3 <- GetCIs(mu.mcmc3)

MyData4 <- cbind(MyData3,L3)
head(MyData4)



p3 <- ggplot() +
  geom_ribbon(data = MyData4,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo))+
  geom_point(data = NLG2, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData4, 
            aes(x = Hits, 
                y = mean))+
  xlab("Average number of\nhits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  annotate("text", x= 1.5, y= 15,                   
           label = paste("R-sq = ", round(r2x3, 2), sep = " "), size = 5)+
  annotate("text", x= 1.5, y= 20,                   
           label = "Truncated model", size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15))
p3





# BLS ####

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/BLS.rda")
names(BLS)
plot(BLS$Hits, BLS$DW)
summary(BLS)
#1. Bundle data

X_BLS <- model.matrix(~ Hits , data = BLS)   # keeping the intercept, but remember model is only valid of #hits >0
K <- ncol(X_BLS) 
X_BLS <- as.matrix(X_BLS)
X_BLS
K

# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = BLS$DW, 
                  X    = X_BLS,
                  N    = nrow(BLS),
                  K    = K       )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("BLS.txt")
cat("
    model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 200)}   
    
    #1B. Priors random effects 
    # not applicable
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- eta[i] 
    eta[i] <- inprod(beta[], X[i,]) }
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()








# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = runif(2, 0, 20),
    r     = runif(1, 0, 10))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "r",    # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_BLS <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "BLS.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_BLS  <- update(J_BLS, n.iter = 200000, n.thin = 10)  
out <- J_BLS$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c(colnames(X_BLS), "Gamma variance parameter r")
MyBUGSChains(out, 
             c(uNames("beta", K),  "r"),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # based on original covaraites
MyBUGSHist(out, 
           c(uNames("beta", K), "r"),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Ok

plot(y = E1, 
     x = BLS$Hits)
abline(h = 0, lty = 2)
# Ok, same as above, because it's identity link




# 1. Get the betas
beta.mcmc <- out$sims.list$beta  #betas
dim(beta.mcmc)  #60000 iterations saved 
# Using these we can puzzle together the pieces.



#2A. Define a grid of covariate values
#   without extrapolation
range(BLS$Hits)

MyData <- data.frame(Hits = seq(from = min(BLS$Hits), 
                                to = max(BLS$Hits), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    # removed exp function because there's not log-link


# Plot observed vs fitted
plot(out$mean$mu,BLS$DW)    
plot(sort(out$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out$mean$mu,BLS$DW)   # not used I don't think
(r2 <- var(out$mean$mu)/var(BLS$DW))
resids <- out$mean$mu-BLS$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))   # 0.82

L <- GetCIs(mu.mcmc)
L



MyData2 <- cbind(MyData,L)
head(MyData2)




p <- ggplot() +
  geom_point(data = BLS, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Broad Leaved Shrubs")+
  annotate("text", x= 1, y= 300,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p

#compare with lm R2:
summary(lm(BLS$DW~BLS$Hits))   # R2 = 0.87


est <- print(OUT1, digits = 5) 
est[2]
pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=55, y=6000,
           label = paste("mean = ", round(est[2], 4), sep = " "),
           size = 5)+
  xlab("Estimated slope")+
  xlim(45, 110)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=4, y=8500,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 10)+
  theme_few()+theme(text = element_text(size=15))
pHist_I



library(gridExtra)
grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
ggsave(filename = "plots/BLS_model.tiff", height = 10, width = 10,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1)))






# NLS ####

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/NLS.rda")
names(NLS)
plot(NLS$Hits, NLS$DW)
abline(lm(NLS$DW~NLS$Hits))
identify(NLS$Hits, NLS$DW)
NLS[2,]   # it's plot 4 - no mistake
NLS[10,]  # it's plot 26 - no mistake
NLS[11,]  # it's plot 32 - no mistake
# keep all data points
summary(NLS)

# linaer, quadratic and qubic regression

x <-  NLS$Hits
x2 <- I(NLS$Hits^2)
x3 <- I(NLS$Hits^3)
lmfit <- lm(NLS$DW~x)
pol2 <- lm(NLS$DW~x+I(x^2))
pol3 <- lm(NLS$DW~x+I(x^2)+I(x^3))
summary(lmfit) # 0.73, 
summary(pol2) # 0.74
summary(pol3) # 0.87 


plot(x, NLS$DW, xlab="Frequency", ylab="Dry Wweight (g/m2)", main="Narrow leaved shrubs")
abline(lmfit)
lines(seq(0,5,length.out = 30), predict(pol2, newdata = data.frame(x = seq(0,5,length.out = 30))))
lines(seq(0,5,length.out = 30), predict(pol3, newdata = data.frame(x = seq(0,5,length.out = 30))))
text(x=1, y=300, "R-suared values\nLinear = 0.73\nQuadratic = 0.74\nQubic = 0.87")

predict(lmfit, newdata = data.frame(x = seq(0,5,length.out = 30)))
# trying quadratic first

#1. Bundle data

X_NLS <- model.matrix(~ Hits , data = NLS)   
K <- ncol(X_NLS) 
X_NLS <- as.matrix(X_NLS)
X_NLS
K

# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = NLS$DW, 
                  X    = X_NLS[,2],
                  N    = nrow(NLS)   )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("NLS.txt")
cat("
    model{
    #1A. Priors betas
    alpha ~ dunif(0,200)
    beta1 ~ dunif(0,200)
    beta2 ~ dnorm(0,0.000001)
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- alpha + beta1 * X[i] + beta2 * X[i]^2 }              
    
#3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()








# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    alpha  = runif(1, 0, 200),
    beta1  = runif(1, 0, 100),
    beta2  = rnorm(0,100),
    r      = runif(1, 0, 20))  }


# Step 5: Specify what to save
params <- c("alpha", "beta1", "beta2",     #Regression parameters
            "r",                           # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_NLS <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "NLS.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_NLS  <- update(J_NLS, n.iter = 20000, n.thin = 10)  
out <- J_NLS$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c("alpha", "beta1", "beta2", "r")
MyBUGSChains(out, 
             c(MyNames),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(MyNames),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # positive quadratic term! No good!
MyBUGSHist(out, 
           c(uNames("beta", K), "r"),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# not great





# 1. Get the betas
beta.mcmc <- cbind(out$sims.list$alpha, out$sims.list$beta1, out$sims.list$beta2)
dim(beta.mcmc)  #60000 iterations saved 
head(beta.mcmc)

#2A. Define a grid of covariate values
#   without extrapolation
range(NLS$Hits)

MyData <- data.frame(Hits = seq(from = min(NLS$Hits), 
                                to = max(NLS$Hits), length = 50))
MyData$Hits2 <- MyData[,1]^2 
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits+Hits2, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    # removed exp function because there's not log-link
head(beta.mcmc)


# Plot observed vs fitted
plot(out$mean$mu,NLS$DW)    
plot(sort(out$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out$mean$mu,NLS$DW)   
(r2 <- var(out$mean$mu)/var(NLS$DW))   # above 1! that why we use the other R2->
resids <- out$mean$mu-NLS$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))   # 0.75

L <- GetCIs(mu.mcmc)
L



MyData2 <- cbind(MyData,L)
head(MyData2, 20)




p_quad <- ggplot() +
  geom_point(data = NLS, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Narrow Leaved Shrubs")+
  annotate("text", x= 1, y= 1000,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p_quad



# Not great. Trying linear
# Linear model ####
#1. Bundle data

X_NLS <- model.matrix(~ Hits , data = NLS)   
K <- ncol(X_NLS) 
X_NLS <- as.matrix(X_NLS)
X_NLS
K

# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = NLS$DW, 
                  X    = NLS$Hits,
                  N    = nrow(NLS) )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("NLS2.txt")
cat("
    model{
    #1A. Priors betas
    alpha ~ dunif(0,200)
    beta1 ~ dunif(0,200)
    
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- alpha + beta1 * X[i]  }              
    
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()








# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    alpha  = runif(1, 0, 200),
    beta1  = runif(1, 0, 200),
    r      = runif(1, 0, 50))  }


# Step 5: Specify what to save
params <- c("alpha", "beta1",     #Regression parameters
            "r",                           # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_NLS2 <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "NLS2.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_NLS2  <- update(J_NLS2, n.iter = 200000, n.thin = 10)  
out2 <- J_NLS2$BUGSoutput
print(out2, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c("alpha", "beta1", "r")
MyBUGSChains(out2, 
             c(MyNames),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT2    <- MyBUGSOutput(out2, 
                        c(MyNames),
                        VarNames = MyNames)
print(OUT2, digits = 5)  # 7 + 74 *X
summary(lmfit)           # 31 + 52 *X
# the OLS is heavely influenced by the extreme points! Gamma is better.
MyBUGSHist(out2, 
           c(MyNames),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# ok




# 1. Get the betas
beta.mcmc2 <- cbind(out2$sims.list$alpha, out2$sims.list$beta1)
dim(beta.mcmc2)  #60000 iterations saved 
head(beta.mcmc2)

#2A. Define a grid of covariate values
#   without extrapolation
range(NLS$Hits)

MyData3 <- data.frame(Hits = seq(from = min(NLS$Hits), 
                                to = max(NLS$Hits), length = 50))
head(MyData3)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc2 <- Xp %*% t(beta.mcmc2)     # model matrix * est.betas
mu.mcmc2  <- (eta.mcmc2)    



# Plot observed vs fitted
plot(out2$mean$mu,NLS$DW)    
plot(sort(out2$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out2$mean$mu,NLS$DW)   
(r2 <- var(out2$mean$mu)/var(NLS$DW))   # above 1! that why we use the other R2->
resids2 <- out2$mean$mu-NLS$DW
(r2x2 <- var(out2$mean$mu)/(var(out2$mean$mu)+var(resids2)))   # 0.79

L2 <- GetCIs(mu.mcmc2)
L2



MyData4 <- cbind(MyData3,L2)
head(MyData4, 20)




p_lin <- ggplot() +
  geom_point(data = NLS, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData4, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData4,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Narrow Leaved Shrubs")+
  annotate("text", x= 1, y= 300,                   
           label = paste("R-sq = ", round(r2x2, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p_lin

#grid.arrange(p_lin, p_quad)


est <- print(OUT2, digits = 5) 
est[2]
pHist <- ggplot(data = as.data.frame(beta.mcmc2), aes(x = beta.mcmc2[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc2[,2]), xend = mean(beta.mcmc2[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc2[,2], probs = 0.025), xend = quantile(beta.mcmc2[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=30, y=6000,
           label = paste("mean = ", round(est[2], 4), sep = " "),
           size = 5)+
  xlab("Estimated slope")+
  xlim(0, 130)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc2), aes(x = beta.mcmc2[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc2[,1]), xend = mean(beta.mcmc2[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc2[,1], probs = 0.025), xend = quantile(beta.mcmc2[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=20, y=8500,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 40)+
  theme_few()+theme(text = element_text(size=15))
pHist_I

grid.arrange(arrangeGrob(p_lin, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))

ggsave(filename = "plots/NLS_model.tiff", height = 10, width = 10,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1)))




# Gaussian Model ####
# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = NLS$DW, 
                  X    = X_NLS,
                  N    = nrow(NLS),
                  K    = K       )
JAGS.data


# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = rnorm(2, 0, 0.1),
    sigma     = runif(1, 0, 200))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "sigma",    # gamma parameter
            "mu", 
            "Res"  )  

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
sink("NLS_L.txt")
cat("
    model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dnorm(0, 0.0001)}   
    
    #1B. Prior for sigma
    tau <- 1 / (sigma * sigma)
    sigma ~ dunif(0, 200)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]   ~ dnorm(mu[i], tau)
    mu[i]  <- eta[i]
    eta[i]  <- inprod(beta[], X[i,]) }
    

    #3. Calculate residuals
    for (i in 1:N) {
    Res[i] <- Y[i] - mu[i]
    }
    
    }
    ",fill = TRUE)
sink()

J_NLS_L <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "NLS_L.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_NLS_L  <- update(J_NLS_L, n.iter = 20000, n.thin = 10)  
out2 <- J_NLS_L$BUGSoutput
print(out2, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c(colnames(X_NLS), "Sigma")
MyBUGSChains(out2, 
             c(uNames("beta", K),  "sigma"),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT2    <- MyBUGSOutput(out2, 
                        c(uNames("beta", K), "r"),
                        VarNames = MyNames)
print(OUT2, digits = 5)  # based on original covaraites
MyBUGSHist(out2, 
           c(uNames("beta", K), "r"),
           PanelNames = MyNames)

# Model validation:

E1 <- out2$mean$Res
F1 <- out2$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# Ok, but very uneven covariate. No negative fitted values.





# 1. Get the betas
beta.mcmc2 <- out2$sims.list$beta  #betas
dim(beta.mcmc2)  #60000 iterations saved 


#2A. Define a grid of covariate values
#   without extrapolation
range(NLS$Hits)

MyData_L <- data.frame(Hits = seq(from = min(NLS$Hits), 
                                to = max(NLS$Hits), length = 50))
head(MyData_L)

#B. Convert the covariate values into an X matrix
Xp_L <- model.matrix(~ Hits, 
                   data = MyData_L) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc2 <- Xp_L %*% t(beta.mcmc2)     # model matrix * est.betas
mu.mcmc2  <- (eta.mcmc2)    # backtransforming


# Plot observed vs fitted
plot(out2$mean$mu,NLS$DW)    # ok
plot(sort(out2$mean$mu))  # these are the mean fitted values for each observed value of the covariate
cor.test(out2$mean$mu,NLS$DW)   
(r2_L <- var(out2$mean$mu)/var(NLS$DW))   # 
resids_L <- out2$mean$mu-NLS$DW
(r2xL <- var(out2$mean$mu)/(var(out2$mean$mu)+var(resids_L)))   # 0.73

L2 <- GetCIs(mu.mcmc2)


MyData2_L <- cbind(MyData_L,L2)
head(MyData2_L)




p2 <- ggplot() +
  geom_point(data = NLS, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2_L, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2_L,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Narrow Leaved Shrubs")+
  annotate("text", x= 1, y= 300,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p2




# TH ####

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/TH.rda")
names(TH)
plot(TH$Hits, TH$DW) # nice
abline(lm(TH$DW~TH$Hits))
summary(lm(TH$DW~TH$Hits)) # 8+30x
summary(TH)



#  Prepare data for JAGS
JAGS.data <- list(Y    = TH$DW, 
                  X    = TH$Hits,
                  N    = nrow(TH)   )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("TH.txt")
cat("
    model{
    #1A. Priors betas
    alpha ~ dunif(0,200)
    beta1 ~ dunif(0,200)
    
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- alpha + beta1 * X[i]  }              
    
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()


# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    alpha  = runif(1, 0, 20),
    beta1  = runif(1, 0, 100),
    r      = runif(1, 0, 20))  }


# Step 5: Specify what to save
params <- c("alpha", "beta1",              #Regression parameters
            "r",                           # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_TH <- jags(data       = JAGS.data,
              inits      = inits,
              parameters = params,
              model      = "TH.txt",
              n.thin     = 10,
              n.chains   = 3,
              n.burnin   = 4000,
              n.iter     = 5000)

J_TH  <- update(J_TH, n.iter = 200000, n.thin = 10)  
out <- J_TH$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c("alpha", "beta1", "r")
MyBUGSChains(out, 
             c(MyNames),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(MyNames),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # comparable to OLS
MyBUGSHist(out, 
           c(MyNames),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# ok





# 1. Get the betas
beta.mcmc <- cbind(out$sims.list$alpha, out$sims.list$beta1)
dim(beta.mcmc)  #60000 iterations saved 

#2A. Define a grid of covariate values
#   without extrapolation
range(TH$Hits)

MyData <- data.frame(Hits = seq(from = min(TH$Hits), 
                                to = max(TH$Hits), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    



# Plot observed vs fitted
plot(out$mean$mu,TH$DW)    
plot(sort(out$mean$mu))  # best one yet
cor.test(out$mean$mu,TH$DW)   # high
(r2 <- var(out$mean$mu)/var(TH$DW))   # above 1! that why we use the other R2->
resids <- out$mean$mu-TH$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))   # 0.87

L <- GetCIs(mu.mcmc)
L



MyData2 <- cbind(MyData,L)
head(MyData2, 20)




p <- ggplot() +
  geom_point(data = TH, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Tall herbs")+
  annotate("text", x= 1, y= 100,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p


est <- print(OUT1, digits = 5) 
pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=58, y=6000,
           label = paste("mean = ", round(est[2], 4), sep = " "),
           size = 5)+
  xlab("Estimated slope")+
  xlim(10, 70)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=15, y=5500,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 20)+
  theme_few()+theme(text = element_text(size=15))
pHist_I



grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))

ggsave(filename = "plots/TH_model.tiff", height = 10, width = 10,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1)))





# SH ####

setwd("M:/Anders L Kolstad/R/R_projects/soilTemperature/")
load("data/SH.rda")
names(SH)
plot(SH$Hits, SH$DW) # uneven
abline(lm(SH$DW~SH$Hits))
summary(lm(SH$DW~SH$Hits)) # 0 + 18x
summary(SH)



#  Prepare data for JAGS
JAGS.data <- list(Y    = SH$DW, 
                  X    = SH$Hits,
                  N    = nrow(SH)   )
JAGS.data

# Step 3: Formulate JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("SH.txt")
cat("
    model{
    #1A. Priors betas
    alpha ~ dunif(0,50)
    beta1 ~ dunif(0,200)
    
    
    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 20)
    
    #2. Likelihood
    for (i in 1:N) {
    Y[i]        ~ dgamma(r, mu.eff[i])
    mu.eff[i]  <- r / mu[i]
    mu[i] <- alpha + beta1 * X[i]  }              
    
    #3. Discrepancy measures: Pearson residuals   
    for (i in 1:N) {
    VarY[i] <- mu[i]^2 / r
    PRes[i] <- (Y[i] - mu[i])  / sqrt(VarY[i])
    }
    }
    ",fill = TRUE)
sink()


# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    alpha  = runif(1, 0, 20),
    beta1  = runif(1, 0, 100),
    r      = runif(1, 0, 20))  }


# Step 5: Specify what to save
params <- c("alpha", "beta1",              #Regression parameters
            "r",                           # gamma parameter
            "mu" ,
            "PRes")  


# Step 6: Start JAGS

J_SH <- jags(data       = JAGS.data,
             inits      = inits,
             parameters = params,
             model      = "SH.txt",
             n.thin     = 10,
             n.chains   = 3,
             n.burnin   = 4000,
             n.iter     = 5000)

J_SH  <- update(J_SH, n.iter = 200000, n.thin = 10)  
out <- J_SH$BUGSoutput
print(out, digits = 3)  


# Step 7: Assess mixing and check overdispersion
# Adjust this variable if extra parameters are added!
MyNames <- c("alpha", "beta1", "r")
MyBUGSChains(out, 
             c(MyNames),
             PanelNames = MyNames)
# Mixing is good




# Step 8: Numerical output
OUT1    <- MyBUGSOutput(out, 
                        c(MyNames),
                        VarNames = MyNames)
print(OUT1, digits = 5)  # comparable to OLS
MyBUGSHist(out, 
           c(MyNames),
           PanelNames = MyNames)

# Model validation:

E1 <- out$mean$PRes
F1 <- out$mean$mu

par(mfrow = c(1, 1))
plot(x = F1, 
     y = E1,
     xlab = "Posterior mean fitted values",
     ylab = "Posterior mean Pearson residuals")
abline(h = 0, lty = 2, col = 1)
# ok, slight funnel





# 1. Get the betas
beta.mcmc <- cbind(out$sims.list$alpha, out$sims.list$beta1)
dim(beta.mcmc)  #60000 iterations saved 

#2A. Define a grid of covariate values
#   without extrapolation
range(SH$Hits)

MyData <- data.frame(Hits = seq(from = min(SH$Hits), 
                                to = max(SH$Hits), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ Hits, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    



# Plot observed vs fitted
plot(out$mean$mu,SH$DW)    
plot(sort(out$mean$mu))  
cor.test(out$mean$mu,SH$DW)   # high
(r2 <- var(out$mean$mu)/var(SH$DW))   
resids <- out$mean$mu-SH$DW
(r2x <- var(out$mean$mu)/(var(out$mean$mu)+var(resids)))   # 0.77

L <- GetCIs(mu.mcmc)
L



MyData2 <- cbind(MyData,L)
head(MyData2, 20)




p <- ggplot() +
  geom_point(data = SH, 
             aes(x = Hits, 
                 y = DW))+
  geom_line(data = MyData2, 
            aes(x = Hits, 
                y = mean))+
  geom_ribbon(data = MyData2,
              aes(x = Hits, 
                  ymax = up, 
                  ymin = lo), 
              alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab(expression(paste("Dry weight (g m"^"-2",")")))+
  theme(legend.position="none") +
  ggtitle("Short herbs")+
  annotate("text", x= 0.5, y= 60,                   
           label = paste("R-sq = ", round(r2x, 2), sep = " "),
           size = 5)+
  theme_few()+ theme(plot.title = element_text(hjust = 0.5)) + theme(text = element_text(size=15)) 
p


est <- print(OUT1, digits = 5) 
pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=24, y=6000,
           label = paste("mean = ", round(est[2], 4), sep = " "),
           size = 5)+
  xlab("Estimated slope")+
  xlim(5, 30)+
  theme_few()+theme(text = element_text(size=15)) 
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2200,
           colour = "blue", size = 3)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 0,
           colour = "blue", size = 3)+
  annotate("text", x=2, y=5500,
           label = paste("mean = ", round(est[1], 4), sep = " "),
           size = 5)+
  xlab("Estimated intercept")+
  xlim(0, 3)+
  theme_few()+theme(text = element_text(size=15))
pHist_I



grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))

ggsave(filename = "plots/SH_model.tiff", height = 10, width = 10,
       plot = grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
                           arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1)))







