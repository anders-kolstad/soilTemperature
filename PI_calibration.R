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
setwd("M:\\Anders L Kolstad\\HIGHSTATS\\AllData")
source(file = "HighstatLibV10.R")  
source(file = "MCMCSupportHighstatV4.R")

# Load data ####
setwd("M:\\Anders L Kolstad\\systherb data\\PI_calibration")
dat <- read_excel("PI calibration SustHerb 2017.xlsx", 
                  sheet = "Sheet1")


# Housekeeping ####
str(dat)
dat$within_moss_DW_g <- as.numeric(dat$within_moss_DW_g)
dat$`Moss depth cm` <- as.numeric(dat$`Moss depth cm`)
dat <- dat[1:40,]
dat$NLS_DW_g <- rowSums(dat[,c("Calluna_DW_g", "Empetrum_DW_g")], na.rm = T)
dat$NLS_DW_g[dat$NLS_DW_g == 0] <- NA

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




# Standardise the number of hits as the avergae per pin:
std_grp <- c("Tall_herbs", "Short_herbs", "Narrow_leaved_shrubs",
              "Broad_leaved_shrubs", "Narrow_leaved_grasses", "Broad_leaved_grasses"
              )
dat2 <- dat
dat2[,std_grp] <- dat2[,std_grp]/49
head(dat2[,std_grp])
head(dat[,std_grp])
dat <- dat2
rm(dat2)
# OBS, Calluna and Empetrum are still not standardised, but NLS is.

# Scale 'within moss' to be same area unit (0.25m2)
# area samples is
pi*5*5 # = 78.53982 cm2 
(pi*5*5)/10000 # = 0.007853982 m2
(pi*5*5)/10000/0.25 # 0.03141593 per 50x50cm quadrat
dat$within_moss_DW_gmsq <- dat$within_moss_DW_g/((pi*5*5)/10000/0.25)

# sum all species:
dat$total_hits <- rowSums(dat[,std_grp], na.rm = TRUE)
abun_grp2 <- c("BLG_DW_g", "NLG_DW_g",
              "BLS_DW_g", "NLS_DW_g", 
              "TH_DW_g", "SH_DW_g")
dat$total_bm <- rowSums(dat[,abun_grp2], na.rm = TRUE)




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



# BLG ####
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


##Step 1: Fit the model.
#Use MCMC to estimate the parameters 

# Step 0. Get data
BLG <- filter(dat2, Broad_leaved_grasses != 0 & BLG_DW_g != 0)



# scale continous covariates
BLG$std.Broad_leaved_grasses <- scale(BLG$Broad_leaved_grasses)
# center continous covariates
BLG$c.Broad_leaved_grasses <- BLG$Broad_leaved_grasses-mean(BLG$Broad_leaved_grasses)
# tidy dtaframe
BLG2 <- select(BLG, DW = BLG_DW_g, 
              Hits = Broad_leaved_grasses,
              c.Hits = c.Broad_leaved_grasses,
              std.Hits =std.Broad_leaved_grasses)
BLG <- as.data.frame(BLG2)


#1. Bundle data

X_BLG <- model.matrix(~ Hits , data = BLG)   # keeping the intercept, but remember model is only valid of #hits >0
K <- ncol(X_BLG) 
X_BLG <- as.matrix(X_BLG)
X_BLG
K

#Random effects:
#None


# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = BLG$DW, 
                 X    = X_BLG,
                 N    = nrow(BLG),
                 K    = K       )
JAGS.data
# Y data should be all positive. X data should be standardised


# Step 3: JAGS modelling code
setwd("M:\\Anders L Kolstad\\R\\R_projects\\soilTemperature")
sink("BLG.txt")
cat("
model{
    #1A. Priors betas
    for (i in 1:K) { beta[i] ~  dunif(0, 50)}   

    #1B. Priors random effects 
    # not applicable

    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 1000)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]        ~ dgamma(r, mu.eff[i])
      mu.eff[i]  <- r / mu[i]     
      mu[i] <- inprod(beta[], X[i,]) }                      # removed log-link
            

    #3. Discrepancy measures: Pearson residuals   
        for (i in 1:N) {
           VarY[i] <- mu[i]^2 / r
           PRes[i] <- (Y[i] - mu[i]) / sqrt(VarY[i])}
    # back-standardice output
        # skipping this in the model syntax this time
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
            "mu",
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

J_BLG  <- update(J_BLG, n.iter = 50000, n.thin = 10)  
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
print(OUT1, digits = 5)  # based on standardised covaraites


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
     x = BLG$std.Broad_leaved_grasses)
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
dim(beta.mcmc)  #15000 iterations saved - should be increased later...
# Using these we can puzzle together the pieces.



beta.mcmc2 <- beta.mcmc
head(beta.mcmc)
beta.mcmc[,2] <- (beta.mcmc2[,2] * sd(BLG$BLG_DW_g)) / sd(BLG$std.Broad_leaved_grasses)



#2A. Define a grid of covariate values
#   without extrapolation
range(BLG$std.Broad_leaved_grasses)
min(BLG$std.Broad_leaved_grasses)
MyData <- data.frame(std.Broad_leaved_grasses = seq(from = min(BLG$std.Broad_leaved_grasses), 
                                                    to = max(BLG$std.Broad_leaved_grasses), length = 50))
head(MyData)

#B. Convert the covariate values into an X matrix
Xp <- model.matrix(~ std.Broad_leaved_grasses, 
                   data = MyData) 



#C. Calculate the predicted MCMC values
# these are insenitive to standardisation of covariats (making the intercept and betas at the wrong scale)

eta.mcmc <- Xp %*% t(beta.mcmc)     # model matrix * est.betas
mu.mcmc  <- (eta.mcmc)    # removed exp function because there's not log-link



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
MyData2$BLG_hits <- MyData$std.Broad_leaved_grasses * sd(BLG$Broad_leaved_grasses) + mean(BLG$Broad_leaved_grasses)







p <- ggplot() +
      geom_point(data = BLG, 
                    aes(x = Broad_leaved_grasses, 
                        y = BLG_DW_g))+
  geom_line(data = MyData2, 
                   aes(x = BLG_hits, 
                       y = mean))+
  geom_ribbon(data = MyData2,
                     aes(x = BLG_hits, 
                         ymax = up, 
                         ymin = lo), 
                     alpha = 0.5)+
  xlab("Average number of hits per pin") + ylab("Dry weight (g)")+
  theme(text = element_text(size=15)) +
  theme(legend.position="none") +
  ggtitle("Broad Leaved Grasses")+
  theme(plot.title = element_text(hjust = 0.5))
p


pHist <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,2]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,2]), xend = mean(beta.mcmc[,2]), y = 0, yend = 2000,
           colour = "blue", size = 1.5)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.975), xend = quantile(beta.mcmc[,2], probs = 0.975), 
           y = 0, yend = 500,
           colour = "red", size = 1.5)+
  annotate("segment", x = quantile(beta.mcmc[,2], probs = 0.025), xend = quantile(beta.mcmc[,2], probs = 0.025), 
           y = 0, yend = 500,
           colour = "red", size = 1.5)+
  annotate("text", x = 13, y = 1800, size = 5, label = "Mean = 9.4662")+
  theme(text = element_text(size=15)) +
  xlab("Estimated slope")
pHist

pHist_I <- ggplot(data = as.data.frame(beta.mcmc), aes(x = beta.mcmc[,1]))+
  geom_histogram(bins = 30)+
  annotate("segment", x = mean(beta.mcmc[,1]), xend = mean(beta.mcmc[,1]), y = 0, yend = 2000,
           colour = "blue", size = 1.5)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.975), xend = quantile(beta.mcmc[,1], probs = 0.975), 
           y = 0, yend = 500,
           colour = "red", size = 1.5)+
  annotate("segment", x = quantile(beta.mcmc[,1], probs = 0.025), xend = quantile(beta.mcmc[,1], probs = 0.025), 
           y = 0, yend = 500,
           colour = "red", size = 1.5)+
  annotate("text", x = 13, y = 1800, size = 5, label = "Mean = 9.4893")+
  theme(text = element_text(size=15)) +
  xlab("Estimated intercept")
pHist_I



library(gridExtra)
grid.arrange(arrangeGrob(p, ncol = 1, nrow=1), 
             arrangeGrob(pHist, pHist_I, ncol = 2, nrow = 1))





# back-transform intercept
# test agains original data again
par(mfrow = c(1,1), mar = c(5,5,5,2))
plot(dat2$Broad_leaved_grasses, dat2$BLG_DW_g,  main = "BLG",  pch = 20) 
summary(lm(dat2$BLG_DW_g~dat2$Broad_leaved_grasses))
abline(lm(dat2$BLG_DW_g~dat2$Broad_leaved_grasses))
