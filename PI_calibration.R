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
plot(dat2$BLG_DW_g, dat2$Broad_leaved_grasses,  main = "BLG", xlab = "", ylab = "", pch = 20) # saturates - non-linear

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

# scale continous covariates (can you do that with gamma?)

#BLG$std.BLG_DW <- scale(BLG$BLG_DW_g)
BLG$std.Broad_leaved_grasses <- scale(BLG$Broad_leaved_grasses)

summary(BLG$std.BLG_DW) # OK
sd(BLG$std.BLG_DW)      # OK



#1. Bundle data

X_BLG <- model.matrix(~ std.Broad_leaved_grasses , data = BLG)                      
K <- ncol(X_BLG)  
head(X_BLG)

#Random effects:
#None


# Step 2: Prepare data for JAGS
JAGS.data <- list(Y    = BLG$BLG_DW_g, 
                 X    = X_BLG,
                 N    = nrow(BLG),
                 K    = K       )
JAGS.data
# Y data should be all positive. X data should be standardised


# Step 3: JAGS modelling code
setwd("M:\\Anders L Kolstad\\systherb data\\PI_calibration")
sink("BLG.txt")
cat("
model{
    #1A. Priors beta and sigma
    for (i in 1:K) { beta[i] ~  dunif(0.0001, 100)}   

    #1B. Priors random effects 
    # not applicable

    #1C. Prior for r parameter of Gamma distribution
    r ~ dunif(0, 10)

    #2. Likelihood
    for (i in 1:N) {
      Y[i]        ~ dgamma(r, mu.eff[i])
      mu.eff[i]  <- r / mu[i]     
      mu[i] <- eta[i]                        # removed log-link
      eta[i]     <- inprod(beta[], X[i,])  
    #3. Discrepancy measures: Pearson residuals   
        for (i in 1:N) {
           VarY[i] <- mu[i]^2 / r
           PRes[i] <- (Y[i] - eta[i]) / sqrt(VarY[i])   
    }
     }          
}
",fill = TRUE)
sink()

# Step 4: Initial values & parameters to save   # R syntax
inits  <- function () {
  list(
    beta  = runif(2, 0.001, 20),
    r     = runif(1, 0, 10))  }


# Step 5: Specify what to save
params <- c("beta", #Regression parameters
            "r",
            "mu",
            "PRes")   # gamma parameter


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
print(OUT1, digits = 5)
