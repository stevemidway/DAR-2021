# Install packages
library(arm)
library(lattice)
library(tidyverse)

# Read in data
df <- read.table('PLD.txt',na.strings='NA',header=T)

str(df)
head(df)
dim(df)
summary(df)

# Plot PLD temp relationship (can be done a lot of ways)
# Base R
plot(pld ~ temp, data=df,
     xlab='Temperature (C)', ylab='PLD (days)',
     pch = 16,las=1)
abline(lm(pld ~ temp, data=df)) 

# ggplot
ggplot(df, aes(y = pld, x= temp)) +
        geom_point() +
        theme_classic() +
        stat_smooth(method = "lm") 



# Fit linear model and check diagnostics
lm1 <- lm(pld ~ temp, data=df)
summary(lm1)

# Diagnostic plots
par(mfrow=c(2,2))
plot(lm1)

# Histogram of residuals
par(mfrow=c(1,1))
hist(resid(lm1), breaks=20, las=1, col=1, border='white', main='')

# Assumption of homoscedasticity is violated, we will work with log(temp) and log(pld)

# Log-transform the pld and temp data
lm2 <- lm(log(pld) ~ log(temp), data=df)
par(mfrow=c(2,2))
plot(lm2)

par(mfrow=c(1,1))
hist(resid(lm2),breaks=20, las=1, col=1, border='white', main='')

# Residual plots look better

# Summary of model output
summary(lm2)
plot(log(pld) ~ log(temp), data=df,
     xlab='Temperature (C)', ylab='PLD (days)',
     pch = 16,las=1)
abline(lm2)

# With phylum memberships
ggplot(df, aes(y = log(pld), x = log(temp))) +
        geom_point() +
        theme_classic() +
        stat_smooth(method = "lm") +
        facet_wrap(~phylum)


####################
# Add random effects
library(lme4)

# Make log-transformed variables
df$l.pld <- log(df$pld)
df$l.temp <- log(df$temp)

xyplot(l.pld ~ l.temp | species, df, pch=16, type = c("p", "r"),
       ylim=c(0,10),par.strip.text=list(cex=0.5))

ggplot(df, aes(y = log(pld), x = log(temp))) +
        geom_point() +
        theme_classic() +
        stat_smooth(method = "lm",fullrange = T, se = F) +
        facet_wrap(~species)

# Evidence for which parameters to be Random Effects?

# Random intercepts
mm1 <- lmer(l.pld ~ l.temp + (1 | species), data = df)
summary(mm1)
coef(mm1)$species

# ICC
vars <- as.data.frame(VarCorr(mm1))
ICC <- vars$vcov[1] / (vars$vcov[1] + vars$vcov[2])
ICC #proportion of the total variance in Y that is accounted for by the clustering.

# Random slopes
mm2 <- lmer(l.pld ~ l.temp + ( 0 + l.temp | species), data = df)
summary(mm2)
coef(mm2)$species

# Random intercept and slope (correlated)
mm3 <- lmer(l.pld ~ l.temp + (l.temp | species), data = df)
summary(mm3)
coef(mm3)$species

# Random intercept and slope (not correlated)
#mm4 <- lmer(l.pld ~ l.temp + (1 | species) + ( 0 + l.temp | species), data = df)
#summary(mm4)
#coef(mm4)$species
#anova(mm3, mm4)

## Plotting
# There are lots of options

# ggplot2, sjPlot, and other packages can plot random effects and mixed models
# Or, be HARDCORE and plot it without a package using loops!
# Note xyplot() does not use random coefficients! 

# Plot Raw Data
xyplot(l.pld ~ l.temp | species, df, pch=16, type = c("p", "r"),
       ylim=c(0,10),par.strip.text=list(cex=0.5))

# Plot random effect on intercepts
xyplot(fitted(mm1) ~ l.temp | species, df, pch=16, type = c("p", "r"),
       ylim=c(0,10),par.strip.text=list(cex=0.5))

# Plot random effect on slopes
xyplot(fitted(mm2) ~ l.temp | species, df, pch=16, type = c("p", "r"),
       ylim=c(0,10),par.strip.text=list(cex=0.5))

# Plot random effect on intercepts and slopes
xyplot(fitted(mm3) ~ l.temp | species, df, pch=16, type = c("p", "r"),
       ylim=c(0,10),par.strip.text=list(cex=0.5))




# Simulation with 3 groups with different intercepts (same slopes)
# Modeled with fixed effects only and then a random intercept
# Mixed model produces better results
x.g <- 1:5
a1 <- 0
a2 <- 2
a3 <- 4
b <- 1
sigma2 <- 0.45
n <- 5
set.seed(8)
eps <-rnorm(n,mean=0,sd=sqrt(sigma2))
y1 <- a1 + b * x.g + eps
y2 <- a2 + b * x.g + eps
y3 <- a3 + b * x.g + eps
y <- c(y1, y2, y3)
x <- c(x.g, x.g, x.g)
g <- c(rep(1, 5),rep(2, 5),rep(3, 5))

df <- cbind(y, x, g)

df <- as.data.frame(df)

plot(y ~ x, data = df, 
     xlim = c(0,6), ylim = c(0,10), pch=16)

plot(y ~ x, data = df, col=df$g, 
     xlim = c(0,6), ylim = c(0,10), pch=16)

lm.sim <- lm(y ~ x, data = df)
summary(lm.sim)

# Random intercepts
mm.sim <- lmer(y ~ x  + (1 | g), data = df)
summary(mm.sim)
coef(mm.sim)

AIC(lm.sim, mm.sim)

# ICC
vars <- as.data.frame(VarCorr(mm.sim))
ICC <- vars$vcov[1] / (vars$vcov[1] + vars$vcov[2])
ICC #proportion of the total variance in Y that is accounted for by the clustering.


########## RE not needed
x.g <- 1:5
a1 <- 0
a2 <- 0
a3 <- 0
b <- 1
sigma2 <- 0.25
n <- 5
#set.seed(8)
eps <-rnorm(n,mean=0,sd=sqrt(sigma2))
y1 <- a1 + b * x.g + eps
eps <-rnorm(n,mean=0,sd=sqrt(sigma2))
y2 <- a2 + b * x.g + eps
eps <-rnorm(n,mean=0,sd=sqrt(sigma2))
y3 <- a3 + b * x.g + eps
y <- c(y1, y2, y3)
x <- c(x.g, (x.g +0.1), (x.g+0.2))
g <- c(rep(1, 5),rep(2, 5),rep(3, 5))

df <- cbind(y, x, g)

df <- as.data.frame(df)

plot(y ~ x, data = df, 
     xlim = c(0,6), ylim = c(0,6), pch=16)

plot(y ~ x, data = df, col=df$g, 
     xlim = c(0,6), ylim = c(0,6), pch=16)

lm.sim <- lm(y ~ x, data = df)
summary(lm.sim)

# Random intercepts
mm.sim <- lmer(y ~ x  + (1 | g), data = df)
summary(mm.sim)
coef(mm.sim)

AIC(lm.sim, mm.sim)

# ICC
vars <- as.data.frame(VarCorr(mm.sim))
ICC <- vars$vcov[1] / (vars$vcov[1] + vars$vcov[2])
ICC



