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
ICC # Proportion of the total variance in Y that is accounted for by the clustering.

# Random slopes
mm2 <- lmer(l.pld ~ l.temp + ( 0 + l.temp | species), data = df)
summary(mm2)
coef(mm2)$species

# Random intercept and slope (correlated)
mm3 <- lmer(l.pld ~ l.temp + (l.temp | species), data = df)
summary(mm3)
coef(mm3)$species

# Note that random slopes and random intercepts in mixed model are not the 
# same estimates as those produced by the random effects model!!!!
