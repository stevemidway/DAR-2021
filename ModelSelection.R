
### Supplementary code for Model Selection
### Marine Science Data Analysis in R; OCS 7001
### Midway, 2021

library(MASS)
library(dplyr)
library(leaps)
library(MuMIn)
library(DAAG)
library(bootstrap)
library(tidyverse)

##################
# 1. IT Model selection Examples
##################

birds <- read.csv(url('https://raw.githubusercontent.com/stevemidway/DAR-2021/main/birds.csv'), header=T)
# Metadata: http://www.stat.ufl.edu/~winner/data/insular.txt

head(birds)

# Global (full) model
glm1 <- glm(EndemicTaxa ~ AreaKM2 + Altitude + Elevation + DistFromParamo + DistNearIsVeg + DistNearIsSouth + DistNearLargeIS, 
            data = birds, family = poisson, na.action = "na.fail")

summary(glm1)
hist(residuals(glm1))
plot(residuals(glm1) ~ fitted(glm1), pch=16, col="black")
abline(0,0)

AIC(glm1)

AICc(glm1)

BIC(glm1)

# Stepwise Regression with stepAIC()
# ?stepAIC
#forward.step <- stepAIC(glm1, direction="forward")
backward.step <- stepAIC(glm1, direction="backward")
both.step <- stepAIC(glm1, direction="both")

# Using Leaps package
leaps <- regsubsets(TotalSR ~ EndemicTaxa + AreaKM2 + Altitude + Elevation + 
                    DistFromParamo + DistNearIsVeg + DistNearIsSouth + DistNearLargeIS, 
                    data = birds,nbest=10) 
# What is this model missing?

summary(leaps)
plot(leaps,scale="r2") 
# What do you observe from this plot?

# Relative Importance
glm1.AIC.dredge <- dredge(glm1, rank = "AIC") # With great power comes great responsibility
glm1.AIC.dredge
glm1.AICc.dredge <- dredge(glm1,rank = "AICc")
glm1.BIC.dredge <- dredge(glm1,rank = "BIC")


glm1.AIC.avg <- model.avg(glm1.AIC.dredge, cumsum(weight) <= .95) # Average all models in 95% confidence set
summary(glm1.AIC.avg)
coefs <- glm1.AIC.avg$coefficients[2,]
glm1.ci <- confint(glm1.AIC.avg, level=0.95) # Calculate 95% CI
glm1.ci
glm1.imp <- glm1.AIC.avg$importance

# Compare ITs
AIC <- cbind(rownames(as.data.frame(glm1.AIC.dredge)), 
           data.frame(glm1.AIC.dredge[,11], row.names=NULL))
colnames(AIC) <- c("Model","AIC")

AICc <- cbind(rownames(as.data.frame(glm1.AICc.dredge)), 
             data.frame(glm1.AICc.dredge[,11], row.names=NULL))
colnames(AICc) <- c("Model","AICc")

BIC <- cbind(rownames(as.data.frame(glm1.BIC.dredge)), 
             data.frame(glm1.BIC.dredge[,11], row.names=NULL))
colnames(BIC) <- c("Model","BIC")

mods <- merge(BIC, AIC, by="Model")
mods <- merge(mods, AICc, by = "Model")

library(patchwork)
cor(mods$AIC, mods$AICc)
p1 <- ggplot(mods, aes(x = AIC, AICc)) +
  geom_point() +
  theme_classic(base_size = 15) +
  ggtitle("Correlation = 0.94")

cor(mods$AIC, mods$BIC)
p2 <- ggplot(mods, aes(x = AIC, BIC)) +
  geom_point() +
  theme_classic(base_size = 15) +
  ggtitle("Correlation = 0.99")

cor(mods$AICc, mods$BIC)
p3 <- ggplot(mods, aes(x = BIC, AICc)) +
  geom_point() +
  theme_classic(base_size = 15) +
  ggtitle("Correlation = 0.96")

p1 / p2 / p3

##################
# 1. Cross-validation Model selection Examples
##################
library(caret)

Hg <- read.csv(url("https://raw.githubusercontent.com/stevemidway/DAR-2021/main/fishermen_mercury.csv"))
# Metadata: http://www.stat.ufl.edu/~winner/data/fishermen_mercury.txt

head(Hg)
names(Hg)

data_ctrl <- trainControl(method = "cv", number = 3)

model_cv <- train(MeHg ~ age + weight + fishmlwk, 
                     data = Hg,                        
                     trControl = data_ctrl,             
                     method = "lm",                    
                     na.action = na.pass)

model_cv
# Want to minimize RMSE

# Look at final model fits
model_cv$finalModel

# Look at specific folds
model_cv$resample


# Methyl Mercury > 5 mg/ml is a threshold for serious mercury levels.
# Can you code a cross-validation model to find the best model among 
# the predictors restime, weight, and fishmlwk in predicting serious 
# methyl mercury levels for screening?

# Step 1: Transform predictor
Hg$serious <- ifelse(Hg$MeHg > 5, 1, 0)

glm2 <- glm(serious ~ restime, data = Hg, family = binomial) # One model to test
coefficients(glm2) # view coefficients, if curious
n <- dim(Hg)[1] # length of dataset (same as LOO)
preds <- rep(0, n) # create empty storage vector for predictions

# Loop for predictions
for(i in 1:n){
preds[i] <- 1 / (1 + exp(coefficients(glm2)[1] + coefficients(glm2)[2] * Hg$restime[i] ))
}

# With a cutoff of 50% for success, let's evlaute how this model predicted individuals with serious MeHg
compare <- abs(Hg$serious - preds)
success <- ifelse(compare < 0.5, 1, 0)
(sum(success)/length(success)) * 100 # View Cross validation as a percentage

