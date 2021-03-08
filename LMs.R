# Simple Examples of General Linear Models
# Midway, 2021

# Libraries
library(tidyverse)

# Check out the data
iris
str(iris)
summary(iris)

## Model of the Mean
lm(Sepal.Length ~ 1,
   data = iris)

summary(lm(Sepal.Length ~ 1,
            data = iris))

mean(iris$Sepal.Length)
sd(iris$Sepal.Length)

hist(iris$Sepal.Length, 
     breaks = 13,
     xlim = c(0,10),
     col = 1,
     border = "white")

ggplot(iris, aes(y = Sepal.Length, x = Species)) +
  geom_jitter(width = 0.2) +
  theme_classic(base_size = 15)


## t-test
iris2 <- filter(iris, Species != "virginica")

lm(Sepal.Length ~ Species,
   data = iris2)

summary(lm(Sepal.Length ~ Species - 1,
           data = iris2))

boxplot(Sepal.Length ~ Species,
        data = iris2)

t.test(Sepal.Length ~ Species,
   data = iris2)

## ANOVA
summary(lm(Sepal.Length ~ Species,
   data = iris))

summary(lm(Sepal.Length ~ Species - 1,
           data = iris))

aov.mod <- aov(Sepal.Length ~ Species,
           data = iris)

summary(aov.mod)

TukeyHSD(aov.mod)

## Simple Linear Regression
ggplot(iris, aes(y = Sepal.Length, x = Sepal.Width)) +
  geom_point() +
  stat_smooth(method = lm, formula = y ~ x) +
  theme_classic(base_size = 15)

slr.mod <- lm(Sepal.Length ~ Petal.Width,
           data = iris)

summary(slr.mod)

plot(slr.mod)

hist(resid(slr.mod),
     breaks = 10,
     xlim = c(-2,2),
     col = 1,
     border = "white")

## Multiple Linear Regression
cor(iris[,1:4])

cor(iris$Petal.Width, iris$Petal.Length)
# NO Petal Length = same information!

cor(iris$Petal.Width, iris$Sepal.Width)
# Yes; correlation is not concerning

mlr.mod <- lm(Sepal.Length ~ Petal.Width + Sepal.Width,
           data = iris)

summary(mlr.mod)
