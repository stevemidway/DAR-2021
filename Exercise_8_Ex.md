Exercise 8: Random Effects
================
DAR Lab
Spring 2021

### 1. Find a dataset with a factor that could be considered a random effect.

For my investigation into random effects, I choose to look at the
`Orange` data set in the library `MASS`. The `Orange` data set includes
35 observations of 3 variables. I plan to consider `circumference` as
the response variable, `age` as the predictor variable, and `Tree` as a
possible random effect. To first explore the data, I have created a
visual (Figure 1) of the `age` and `cirumference` relationship by
`Tree`.

![Figure 1: Orange tree circumference by age for five individual orange
trees. The orange dots represent the data and the blue line represent
the OLS fits for each tree. In other words, no random effects have yet
been fit.](Exercise_8_Ex_files/figure-gfm/unnamed-chunk-1-1.png)

Although the relationship of `circumference` and `age` appears similar,
which we might expect, there are difference among different trees, and
this information combined with the fact that individual organisms are a
common type of random effect, leads me to evalute `Tree` as a random
effect. Additionally, an ICC value from a mixed model with a random
intercept was estimated to be 0.63, which is not extremely high, but
certaintly suggests within group variance that would benefit from a
random effect. (See code below for example of the model and ICC
estimation.)

``` r
library(lme4)
mm1 <- lmer(circumference ~ age + (1 | Tree), data = Orange)
summary(mm1)
vars <- as.data.frame(VarCorr(mm1))
ICC <- vars$vcov[1] / (vars$vcov[1] + vars$vcov[2])
ICC # Proportion of the total variance in Y that is accounted for by the clustering.
```

### 2. Using the `lme4` library, run a simple model of your data.

I have choosen to run a random effects model, in which both the
intercept and slope estimates are random effects. This allows for the
maximum flexibility in group-specific fits, and if one parameter does
not vary much, the estimates will simply default to looking like fixed
effects estimates. Below is the code for the random effects model I ran.

``` r
library(lme4)
RE.mod <- lmer(circumference ~ age + (age | Tree), data = Orange)
summary(RE.mod)
```

    ## Linear mixed model fit by REML ['lmerMod']
    ## Formula: circumference ~ age + (age | Tree)
    ##    Data: Orange
    ## 
    ## REML criterion at convergence: 281.1
    ## 
    ## Scaled residuals: 
    ##      Min       1Q   Median       3Q      Max 
    ## -2.09099 -0.50176 -0.07625  0.71181  1.63662 
    ## 
    ## Random effects:
    ##  Groups   Name        Variance  Std.Dev. Corr
    ##  Tree     (Intercept) 8.312e+00  2.88310     
    ##           age         5.083e-04  0.02255 0.99
    ##  Residual             1.016e+02 10.07726     
    ## Number of obs: 35, groups:  Tree, 5
    ## 
    ## Fixed effects:
    ##             Estimate Std. Error t value
    ## (Intercept) 17.39965    3.88098   4.483
    ## age          0.10677    0.01068   9.999
    ## 
    ## Correlation of Fixed Effects:
    ##     (Intr)
    ## age 0.037 
    ## optimizer (nloptwrap) convergence code: 0 (OK)
    ## unable to evaluate scaled gradient
    ## Model failed to converge: degenerate  Hessian with 1 negative eigenvalues

The random intercepts and random slopes were estimated to be:

``` r
coef(RE.mod)
```

    ## $Tree
    ##   (Intercept)        age
    ## 3    14.70720 0.08553844
    ## 1    15.26656 0.08987979
    ## 5    17.02026 0.10388894
    ## 2    19.70009 0.12485999
    ## 4    20.30415 0.12968446
    ## 
    ## attr(,"class")
    ## [1] "coef.mer"

### 3. Include a figure of your data and model.

Athough the data are shown in Figure 1, Figure 2 shows the tree-specific
the random effects model fits.

    ##   Tree intercept        age
    ## 1    3  14.70720 0.08553844
    ## 2    1  15.26656 0.08987979
    ## 3    5  17.02026 0.10388894
    ## 4    2  19.70009 0.12485999
    ## 5    4  20.30415 0.12968446

![Figure 2: Orange tree circumference by age for five individual orange
trees. The orange dots represent the data and the black lines represent
the random effects model
fit.](Exercise_8_Ex_files/figure-gfm/unnamed-chunk-5-1.png)

And based on this helpful
[tutorial](http://rstudio-pubs-static.s3.amazonaws.com/281747_c093ad256da84672b0cb43b5125290b6.html),
we can see the tree-specific fits with all possible regression fits.

![Figure 3: Orange tree circumference by age for five individual orange
trees. The orange dots represent the data and the different model fits
represent different
models.](Exercise_8_Ex_files/figure-gfm/unnamed-chunk-6-1.png)

#### Code used for Figure 3

``` r
ab_lines <- coef(RE.mod)[["Tree"]] %>% 
  tibble::rownames_to_column("Tree") %>% 
  rename(intercept = `(Intercept)`) %>% 
  tibble::add_column(Model = "Random Effects")

ab_lines2 <- lmList(circumference ~ age | Tree, Orange) %>% 
  coef() %>% 
  tibble::rownames_to_column("Tree") %>% 
  rename(intercept = `(Intercept)`) %>% 
  tibble::add_column(Model = "OLS Fit")

ab_lines3 <- data_frame(
  Tree = unique(Orange$Tree),
  intercept = coef(lm(circumference ~ age, Orange))[1],
  age = coef(lm(circumference ~ age, Orange))[2],
  Model = "No Tree Effect"
)

all_lines <- bind_rows(ab_lines, ab_lines2, ab_lines3)

ggplot(Orange) + 
  aes(x = age, y = circumference) + 
  geom_abline(aes(intercept = intercept, slope = age, color = Model), data = all_lines) +
  geom_point(color = "orange") +
  theme_classic(base_size = 15) +
  facet_wrap(~Tree)
```
