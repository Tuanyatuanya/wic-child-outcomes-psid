---
title: "Moderation of the WIC–Math Association by Income, Race and Age"
output: github_document
---



Three interaction models test whether the association between prenatal WIC participation and 1997 math scores varies with (log, centered) family income, child race and child age. Full write-up: [report](../reports/03_WIC_moderation_effects.pdf).


``` r
library(lmtest); library(sandwich)
good <- read.csv("../data/good.csv")
regdata <- subset(
  good,
  select = c(mathraw97, WICpreg, CHRACE, AGE97, faminc97, bthwht, HOME97)
)

regdata$RACE <- ifelse(regdata$CHRACE == 9, NA,
                 ifelse(regdata$CHRACE == 1, 0,
                 ifelse(regdata$CHRACE == 2, 1, NA)))

regdata <- na.omit(regdata)

min_inc <- min(regdata$faminc97, na.rm = TRUE)
regdata$faminc97_log <- log(regdata$faminc97 - min_inc + 1)

regdata$AGE97_c      <- as.numeric(scale(regdata$AGE97,       center = TRUE, scale = FALSE))
regdata$faminc97_c   <- as.numeric(scale(regdata$faminc97_log,center = TRUE, scale = FALSE))
regdata$HOME97_c     <- as.numeric(scale(regdata$HOME97,      center = TRUE, scale = FALSE))

# N
nrow(regdata)
```

```
## [1] 1848
```

``` r
# Continuous variables: N, mean, sd, min, max
vars_cont <- c("mathraw97", "AGE97", "faminc97", "HOME97")
descriptives_cont <- sapply(
  regdata[vars_cont],
  function(x) c(
    N    = sum(!is.na(x)),
    Mean = mean(x),
    SD   = sd(x),
    Min  = min(x),
    Max  = max(x)
  )
)
descriptives_cont
```

```
##       mathraw97       AGE97  faminc97      HOME97
## N    1848.00000 1848.000000   1848.00 1848.000000
## Mean   35.97511    7.390152  53452.15   20.234903
## SD     22.33576    2.921802  54909.07    3.096661
## Min     0.00000    3.000000      0.00    7.900000
## Max    98.00000   13.000000 784610.59   27.000000
```

``` r
# Categorical variables: frequencies and proportions
table(regdata$WICpreg)
```

```
## 
##    0    1 
## 1103  745
```

``` r
prop.table(table(regdata$WICpreg))
```

```
## 
##         0         1 
## 0.5968615 0.4031385
```

``` r
table(regdata$RACE)
```

```
## 
##   0   1 
## 970 878
```

``` r
prop.table(table(regdata$RACE))
```

```
## 
##         0         1 
## 0.5248918 0.4751082
```

``` r
table(regdata$bthwht)
```

```
## 
##    0    1 
## 1169  679
```

``` r
prop.table(table(regdata$bthwht))
```

```
## 
##         0         1 
## 0.6325758 0.3674242
```

``` r
# Correlation matrix for continuous vars
cor_matrix <- cor(
  regdata[, c("mathraw97", "AGE97", "faminc97", "HOME97")],
  use = "pairwise.complete.obs"
)
cor_matrix
```

```
##           mathraw97      AGE97   faminc97    HOME97
## mathraw97 1.0000000 0.91913234 0.15694496 0.3106784
## AGE97     0.9191323 1.00000000 0.05260234 0.1920992
## faminc97  0.1569450 0.05260234 1.00000000 0.3958002
## HOME97    0.3106784 0.19209918 0.39580016 1.0000000
```

``` r
### 1. Moderation by family income
m_income <- lm(
  mathraw97 ~ WICpreg * faminc97_c + AGE97_c + RACE + bthwht + HOME97_c,
  data = regdata
)
summary(m_income)
```

```
## 
## Call:
## lm(formula = mathraw97 ~ WICpreg * faminc97_c + AGE97_c + RACE + 
##     bthwht + HOME97_c, data = regdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -42.176  -4.733  -0.274   4.728  27.755 
## 
## Coefficients:
##                    Estimate Std. Error t value Pr(>|t|)    
## (Intercept)        38.10163    0.33458 113.878  < 2e-16 ***
## WICpreg            -1.42062    0.46674  -3.044  0.00237 ** 
## faminc97_c          1.03795    0.25982   3.995 6.73e-05 ***
## AGE97_c             6.95518    0.06749 103.050  < 2e-16 ***
## RACE               -2.25582    0.46224  -4.880 1.15e-06 ***
## bthwht             -1.92998    0.40349  -4.783 1.86e-06 ***
## HOME97_c            0.59720    0.07407   8.062 1.34e-15 ***
## WICpreg:faminc97_c -0.91173    0.34760  -2.623  0.00879 ** 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.014 on 1840 degrees of freedom
## Multiple R-squared:  0.8718,	Adjusted R-squared:  0.8713 
## F-statistic:  1787 on 7 and 1840 DF,  p-value: < 2.2e-16
```

``` r
### 2. Moderation by race
m_race <- lm(
  mathraw97 ~ WICpreg * RACE + AGE97_c + faminc97_c + bthwht + HOME97_c,
  data = regdata
)
summary(m_race)
```

```
## 
## Call:
## lm(formula = mathraw97 ~ WICpreg * RACE + AGE97_c + faminc97_c + 
##     bthwht + HOME97_c, data = regdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -41.766  -4.754  -0.089   4.797  26.808 
## 
## Coefficients:
##              Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  38.56318    0.33764 114.213  < 2e-16 ***
## WICpreg      -2.58922    0.70392  -3.678 0.000242 ***
## RACE         -3.18803    0.57401  -5.554 3.20e-08 ***
## AGE97_c       6.95055    0.06748 103.005  < 2e-16 ***
## faminc97_c    0.55320    0.18491   2.992 0.002810 ** 
## bthwht       -1.92828    0.40360  -4.778 1.91e-06 ***
## HOME97_c      0.59379    0.07416   8.007 2.07e-15 ***
## WICpreg:RACE  2.17812    0.88769   2.454 0.014232 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.015 on 1840 degrees of freedom
## Multiple R-squared:  0.8717,	Adjusted R-squared:  0.8712 
## F-statistic:  1786 on 7 and 1840 DF,  p-value: < 2.2e-16
```

``` r
# coeftest(m_race, vcov = vcovHC(m_race, type = "HC3"))

### 3. Moderation by age
m_age <- lm(
  mathraw97 ~ WICpreg * AGE97_c + faminc97_c + RACE + bthwht + HOME97_c,
  data = regdata
)
summary(m_age)
```

```
## 
## Call:
## lm(formula = mathraw97 ~ WICpreg * AGE97_c + faminc97_c + RACE + 
##     bthwht + HOME97_c, data = regdata)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -40.201  -4.739  -0.158   4.723  28.365 
## 
## Coefficients:
##                 Estimate Std. Error t value Pr(>|t|)    
## (Intercept)     38.27479    0.32341 118.346  < 2e-16 ***
## WICpreg         -1.30578    0.46356  -2.817 0.004902 ** 
## AGE97_c          7.12945    0.08588  83.018  < 2e-16 ***
## faminc97_c       0.57756    0.18470   3.127 0.001794 ** 
## RACE            -2.32082    0.46039  -5.041 5.09e-07 ***
## bthwht          -1.97795    0.40307  -4.907 1.01e-06 ***
## HOME97_c         0.61148    0.07398   8.266 2.62e-16 ***
## WICpreg:AGE97_c -0.44000    0.13035  -3.376 0.000752 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 8.004 on 1840 degrees of freedom
## Multiple R-squared:  0.8721,	Adjusted R-squared:  0.8716 
## F-statistic:  1792 on 7 and 1840 DF,  p-value: < 2.2e-16
```

``` r
# coeftest(m_age, vcov = vcovHC(m_age, type = "HC3"))
```
