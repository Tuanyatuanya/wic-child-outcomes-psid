---
title: "Prenatal WIC and AFDC Participation and Children's Reading Achievement"
output: github_document
---



Hierarchical OLS: does prenatal participation in WIC and AFDC predict children's 1997 reading scores (PSID-CDS) beyond child age, family income, low birth weight and home environment? Full write-up: [report](../reports/01_WIC_AFDC_reading_achievement.pdf).


``` r
good <- read.csv('../data/good.csv')

df <- na.omit(good[, c("WICpreg", "AFDCpreg", "readss97", "AGE97", "faminc97", "bthwht", "HOME97")])

summary(df)
```

```
##     WICpreg          AFDCpreg         readss97         AGE97       
##  Min.   :0.0000   Min.   :0.0000   Min.   : 47.5   Min.   : 3.000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.: 92.5   1st Qu.: 5.000  
##  Median :0.0000   Median :0.0000   Median :101.5   Median : 7.000  
##  Mean   :0.4011   Mean   :0.1626   Mean   :102.5   Mean   : 7.449  
##  3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:112.5   3rd Qu.:10.000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :165.5   Max.   :13.000  
##     faminc97          bthwht           HOME97     
##  Min.   :     0   Min.   :0.0000   Min.   : 7.90  
##  1st Qu.: 20687   1st Qu.:0.0000   1st Qu.:18.10  
##  Median : 43154   Median :0.0000   Median :20.60  
##  Mean   : 53559   Mean   :0.3512   Mean   :20.25  
##  3rd Qu.: 69909   3rd Qu.:1.0000   3rd Qu.:22.50  
##  Max.   :784611   Max.   :1.0000   Max.   :27.00
```

``` r
#For binary variables (proportions)
colMeans(df[, c("WICpreg", "AFDCpreg", "bthwht")])
```

```
##   WICpreg  AFDCpreg    bthwht 
## 0.4010840 0.1626016 0.3512195
```

``` r
# Model 0: Controls only
m0 <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97, data = df)
# Model 1: Add WIC
m1 <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + WICpreg, data = df)
# Model 2: Add AFDC
m2 <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + AFDCpreg, data = df)
# Model 3: Add both WIC + AFDC
m3 <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + WICpreg + AFDCpreg, data = df)

stargazer::stargazer(m0, m1, m2, m3, type = "text")
```

```
## 
## =======================================================================================================================
##                                                             Dependent variable:                                        
##                     ---------------------------------------------------------------------------------------------------
##                                                                  readss97                                              
##                               (1)                      (2)                      (3)                      (4)           
## -----------------------------------------------------------------------------------------------------------------------
## AGE97                       0.440***                 0.424***                 0.449***                 0.432***        
##                             (0.122)                  (0.122)                  (0.122)                  (0.122)         
##                                                                                                                        
## faminc97                   0.0001***                0.00005***               0.0001***                0.00005***       
##                            (0.00001)                (0.00001)                (0.00001)                (0.00001)        
##                                                                                                                        
## bthwht                     -2.531***                -2.277***                -2.341***                -2.203***        
##                             (0.738)                  (0.736)                  (0.737)                  (0.735)         
##                                                                                                                        
## HOME97                      1.398***                 1.219***                 1.281***                 1.177***        
##                             (0.122)                  (0.127)                  (0.125)                  (0.128)         
##                                                                                                                        
## WICpreg                                             -3.819***                                         -3.123***        
##                                                      (0.783)                                           (0.839)         
##                                                                                                                        
## AFDCpreg                                                                     -3.806***                 -2.397**        
##                                                                               (0.978)                  (1.045)         
##                                                                                                                        
## Constant                   68.787***                74.466***                71.858***                75.365***        
##                             (2.377)                  (2.633)                  (2.496)                  (2.659)         
##                                                                                                                        
## -----------------------------------------------------------------------------------------------------------------------
## Observations                 1,845                    1,845                    1,845                    1,845          
## R2                           0.178                    0.188                    0.185                    0.191          
## Adjusted R2                  0.176                    0.186                    0.182                    0.188          
## Residual Std. Error    14.525 (df = 1840)       14.435 (df = 1839)       14.469 (df = 1839)       14.419 (df = 1838)   
## F Statistic         99.465*** (df = 4; 1840) 85.318*** (df = 5; 1839) 83.214*** (df = 5; 1839) 72.140*** (df = 6; 1838)
## =======================================================================================================================
## Note:                                                                                       *p<0.1; **p<0.05; ***p<0.01
```

``` r
summary(m3) 
```

```
## 
## Call:
## lm(formula = readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + 
##     WICpreg + AFDCpreg, data = df)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -53.130  -8.980  -0.435   9.113  56.031 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept)  7.536e+01  2.659e+00  28.338  < 2e-16 ***
## AGE97        4.324e-01  1.217e-01   3.554 0.000389 ***
## faminc97     4.649e-05  6.918e-06   6.720 2.42e-11 ***
## bthwht      -2.203e+00  7.354e-01  -2.996 0.002772 ** 
## HOME97       1.177e+00  1.277e-01   9.221  < 2e-16 ***
## WICpreg     -3.123e+00  8.388e-01  -3.723 0.000203 ***
## AFDCpreg    -2.397e+00  1.045e+00  -2.293 0.021946 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 14.42 on 1838 degrees of freedom
## Multiple R-squared:  0.1906,	Adjusted R-squared:  0.188 
## F-statistic: 72.14 on 6 and 1838 DF,  p-value: < 2.2e-16
```

``` r
# Compare R-squared values
summary(m0)$r.squared
```

```
## [1] 0.1777854
```

``` r
summary(m1)$r.squared
```

```
## [1] 0.1882915
```

``` r
summary(m2)$r.squared
```

```
## [1] 0.1845039
```

``` r
summary(m3)$r.squared
```

```
## [1] 0.1906074
```

``` r
delta_wic <- summary(m1)$r.squared - summary(m0)$r.squared
delta_afdc <- summary(m2)$r.squared - summary(m0)$r.squared
delta_both <- summary(m3)$r.squared - summary(m0)$r.squared

cat("delta R-squared WIC:", delta_wic, "\n")
```

```
## delta R-squared WIC: 0.01050606
```

``` r
cat("delta R-squared AFDC:", delta_afdc, "\n")
```

```
## delta R-squared AFDC: 0.006718478
```

``` r
cat("delta R-squared Both:", delta_both, "\n")
```

```
## delta R-squared Both: 0.01282193
```

``` r
df_z <- as.data.frame(scale(df))
m3_z <- lm(readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + WICpreg + AFDCpreg, data = df_z)
summary(m3_z)
```

```
## 
## Call:
## lm(formula = readss97 ~ AGE97 + faminc97 + bthwht + HOME97 + 
##     WICpreg + AFDCpreg, data = df_z)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -3.3205 -0.5612 -0.0272  0.5695  3.5018 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -9.932e-16  2.098e-02   0.000 1.000000    
## AGE97        7.878e-02  2.217e-02   3.554 0.000389 ***
## faminc97     1.598e-01  2.377e-02   6.720 2.42e-11 ***
## bthwht      -6.575e-02  2.194e-02  -2.996 0.002772 ** 
## HOME97       2.283e-01  2.476e-02   9.221  < 2e-16 ***
## WICpreg     -9.568e-02  2.570e-02  -3.723 0.000203 ***
## AFDCpreg    -5.530e-02  2.411e-02  -2.293 0.021946 *  
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.9011 on 1838 degrees of freedom
## Multiple R-squared:  0.1906,	Adjusted R-squared:  0.188 
## F-statistic: 72.14 on 6 and 1838 DF,  p-value: < 2.2e-16
```
