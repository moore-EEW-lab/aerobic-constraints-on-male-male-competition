library(car)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(visreg)
library(MuMIn)
library(viridis)



alt <- read.csv('morph.elev.csv')


### are territorial males smaller at high elevations?
size01 <- lmer(log(total.length) ~ log(elevation) + (1|spp) + (1|genus) + (1|family), data = alt)
summary(size01)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(total.length) ~ log(elevation) + (1 | spp) + (1 | genus) +      (1 | family)
   # Data: alt

# REML criterion at convergence: -288

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -5.4345 -0.3430  0.0966  0.5067  1.9670 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # spp      (Intercept) 0.013171 0.11476 
 # genus    (Intercept) 0.006276 0.07922 
 # family   (Intercept) 0.047166 0.21718 
 # Residual             0.006304 0.07940 
# Number of obs: 157, groups:  spp, 19; genus, 13; family, 4

# Fixed effects:
                # Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)      2.08875    0.30026  80.68529   6.957 8.31e-10 ***
# log(elevation)  -0.07937    0.03601 148.72882  -2.204    0.029 *  

confint(size01)

                     # 2.5 %       97.5 %
# .sig01          0.06123872  0.204438578
# .sig02          0.00000000  0.186313312
# .sig03          0.08511913  0.485752268
# .sigma          0.07058977  0.089544192
# (Intercept)     1.50357619  2.674109604
# log(elevation) -0.15087175 -0.008186841

### yes!


## if we look within two well-sampled speccies, are territorial males smaller at high elevations? 
size02 <- lmer(log(total.length) ~ log(elevation) + (1|spp), data = subset(alt, spp %in% c('quadrimaculata', 'palmata')))
summary(size02)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(total.length) ~ log(elevation) + (1 | spp)
   # Data: subset(alt, spp %in% c("quadrimaculata", "palmata"))

# REML criterion at convergence: -178.5

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -3.2670 -0.4160  0.1030  0.6006  1.6759 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # spp      (Intercept) 0.116242 0.34094 
 # Residual             0.004577 0.06765 
# Number of obs: 77, groups:  spp, 2

# Fixed effects:
               # Estimate Std. Error       df t value Pr(>|t|)   
# (Intercept)     2.10351    0.34553  4.14757   6.088  0.00327 **
# log(elevation) -0.07214    0.03160 74.01035  -2.283  0.02531 * 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr)
# log(elevtn) -0.716


confint(size02)
                     # 2.5 %       97.5 %
# .sig01          0.11435906  1.009586290
# .sigma          0.05772648  0.079574203
# (Intercept)     1.42762542  2.781899276
# log(elevation) -0.13418066 -0.009529236

### yes again!

### do high-elevation males have lower body condition? Use covariate method
bc01 <- lmer(body.mass ~ elevation + total.length + (total.length|spp) + (1|genus) + (1|family), data = alt)
summary(bc01)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: body.mass ~ elevation + total.length + (total.length | spp) +      (1 | genus) + (1 | family)
   # Data: alt

# REML criterion at convergence: -278.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -2.3076 -0.3500 -0.0652  0.3721  3.2301 

# Random effects:
 # Groups   Name         Variance  Std.Dev. Corr 
 # spp      (Intercept)  6.871e-03 0.082894      
          # total.length 6.145e-05 0.007839 -1.00
 # genus    (Intercept)  1.830e-02 0.135263      
 # family   (Intercept)  3.166e-02 0.177946      
 # Residual              3.001e-03 0.054778      
# Number of obs: 121, groups:  spp, 17; genus, 12; family, 4

# Fixed effects:
               # Estimate Std. Error         df t value Pr(>|t|)  
# (Intercept)   2.048e-01  1.279e-01  7.176e+00   1.601   0.1524  
# elevation    -7.486e-06  1.148e-05  1.088e+02  -0.652   0.5156  
# total.length  2.876e-02  1.563e-02  5.937e+01   1.841   0.0706 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) elevtn
# elevation   -0.253       
# total.lngth -0.569  0.104
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see ?isSingular

confint(bc01, parm = 'elevation')
                  # 2.5 %       97.5 %
# elevation -3.042304e-05 1.519798e-05


## no relationship between body mass and elevation after controlling for total length. Do not have lower body condition at high elevations

#### if we look within those two well-sampled species, do territorial males have lower body condition at high elevation?
bc02 <- lmer(body.mass ~ elevation + total.length + (total.length|spp) + (1|genus) + (1|family), data = subset(alt, spp %in%c('quadrimaculata', 'palmata')))
summary(bc02)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: body.mass ~ elevation + total.length + (total.length | spp) +      (1 | genus) + (1 | family)
   # Data: subset(alt, spp %in% c("quadrimaculata", "palmata"))

# REML criterion at convergence: -140.9

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.01431 -0.39786 -0.06487  0.39410  3.05896 

# Random effects:
 # Groups   Name         Variance  Std.Dev.  Corr 
 # spp      (Intercept)  2.730e-02 1.652e-01      
          # total.length 2.081e-05 4.562e-03 -1.00
 # genus    (Intercept)  1.452e-07 3.810e-04      
 # family   (Intercept)  4.385e-10 2.094e-05      
 # Residual              3.191e-03 5.649e-02      
# Number of obs: 61, groups:  spp, 2; genus, 2; family, 2

# Fixed effects:
               # Estimate Std. Error         df t value Pr(>|t|)  
# (Intercept)   2.507e-01  1.752e-01  1.476e+00   1.431   0.3277  
# elevation    -2.699e-06  1.220e-05  5.730e+01  -0.221   0.8257  
# total.length  4.400e-02  2.533e-02  3.628e+01   1.737   0.0909 .
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
            # (Intr) elevtn
# elevation   -0.263       
# total.lngth -0.802  0.123
# optimizer (nloptwrap) convergence code: 0 (OK)
# boundary (singular) fit: see help('isSingular')

confint(bc02, parm = 'elevation')
                  # 2.5 %       97.5 %
# elevation -2.437559e-05 2.438817e-05

### no pattern when we pool across species


## what about if we look within each species one at a time
# body condition for libellula quadrimaculata
bc.lq <- lm(body.mass ~ elevation + total.length, data = subset(alt, spp == 'quadrimaculata'))
summary(bc.lq)

# Call:
# lm(formula = body.mass ~ elevation + total.length, data = subset(alt, 
    # spp == "quadrimaculata"))

# Residuals:
      # Min        1Q    Median        3Q       Max 
# -0.113551 -0.031748 -0.007336  0.027836  0.171770 

# Coefficients:
               # Estimate Std. Error t value Pr(>|t|)
# (Intercept)   1.552e-01  2.702e-01   0.574    0.570
# elevation    -6.154e-07  1.779e-05  -0.035    0.973
# total.length  4.127e-02  7.012e-02   0.589    0.560

# Residual standard error: 0.07014 on 31 degrees of freedom
  # (13 observations deleted due to missingness)
# Multiple R-squared:  0.01135,	Adjusted R-squared:  -0.05243 
# F-statistic: 0.178 on 2 and 31 DF,  p-value: 0.8378

confint(bc.lq, parm = 'elevation')
                 # 2.5 %       97.5 %
# elevation -3.69046e-05 3.567374e-05

## nothing doing for quadrimaculata

## body condition for Aeshna palmata
bc.ap <- lm(body.mass ~ elevation + total.length, data = subset(alt, spp == 'palmata'))
summary(bc.ap)

# Call:
# lm(formula = body.mass ~ elevation + total.length, data = subset(alt, 
    # spp == "palmata"))

# Residuals:
     # Min       1Q   Median       3Q      Max 
# -0.08324 -0.01457  0.00068  0.02272  0.06676 

# Coefficients:
               # Estimate Std. Error t value Pr(>|t|)   
# (Intercept)   4.415e-01  1.261e-01   3.501  0.00184 **
# elevation    -1.076e-05  1.440e-05  -0.747  0.46214   
# total.length  3.143e-02  1.882e-02   1.670  0.10784   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 0.03474 on 24 degrees of freedom
  # (4 observations deleted due to missingness)
# Multiple R-squared:  0.1435,	Adjusted R-squared:  0.07208 
# F-statistic:  2.01 on 2 and 24 DF,  p-value: 0.156

confint(bc.ap, parm = 'elevation')
                 # 2.5 %       97.5 %
# elevation -4.04889e-05 1.896236e-05

## nothing doing here either