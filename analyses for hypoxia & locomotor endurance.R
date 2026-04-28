library(car)
library(MASS)
library(lme4)
library(ggplot2)
library(lmerTest)
library(MuMIn)
library(emmeans)
library(visreg)

hyp <- read.csv('hypoxia.flight.endurance.csv')

end01 <- lm(duration ~ po2 + sp + po2:sp, data = hyp)
summary(end01)
# Call:
# lm(formula = duration ~ po2 + sp + po2:sp, data = hyp)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -35.770 -10.207  -1.849  10.407  33.253 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -116.5252    25.0375  -4.654 1.35e-05 ***
# po2            1.7840     0.2276   7.840 2.23e-11 ***
# splpull       99.5115    34.5378   2.881  0.00515 ** 
# spplong      -68.8807    37.9524  -1.815  0.07348 .  
# po2:splpull   -0.6940     0.3128  -2.219  0.02946 *  
# po2:spplong    0.5460     0.3434   1.590  0.11596    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 16.49 on 76 degrees of freedom
  # (3 observations deleted due to missingness)
# Multiple R-squared:  0.7493,	Adjusted R-squared:  0.7328 
# F-statistic: 45.44 on 5 and 76 DF,  p-value: < 2.2e-16

confint(end01)
                   # 2.5 %       97.5 %
# (Intercept) -166.3916086 -66.65872889
# po2            1.3307983   2.23722776
# splpull       30.7234555 168.29952690
# spplong     -144.4694611   6.70815836
# po2:splpull   -1.3169273  -0.07113329
# po2:spplong   -0.1378823   1.22984241

emtrends(end01, specs = 'sp', var = 'po2')
 # sp    po2.trend    SE df lower.CL upper.CL
 # lluc       1.78 0.228 76    1.331     2.24
 # lpull      1.09 0.215 76    0.663     1.52
 # plong      2.33 0.257 76    1.818     2.84


#### does wing loading, wing size, or body mass affect these patterns?
wl01 <- lm(duration ~ po2 + sp + po2:sp + wing.load, data = hyp)
summary(wl01) # no effect of wing loading in males that we have flight duration for
confint(wl01, parm = 'wing.load')
              # 2.5 %   97.5 %
# wing.load -360.2658 105.0931


wl02 <- lm(duration ~ po2 + sp + po2:sp + log(mass), data = hyp)
summary(wl02)
confint(wl02, parm = 'log(mass)')
              # 2.5 %   97.5 %
# log(mass) -63.93506 17.31723


wl03 <- lm(duration ~ po2 + sp + po2:sp + log(rear.wing.area), data = hyp)
summary(wl03)
confint(wl03, parm = 'log(rear.wing.area)')
                       # 2.5 %  97.5 %
# log(rear.wing.area) -56.6395 62.8168

## they do not
