library(car)
library(MASS)
library(lme4)
library(ggplot2)
library(lmerTest)
library(MuMIn)
library(emmeans)
library(visreg)

hyp <- read.csv('hypoxia.fligth.endurance.csv')

end01 <- lm(duration ~ po2 + sp + po2:sp, data = hyp)
summary(end01)
# Call:
# lm(formula = duration ~ po2 + sp + po2:sp, data = hyp)

# Residuals:
    # Min      1Q  Median      3Q     Max 
# -32.181  -9.163  -2.022  10.196  33.253 

# Coefficients:
             # Estimate Std. Error t value Pr(>|t|)    
# (Intercept) -116.5252    24.5256  -4.751 1.54e-05 ***
# po2            1.7840     0.2229   8.004 9.74e-11 ***
# splpull       99.5115    33.8318   2.941   0.0048 ** 
# po2:splpull   -0.6940     0.3064  -2.265   0.0275 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 16.15 on 54 degrees of freedom
  # (3 observations deleted due to missingness)
# Multiple R-squared:  0.6979,	Adjusted R-squared:  0.6811 
# F-statistic: 41.59 on 3 and 54 DF,  p-value: 4.596e-14

confint(end01)
                  # 2.5 %       97.5 %
# (Intercept) -165.696097 -67.35424036
# po2            1.337119   2.23090655
# splpull       31.682876 167.34010675
# po2:splpull   -1.308239  -0.07982113

emtrends(end01, specs = 'sp', var = 'po2')
 # sp    po2.trend    SE df lower.CL upper.CL
 # lluc       1.78 0.223 54    1.337     2.23
 # lpull      1.09 0.210 54    0.669     1.51


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