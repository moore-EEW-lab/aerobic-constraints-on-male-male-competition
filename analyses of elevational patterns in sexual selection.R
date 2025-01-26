library(car)
library(MASS)
library(ggplot2)
library(lme4)
library(lmerTest)
library(visreg)
library(emmeans)
library(metafor)

ssd <- read.csv('elevation.ss.csv')


inf01 <-  lmer(linear.coefs ~ elevation + abs.Lat + (1|StudyID) + (1|Species), data = ssd)
summary(inf01)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: linear.coefs ~ elevation + abs.Lat + (1 | StudyID) + (1 | Species)
   # Data: ssd

# REML criterion at convergence: 123.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -2.4853 -0.5622 -0.1896  0.4424  3.7070 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # StudyID  (Intercept) 0.009737 0.09868 
 # Species  (Intercept) 0.012824 0.11325 
 # Residual             0.087014 0.29498 
# Number of obs: 184, groups:  StudyID, 25; Species, 19

# Fixed effects:
              # Estimate Std. Error         df t value Pr(>|t|)   
# (Intercept)  5.975e-01  2.151e-01  3.155e+01   2.777  0.00915 **
# elevation   -1.260e-04  5.771e-05  1.329e+02  -2.183  0.03079 * 
# abs.Lat     -7.146e-03  4.447e-03  2.668e+01  -1.607  0.11986   
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
          # (Intr) elevtn
# elevation -0.258       
# abs.Lat   -0.975  0.168

confint(inf01, parm = 'elevation')
                  # 2.5 %        97.5 %
# elevation -0.0002415919 -9.176224e-06


### in informal meta-analytical model across 184 estimates from 19 species, sexual selection for larger bodies declines towards higher elevations

### formal meta-analysis. Much smaller dataset because many estimates do not have standard errors
form01 <- rma.mv(linear.coefs ~ elevation + abs.Lat, random = list(~1|StudyID, ~1|Species),  V = (ssd$ses)^2, data = ssd)

form01$beta[2]
#-0.0002321817
form01$se[2]
#0.0001162154

form01$ci.ub[2] # upper CI for elevation
# -4.403672e-06
form01$ci.lb[2] # lower CI for elevation
# -0.0004599597

### in formal meta-analysis on the smaller dataset for which SE were available, sexual selection favors large bodies at low elevations but not at high elevations


## comparison of endo- vs ectotherm
inf02 <-  lmer(linear.coefs ~ elevation + abs.Lat + Endo.Ecto + Endo.Ecto:elevation + (1|StudyID) + (1|Species), data = ssd)
confint(inf02) # no difference between endotherms and ectotherms
                                # 2.5 %       97.5 %
# .sig01                   0.0000000000 2.276787e-01
# .sig02                   0.0000000000 2.381087e-01
# .sigma                   0.2658904671 3.325211e-01
# (Intercept)              0.1847339606 1.112260e+00
# elevation               -0.0002916198 2.682224e-05
# abs.Lat                 -0.0173234271 1.429613e-03
# Endo.EctoEndo           -0.4632174050 3.257535e-01
# elevation:Endo.EctoEndo -0.0003009872 3.992899e-04

## comparison between vertebrates and invertebrates
inf03 <- lmer(linear.coefs ~ elevation + abs.Lat + Taxon.group + Taxon.group:elevation + (1|StudyID) + (1|Species), data = ssd)
confint(inf03) # no difference between verts and inverts

                               # 2.5 %        97.5 %
# .sig01                  0.0000000000  2.240525e-01
# .sig02                  0.0000000000  2.261355e-01
# .sigma                  0.2630573962  3.277208e-01
# (Intercept)             0.1695883546  1.121822e+00
# elevation              -0.0003111008 -3.616189e-06
# abs.Lat                -0.0163873363  1.620236e-03
# Taxon.groupV           -0.2944242804  1.266284e-01
# elevation:Taxon.groupV -0.0001536527  3.507418e-04