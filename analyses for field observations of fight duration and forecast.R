library(car)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(visreg)
library(MuMIn)
library(viridis)

fights <- read.csv('fights.altitude.csv')


## model set. just temp (cold constraints), temp + elev (aerobic constraints), temp + pond size (differences in hab availability at diff elevs), elev + activity (diffs in temp are just a function of differences in encounter rate), activity + temp (diffs in elev are just differences in pop size, etc), activity + elev + temp (test for effects of elev and temp after accounting for activity), global model> just bobyqa optimizer to help with convergence
mod00a <- lmer(log(duration) ~ log(temp) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F,control = lmerControl(optimizer = 'bobyqa'))
mod00b <- lmer(log(duration) ~ log(elev) + log(temp) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))
mod00c <- lmer(log(duration) ~ log(temp) + log(pond.size) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))
mod00d <- lmer(log(duration) ~ log(elev) + log(f.per.h) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))
mod00e <- lmer(log(duration) ~ log(f.per.h) + log(temp) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))
mod00f <- lmer(log(duration) ~ log(elev) + log(temp) + log(f.per.h) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))
mod00g <- lmer(log(duration) ~ log(elev) + log(temp) + log(pond.size) + log(f.per.h) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = F, control = lmerControl(optimizer = 'bobyqa'))

aics <- AICc(mod00a, mod00b, mod00c, mod00d, mod00e, mod00f, mod00g) # best model includes just temp and elevation. model with activity and global model are both close
# mod00a  8 2124.470
# mod00b  9 2085.080
# mod00c  9 2125.549
# mod00d  9 2093.013
# mod00e  9 2125.390
# mod00f 10 2086.170
# mod00g 11 2088.136


Weights(aics) # model with temp and elev gets most weight. global model and model with activity also decent. Worth looking at param ests for each
# 0.000 0.551 0.000 0.010 0.000 0.319 0.120

#### patterns for best model
mod01 <- lmer(log(duration) ~ log(elev) + log(temp) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = TRUE, control = lmerControl(optimizer = 'bobyqa'))
summary(mod01)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | spp) + (1 | date) +      (1 | locat) + (1 | genus) + (1 | family)
   # Data: fights

# REML criterion at convergence: 2071.4

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -3.2451 -0.6630  0.0152  0.6526  3.1703 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # date     (Intercept) 5.006e-02 2.237e-01
 # spp      (Intercept) 1.044e-02 1.022e-01
 # locat    (Intercept) 6.888e-03 8.299e-02
 # genus    (Intercept) 7.780e-02 2.789e-01
 # family   (Intercept) 2.995e-10 1.731e-05
 # Residual             3.018e-01 5.494e-01
# Number of obs: 1217, groups:  date, 18; spp, 17; locat, 16; genus, 11; family, 3

# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  28.9085     2.7675 17.4526  10.446 6.24e-09 ***
# log(elev)    -3.0877     0.2556 12.6624 -12.080 2.56e-08 ***
# log(temp)    -1.1678     0.3342 28.9849  -3.494  0.00155 ** 

confint(mod01, parm = 'log(elev)')
              # 2.5 %     97.5 %
# log(elev) -3.598266 -2.5419047

confint(mod01, parm = 'log(temp)')
# log(temp) -1.820168 -0.5081843

#### patterns for second best model
mod02 <- lmer(log(duration) ~ log(elev) + log(temp) + log(f.per.h) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = T, control = lmerControl(optimizer = 'bobyqa'))
summary(mod02)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + log(f.per.h) + (1 | spp) +      (1 | date) + (1 | locat) + (1 | genus) + (1 | family)
   # Data: fights
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 2074.1

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -3.2429 -0.6786  0.0094  0.6503  3.1754 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # date     (Intercept) 0.055481 0.23554 
 # spp      (Intercept) 0.010701 0.10345 
 # locat    (Intercept) 0.004965 0.07046 
 # genus    (Intercept) 0.073365 0.27086 
 # family   (Intercept) 0.000000 0.00000 
 # Residual             0.301944 0.54949 
# Number of obs: 1217, groups:  date, 18; spp, 17; locat, 16; genus, 11; family, 3

# Fixed effects:
             # Estimate Std. Error       df t value Pr(>|t|)    
# (Intercept)  28.11956    2.99744 21.87856   9.381 4.01e-09 ***
# log(elev)    -2.99540    0.29023 18.06090 -10.321 5.30e-09 ***
# log(temp)    -1.07324    0.35556 26.93624  -3.018   0.0055 ** 
# log(f.per.h) -0.05906    0.07831  7.69831  -0.754   0.4732    

confint(mod02)[c(8:10), c(1,2)]
                  # 2.5 %      97.5 %
# log(elev)    -3.5346399 -2.45204400
# log(temp)    -1.7271127 -0.34512228
# log(f.per.h) -0.2178335  0.09975739

##### patterns for third best model
mod03 <- lmer(log(duration) ~ log(elev) + log(temp) + log(pond.size) + log(f.per.h) + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = fights, na.action = na.fail, REML = T, control = lmerControl(optimizer = 'bobyqa'))
summary(mod03)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + log(pond.size) + log(f.per.h) +      (1 | spp) + (1 | date) + (1 | locat) + (1 | genus) + (1 |      family)
   # Data: fights
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 2077.7

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -3.2417 -0.6784  0.0101  0.6480  3.1766 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # date     (Intercept) 5.462e-02 2.337e-01
 # spp      (Intercept) 1.097e-02 1.047e-01
 # locat    (Intercept) 8.645e-03 9.298e-02
 # genus    (Intercept) 7.320e-02 2.706e-01
 # family   (Intercept) 3.642e-15 6.035e-08
 # Residual             3.019e-01 5.494e-01
# Number of obs: 1217, groups:  date, 18; spp, 17; locat, 16; genus, 11; family, 3

# Fixed effects:
                # Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)    27.968041   3.634329 20.807792   7.696 1.62e-07 ***
# log(elev)      -2.985059   0.310889 15.893817  -9.602 5.11e-08 ***
# log(temp)      -1.079776   0.413628 32.062240  -2.610   0.0136 *  
# log(pond.size)  0.006335   0.069727  6.510289   0.091   0.9303    
# log(f.per.h)   -0.045344   0.107834 12.070652  -0.420   0.6815    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

confint(mod03)
                    # 2.5 %     97.5 %
# .sig01          0.1418164  0.3605978
# .sig02          0.0000000  0.3277715
# .sig03          0.0000000  0.1958284
# .sig04          0.0000000  0.4805378
# .sig05          0.0000000  0.4197145
# .sigma          0.5282194  0.5726160
# (Intercept)    20.9687633 34.3650542
# log(elev)      -3.5351911 -2.4270952
# log(temp)      -1.7931027 -0.1820043
# log(pond.size) -0.1130302  0.1289113
# log(f.per.h)   -0.2511211  0.1451591


###### okay, so in all three models with support, same pattern. Strong neg effects of elev and temp. Even in two decently supported models, CIs for activity and pond size overlap 0. So no real evidence for effect of either activity or habitat size


### Let's look within the four broad-elevation species
# libellula quadrimaculata
lq <- subset(fights, spp == 'lq')
mod02lq <- lmer(log(duration) ~ log(elev) + log(temp) + (1|date) + (1|locat), data = lq, REML = TRUE, control = lmerControl(optimizer = 'bobyqa'))
summary(mod02lq)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | date) + (1 | locat)
   # Data: lq
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 826.7

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.85076 -0.69903  0.00562  0.62342  2.95232 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # date     (Intercept) 0.009641 0.09819 
 # locat    (Intercept) 0.031606 0.17778 
 # Residual             0.269129 0.51878 
# Number of obs: 530, groups:  date, 9; locat, 7

# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  41.5606     5.1502 10.3490   8.070 8.82e-06 ***
# log(elev)    -4.2785     0.6187  8.8283  -6.915 7.63e-05 ***
# log(temp)    -2.1033     0.3433  4.4788  -6.127  0.00247 ** 

confint(mod02lq)
                 # 2.5 %     97.5 %
# .sig01       0.0000000  0.3529170
# .sig02       0.0000000  0.3242939
# .sigma       0.4885589  0.5516917
# (Intercept) 31.7851390 51.1501942
# log(elev)   -5.4420639 -3.0885032
# log(temp)   -2.8259280 -1.4289687


# Libellula pulchella
lpull <- subset(fights, spp == 'lpull')
mod02lpull <- lmer(log(duration) ~ log(elev) + log(temp) + (1|date) + (1|locat), data = lpull, REML = TRUE, control = lmerControl(optimizer = 'bobyqa'))
summary(mod02lpull)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | date) + (1 | locat)
   # Data: lpull
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 88.3

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -2.4242 -0.5056 -0.1461  0.5471  3.4649 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # date     (Intercept) 1.886e-02 1.373e-01
 # locat    (Intercept) 4.514e-13 6.719e-07
 # Residual             1.853e-01 4.304e-01
# Number of obs: 74, groups:  date, 8; locat, 7

# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)  20.2568     5.1932  7.3671   3.901  0.00533 ** 
# log(elev)    -2.4703     0.4282  7.9959  -5.769  0.00042 ***
# log(temp)     0.1651     0.8932  7.3423   0.185  0.85833    
# ---


confint(mod02lpull, parm = 'log(elev)') # -3.248832 -1.513231
confint(mod02lpull, parm = 'log(temp)') # -1.458866 1.865899

# Aeshna palmata
palm <- subset(fights, spp == 'palm')
mod02palm <- lmer(log(duration) ~ log(elev) + log(temp) + (1|date) + (1|locat), data = palm, REML = TRUE, control = lmerControl(optimizer = 'bobyqa'))
summary(mod02palm)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | date) + (1 | locat)
   # Data: palm
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 320.9

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.55077 -0.67007  0.03248  0.63093  2.85208 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # date     (Intercept) 0.06394  0.2529  
 # locat    (Intercept) 0.00000  0.0000  
 # Residual             0.29645  0.5445  
# Number of obs: 192, groups:  date, 8; locat, 8

# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)  21.2667     9.8929  6.8129   2.150   0.0697 .
# log(elev)    -2.6868     0.7916  6.2579  -3.394   0.0137 *
# log(temp)     0.2999     1.3058  8.6251   0.230   0.8237  


confint(mod02palm)


# Plathemis lydia
plyd <- subset(fights, spp == 'ply')
mod02ply <- lmer(log(duration) ~ log(elev) + log(temp) + (1|date) + (1|locat), data = plyd, REML = TRUE, control = lmerControl(optimizer = 'bobyqa'))
summary(mod02ply)
# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | date) + (1 | locat)
   # Data: plyd
# Control: lmerControl(optimizer = "bobyqa")

# REML criterion at convergence: 101.6

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.66795 -0.62263  0.04577  0.58232  1.96415 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # date     (Intercept) 1.999e-15 4.471e-08
 # locat    (Intercept) 0.000e+00 0.000e+00
 # Residual             4.228e-01 6.503e-01
# Number of obs: 53, groups:  date, 7; locat, 6

# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)   
# (Intercept)  28.3769     8.9889 50.0000   3.157  0.00270 **
# log(elev)    -2.4645     0.9135 50.0000  -2.698  0.00949 **
# log(temp)    -2.2745     0.9966 50.0000  -2.282  0.02677 * 


confint(mod02ply)
                 # 2.5 %     97.5 %
# .sig01       0.0000000  0.2626709
# .sig02       0.0000000  0.2661544
# .sigma       0.5280683  0.7739415
# (Intercept) 10.9500315 45.8036866
# log(elev)   -4.2354522 -0.6934983
# log(temp)   -4.2074960 -0.3415539

####### elevation effect of similar size within all FOUR species


##### Do broad-elevation species in low-elevations fight like low-elevation species?

low.test <- subset(fights, spp%in% c('lq', 'lpull', 'palm', 'dash', 'll', 'pten', 'simp', 'sympobs') & elev < 2100)
low.test$e.range <- ifelse(low.test$spp %in% c('lq', 'lpull', 'palm'), 'wide', 'rest')

lt01 <- lmer(log(duration) ~ e.range + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = low.test, na.action = na.fail, REML = TRUE)
summary(lt01)
# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)    
# (Intercept)   1.9067     0.1544  5.7266  12.350 2.41e-05 ***
# e.rangewide   0.1669     0.1261 42.4159   1.323    0.193    
confint(lt01)
                 # 2.5 %    97.5 %
# .sig01       0.0000000 0.3696464
# .sig02       0.0000000 0.2337298
# .sig03       0.0000000 0.5753824
# .sig04       0.0000000 0.3721728
# .sig05       0.0000000 0.5457386
# .sigma       0.4923516 0.5825012
# (Intercept)  1.4530082 2.2286937
# e.rangewide -0.1082191 0.4947396

exp(1.9067 + 0.1669) - exp(1.9067) # backtransformed difference in fight duration = 1.222564 s longer
exp(1.9067 + 0.1669) / exp(1.9067) # % diff 1.181636

exp(1.9067 -0.1082191) - exp(1.9067) # lower CI for difference in fight duration = -0.6903759 s shorter
exp(1.9067 -0.1082191) / exp(1.9067) # lower CI for x-fold difference in fight duration = 0.8974309x

exp(1.9067 + 0.4947396) - exp(1.9067) # upper CI for difference in fight duration = 4.308216 s longer
exp(1.9067 + 0.4947396) / exp(1.9067) # upper CI for x-fold difference in fight duration = 1.640071x 

## same duration


##### Do broad-elevation species in high elevations fight like high elevation species?
high.test <- subset(fights, spp%in% c('lq', 'lpull', 'palm', 'belt', 'hud', 'amem', 'erem', 'mt') & elev > 2800)
high.test$e.range <- ifelse(high.test$spp %in% c('lq', 'lpull', 'palm'), 'wide', 'rest')

wt01 <- lmer(log(duration) ~ e.range + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|locat), data = high.test, na.action = na.fail, REML = TRUE)
qqnorm(resid(wt01))
plot(resid(wt01) ~ fitted(wt01))

summary(wt01)
# Fixed effects:
            # Estimate Std. Error      df t value Pr(>|t|)  
# (Intercept)   0.7061     0.1693  4.4291   4.170   0.0113 *
# e.rangewide  -0.1463     0.1563 13.2711  -0.936   0.3660  


confint(wt01)
                 # 2.5 %    97.5 %
# .sig01       0.0000000 0.7104228
# .sig02       0.0000000 0.3523531
# .sig03       0.0000000 0.2129067
# .sig04       0.0000000 0.2129906
# .sig05       0.0000000 0.6321231
# .sigma       0.5670807 0.6778846
# (Intercept)  0.3580372 1.0422212
# e.rangewide -0.4569885 0.2828652

exp(0.7061 -0.1463) - exp(0.7061) # backtransformed difference in fight duration = -0.2757517 s
exp(0.7061 -0.1463) / exp(0.7061) # % dif = 0.8638985

exp(0.7061 -0.4569885) - exp(0.7061) # lower CI for difference in fight duration = -0.7431891
exp(0.7061 -0.4569885) / exp(0.7061) # lower CI for x-fold dif = 0.6331876x

exp(0.7061 + 0.2828652) - exp(0.7061) # upper CI for difference in fight duration = 0.6623769
exp(0.7061 + 0.2828652) / exp(0.7061) # upper CI for x-fold dif = 1.326926x

### same duration

## Do broad elevation species fight in low-elevations have longer fights than high-elevation species? 
low.sub <- subset(low.test, e.range == 'wide')
high.sub <- subset(high.test, e.range == 'rest')

cross.test <- rbind(low.sub, high.sub)
ct01 <- lmer(log(duration) ~ e.range + (1|spp) + (1|date) + (1|locat) + (1|genus) + (1|family), data = cross.test, na.action = na.fail, REML = TRUE)
summary(ct01)
# Fixed effects:
            # Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)   0.7308     0.1959 5.6729   3.730 0.010784 *  
# e.rangewide   1.1907     0.2146 8.7605   5.547 0.000394 ***

confint(ct01)
                 # 2.5 %    97.5 %
# .sig01      0.00000000 0.3172100
# .sig02      0.06832379 0.3882725
# .sig03      0.00000000 6.1733130
# .sig04      0.13241598 0.7178571
# .sig05      0.00000000 0.4998222
# .sigma      0.53986478 0.6361094
# (Intercept) 0.33996731 1.1121904
# e.rangewide 0.78834767 1.6300698



exp(0.7308 + 1.1907) - exp(0.7308) # backtransformed difference in fight duration = 6.080451
(exp(0.7308 + 1.1907) / exp(0.7308))-1 # % dif 2.289383
((exp(1.1907 + 0.2146) - exp(1.1907 - 0.2146))/2)/exp(0.7038)

exp(0.7308 + 0.78834767) - exp(0.7308) # lower CI for difference in fight duration = 2.491588
exp(0.7308 + 0.78834767) / exp(0.7308) # lower CI for x-fold diff in fight duration = 2.199759

exp(0.7308 + 1.6300698) - exp(0.7308) # upper CI for difference in fight duration = 8.523426
exp(0.7308 + 1.6300698) / exp(0.7308) # upper CI for x-fold diff in fight duration = 5.104231


## broad-elevation species have longer fights in low-elevations than high-elevation species do


######## elevation projection based on field observations. How will fighting ability be impeded by upslope migration over the next 50 years? assume 10 m upslope per year based on mean climate velocity?
elev <- c(2000, 2500)
temp <- rep(mean(fights$temp), 2)
el.proj.frame <- data.frame(elev, temp)
ln.dur <- predict(mod01, newdata = el.proj.frame, re.form = NA)
el.proj.frame$dur <- exp(ln.dur) # 5.600517 2.709580 

1-(2.709580/5.600517) # moving upslope by 500 m over the next 50 years will incur a 51.62% reduction in fighting ability

# temperature projection. How will fighting ability be impeded if dragonflies stay where they are and experience a 2.5C increase over next 50 years? assume 2.5 increase based on ballpark of 0.05c per year from Chan et al 2024
elev <- rep(2000, 2)
temp <- c(28.3, 30.8)
t.proj.frame <- data.frame(elev, temp)
ln.dur <- predict(mod01, newdata = t.proj.frame, re.form = NA)
t.proj.frame$dur <- exp(ln.dur) # 4.753225 4.260556 

1-(4.260556/4.753225) # staying where they are and experiencing a 2.5C increase over the next 50 years will incur only a 10.35% reduction in fighting ability

###

#

locat <- c(rep('low',2), rep('mid',2), rep('high',2))
elev <- c(rep(1600,2), rep(2300,2), rep(3000,2))
temp <- c(rep(33,2), rep(26.78,2), rep(17.33,2))
cur.tab <- data.frame(locat, elev, temp)
cur.tab$ln.dur <- predict(mod01, newdata = cur.tab, re.form = NA)
cur.tab$duration <- exp(cur.tab$ln.dur)

locat <- c(rep('low',2), rep('mid',2), rep('high', 2))
scen <- rep(c('stay', 'move'), 3)
elev <- c(1600,2100,2300,2800,3000,3500)
temp <- c(35.5,33,29.28,26.78,19.83,17.33)
proj.tab <- data.frame(locat, scen, elev, temp)
ln.dur <- predict(mod01, newdata = proj.tab, re.form = NA)
proj.tab$future.duration <- exp(ln.dur)
proj.tab$current.duration <- cur.tab$duration
proj.tab$dur.decline <- (proj.tab$future.duration/proj.tab$current.duration) - 1

#prediction intervals
myFunc2 <- function(mm) {
	 predict(mm, newdata = proj.tab, re.form = NA)
}

proj.boot <- bootMer(mod01, FUN = myFunc2, use.u = FALSE, type = 'parametric', nsim = 500)
pred.proj.boot <- t(apply(proj.boot$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))
proj.tab$upi <- (exp(pred.proj.boot[,1])/proj.tab$current.duration) - 1
proj.tab$lpi <- (exp(pred.proj.boot[,2])/proj.tab$current.duration) - 1


proj.tab$locat <- factor(proj.tab$locat, levels = c('low', 'mid', 'high'))
proj.tab$scen <- factor(proj.tab$scen, levels = c('stay', 'move'))


  # locat scen elev  temp future.duration current.duration dur.decline        upi        lpi
# 1   low stay 1600 35.50        7.096610         7.728353 -0.08174354 -0.3373054  0.2690401
# 2   low move 2100 33.00        3.337582         7.728353 -0.56813796 -0.6835955 -0.3943084
# 3   mid stay 2300 29.28        2.898013         3.216360 -0.09897759 -0.3213259  0.2194638
# 4   mid move 2800 26.78        1.752194         3.216360 -0.45522473 -0.5971105 -0.2670834
# 5  high stay 3000 19.83        2.011167         2.353924 -0.14561099 -0.3508664  0.1165974
# 6  high move 3500 17.33        1.462450         2.353924 -0.37871831 -0.5443803 -0.1676012






#### are the general patterns the same if we use the average fight duration per species per site visit?

### function for making an easy to use summary table

#summarySE function
## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
    require(plyr)

    # New version of length which can handle NA's: if na.rm==T, don't count them
    length2 <- function (x, na.rm=FALSE) {
        if (na.rm) sum(!is.na(x))
        else       length(x)
    }

    # This is does the summary; it's not easy to understand...
    datac <- ddply(data, groupvars, .drop=.drop,
                   .fun= function(xx, col, na.rm) {
                           c( N    = length2(xx[,col], na.rm=na.rm),
                              mean = mean   (xx[,col], na.rm=na.rm),
                              sd   = sd     (xx[,col], na.rm=na.rm)
                              )
                          },
                    measurevar,
                    na.rm
             )

    # Rename the "mean" column    
    datac <- rename(datac, c("mean"=measurevar))

    datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean

    # Confidence interval multiplier for standard error
    # Calculate t-statistic for confidence interval: 
    # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
    ciMult <- qt(conf.interval/2 + .5, datac$N-1)
    datac$ci <- datac$se * ciMult

    return(datac)
}


## make dataset that has the average duration per species per site visit (also include the elev of the site and the temp of the visit)
sp.site.tab <- summarySE(data = fights, measurevar = 'duration', groupvar = c('spp', 'date', 'locat', 'elev', 'temp', 'genus', 'family'), na.rm = TRUE)

sp.site.dat <- as.data.frame(sp.site.tab)
head(sp.site.dat)

## weight by observation number during the site visit since there are very large differences in the number of observations per species per site visit in some cases
site.mod01 <- lmer(log(duration) ~ log(elev) + log(temp) + (1|spp) + (1|locat) + (1|date) + (1|genus) + (1|family), data = sp.site.dat, weights = N)
summary(site.mod01) # looks almost identical to full model where each fight is treated as an independent obs

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: log(duration) ~ log(elev) + log(temp) + (1 | spp) + (1 | locat) +      (1 | date) + (1 | genus) + (1 | family)
   # Data: sp.site.dat
# Weights: N

# REML criterion at convergence: 61.3

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -2.0147 -0.6246  0.1071  0.5772  1.6526 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # date     (Intercept) 0.038547 0.19633 
 # spp      (Intercept) 0.003810 0.06172 
 # locat    (Intercept) 0.005742 0.07578 
 # genus    (Intercept) 0.081355 0.28523 
 # family   (Intercept) 0.007565 0.08698 
 # Residual             0.515109 0.71771 
# Number of obs: 69, groups:  date, 18; spp, 17; locat, 16; genus, 11; family, 3

# Fixed effects:
            # Estimate Std. Error     df t value Pr(>|t|)    
# (Intercept)   28.506      2.938 25.441   9.704 4.91e-10 ***
# log(elev)     -3.061      0.271 16.532 -11.297 3.43e-09 ***
# log(temp)     -1.054      0.358 41.211  -2.943  0.00532 ** 

confint(site.mod01)

                  # 2.5 %     97.5 %
# .sig01       0.08591291  0.3216702
# .sig02       0.00000000  0.3059015
# .sig03       0.00000000  0.2238436
# .sig04       0.00000000  0.5262116
# .sig05       0.00000000  0.4924460
# .sigma       0.56772253  0.9204517
# (Intercept) 28.51240440 28.6286971
# log(elev)   -3.06256575 -3.0517276
# log(temp)   -1.05821838 -1.0405450

