library(car)
library(lme4)
library(ggplot2)
library(MASS)
library(lmerTest)
library(visreg)
library(emmeans)

########## look at differences in tracheae
trach <- read.csv('elevation.tracheae.csv')

# re-order elevation and genr.restr factors so low-elev & restricted are the reference levels
trach$elevation <- factor(trach$elevation, levels = c('L', 'H'))
trach$gen.restr <- factor(trach$gen.restr, levels = c('restricted', 'generalist'))

## intraspecific comparison. First test if we need body mass in there
mod00 <- lmer(diam ~ elevation + body_mass + (1|species), data = subset(trach, gen.restr == 'generalist'))
confint(mod00, parm = 'body_mass') # no evidence we need body mass
                # 2.5 %     97.5 %
# body_mass -0.04243016 0.09129374


# direct intraspecific comparison of tracheole widths between high and low elevation pops of broad-elev species
mod01 <- lmer(diam ~ elevation + (1|species), data = subset(trach, gen.restr == 'generalist'))
summary(mod01)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: diam ~ elevation + (1 | species)
   # Data: subset(trach, gen.restr == "generalist")

# REML criterion at convergence: -85.5

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -2.58021 -0.41753  0.09637  0.53248  1.42576 

# Random effects:
 # Groups   Name        Variance  Std.Dev.
 # species  (Intercept) 6.193e-05 0.007869
 # Residual             3.716e-04 0.019276
# Number of obs: 20, groups:  species, 2

# Fixed effects:
             # Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)  0.563377   0.008508  2.086345  66.214 0.000168 ***
# elevationH   0.020750   0.008664 17.001309   2.395 0.028417 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Correlation of Fixed Effects:
           # (Intr)
# elevationH -0.560

confint(mod01)
                 # 2.5 %     97.5 %
# .sig01      0.000000000 0.02887242
# .sigma      0.013953897 0.02684484
# (Intercept) 0.545471422 0.58168569
# elevationH  0.003390814 0.03825547


# high elevation populations have wider tracheoles than low elevation populations of the same species

### interspecific comparisons. Need to account for family in these analyses because you have four species in two different families
low.trach <- subset(trach, subset = elevation == 'L')

# first test if we need to account for body mass
spp00 <- lmer(diam ~ gen.restr + body_mass + (1|species) + (1|family), data = low.trach)
confint(spp00, parm = 'body_mass') # no effect of body mass
                # 2.5 %     97.5 %
# body_mass -0.03120136 0.03765353

## compare tracheole widths between broad- and restricted-elevation species
spp01 <- lmer(diam ~ gen.restr + (1|species) + (1|family), data = low.trach)
summary(spp01)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: diam ~ gen.restr + (1 | species) + (1 | family)
   # Data: low.trach

# REML criterion at convergence: -85

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -1.3944 -0.9727  0.2174  0.6522  1.3569 

# Random effects:
 # Groups   Name        Variance  Std.Dev. 
 # species  (Intercept) 1.234e-12 1.111e-06
 # family   (Intercept) 0.000e+00 0.000e+00
 # Residual             2.197e-04 1.482e-02
# Number of obs: 18, groups:  species, 4; family, 2

# Fixed effects:
                     # Estimate Std. Error        df t value Pr(>|t|)    
# (Intercept)          0.559667   0.004941 15.999925 113.280   <2e-16 ***
# gen.restrgeneralist  0.003222   0.006987 15.999925   0.461    0.651    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


confint(spp01)
                          # 2.5 %     97.5 %
# .sig01               0.00000000 0.01122819
# .sig02               0.00000000 0.01235865
# .sigma               0.01040781 0.02015889
# (Intercept)          0.54998074 0.56938553
# gen.restrgeneralist -0.01040931 0.01685375

###############

### compare patterns of mitochondria

mito.vals <- read.csv('elevation.mito.csv')
mito.vals$elevation <- factor(mito.vals$elevation, levels = c('L', 'H'))
mito.vals$gen.restr <- factor(mito.vals$gen.restr, levels = c('restricted', 'generalist'))


### do we need to account for body mass
mito.intra00 <- lmer(mito ~ elevation + body_mass + (1|species), data = subset(mito.vals, gen.restr == 'generalist')) 
confint(mito.intra00) # no

                 # 2.5 %    97.5 %
# .sig01           0.000 13244.737
# .sigma        4508.242  8446.180
# (Intercept) -12536.078 20014.858
# elevationH   -3320.296  7829.698
# body_mass   -12900.777 54444.088

## compare mitochondrial counts between high vs low elevations of broad-elevation species
mito.intra <- lmer(mito ~ elevation  + (1|species), data = subset(mito.vals, gen.restr == 'generalist'))
summary(mito.intra) 

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mito ~ elevation + (1 | species)
   # Data: subset(mito.vals, gen.restr == "generalist")

# REML criterion at convergence: 370.9

# Scaled residuals: 
    # Min      1Q  Median      3Q     Max 
# -1.6511 -0.8984  0.0752  0.5361  2.0771 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # species  (Intercept)        0    0    
 # Residual             40414562 6357    
# Number of obs: 20, groups:  species, 2

# Fixed effects:
            # Estimate Std. Error    df t value Pr(>|t|)    
# (Intercept)    13577       2119    18   6.407 4.95e-06 ***
# elevationH      2458       2857    18   0.860    0.401    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


confint(mito.intra)
                # 2.5 %    97.5 %
# .sig01          0.000  4877.440
# .sigma       4553.909  8519.976
# (Intercept)  9446.574 17688.662
# elevationH  -3120.979  8036.193

### no difference within species

### between species? first see if we need to account for body_mass
mito.inter00 <- lmer(mito ~ gen.restr + body_mass + (1|species) + (1|family), data = mito.vals)
confint(mito.inter00, parm = 'body_mass') # nope
              # 2.5 %   97.5 %
# body_mass -11257.89 15687.27

# compare mitochondrial counts between broad and restricted species
mito.inter01 <- lmer(mito ~ gen.restr + (1|species) + (1|family), data = mito.vals)
summary(mito.inter01)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
# Formula: mito.mean ~ gen.restr + (1 | species) + (1 | family)
   # Data: mito.vals

# REML criterion at convergence: 549.9

# Scaled residuals: 
     # Min       1Q   Median       3Q      Max 
# -1.61508 -0.77074  0.05408  0.48349  2.46134 

# Random effects:
 # Groups   Name        Variance Std.Dev.
 # species  (Intercept)        0    0    
 # family   (Intercept)        0    0    
 # Residual             33804754 5814    
# Number of obs: 29, groups:  species, 4; family, 2

# Fixed effects:
                    # Estimate Std. Error   df t value Pr(>|t|)    
# (Intercept)             9223       1938   27   4.759 5.82e-05 ***
# gen.restrgeneralist     5705       2334   27   2.445   0.0213 *  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

confint(mito.inter01)
                       # 2.5 %    97.5 %
# .sig01                 0.000  2897.902
# .sig02                 0.000  3944.427
# .sigma              4426.133  7433.762
# (Intercept)         5436.895 13004.572
# gen.restrgeneralist 1142.184 10268.411

## broad-elevation males have more mitochondria