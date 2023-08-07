
pacman::p_load(data.table, tidyverse, haven, labelled, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm, gt, lme4, car, lmerTest, 
               ggeffects, magrittr, broom, broom.mixed,
               backports, effects, interactions, plyr)


## Multi-level modeling

## Gelman and Hill (p. 259):

## Varying intercept model with no predictors:
m00 <- lmer(Endorse_BCL ~ 1 + (1| CountryID), 
            data = ds)

m00

## Varying intercept model with an individual-level predictor:
m01 <- lmer(Endorse_BCL ~ IG_Fusion + (1| CountryID), 
            data = ds)

m01

## From PennState QuantDev tutorial:
## https://quantdev.ssri.psu.edu/tutorials/r-bootcamp-introduction-multilevel-model-and-interactions


# Histogram
ggplot(data = ds, 
       aes(x = Endorse_BCL)) +
  geom_histogram(color = "black",
                 bins = 20) +
  labs(x = "Endorse_BCL (high = more stressed)", 
       y = "Frequency")+
  theme_bw()

# Faceted plot
ggplot(data = ds, 
       aes(x = IG_Fusion,
           y = Endorse_BCL)) +
  geom_point() +
  stat_smooth(method="lm", 
              fullrange=TRUE) +
  labs(x = "IG_Fusion",
       y = "Endorse_BCL")+
  facet_wrap( ~ Country) +
  theme_bw()

# Unconditional means model
m00<- lmer(formula = Endorse_BCL ~ 1 + (1 | Country), 
           data = ds,
           na.action = na.exclude)

summary(m00)
VarCorr(m00)


m01<- lmer(formula = Endorse_BCL ~ IG_Fusion + (1 | Country), 
           data = ds,
           na.action = na.exclude)

summary(m01)
VarCorr(m01)
















