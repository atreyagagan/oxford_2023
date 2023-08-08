
library(sjPlot)
library(merTools)

## Random intercept model:

## Allowing intercepts to randomly vary across countries:
m00<- lmer(Endorse_BCL ~ 1 + (1 | Country), 
           data = ds)

summary(m00)
confint(m00)

## ICC:
ICC(outcome = "Endorse_BCL", group = "Country", data = ds)

## Tab model from sjPlot:
tab_model(m00)


## Additional predictors at level 1:

m01 <- lmer(Endorse_BCL~IG_Fusion+IG_Identification+Empathic_concern+
            Age+Female+Married+Wealth_level+ 
              (1 | Country), 
            data = ds)

summary(m01)
tab_model(m01)

confint(m01)
