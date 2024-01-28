## ----error = F, warning=F, message=F--------------------------------------------

rm(list=ls())

## Install "pacman" package if not installed
# (remove the # symbol from the line below):
# install.packages("pacman")

## Load R packages:
pacman::p_load(data.table, tidyverse, haven, labelled, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm)

## Import dataset:
ds <- read_sav("/home/gagan/Desktop/oxford/data/pakistan/Pakistan Data.sav")
# ds <- fread("/home/gagan/Desktop/oxford/data/pakistan/Pakistan_Source_updated.csv")

options(digits = 2)



## ----error = F, message = F, warning = F----------------------------------------

ds$Age1 <- as.numeric(gsub("\\D", "", ds$Age))

ds$age <- as.numeric(ifelse(ds$Age1 == "6465", 64.5,
                     ifelse(ds$Age1 == "806", 80.5, 
                     ifelse(ds$Age1 < 18, NA, ds$Age1))))
summary(ds$age)

ds %>% drop_na(age)%>%
ggplot(aes(x = age))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 34.00", 
                 xintercept = 34.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Age", 
       y = "Frequency", 
       title = "Age distribution: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$gender <- ifelse(ds$Gender == 1, "Male",
             ifelse(ds$Gender == 2, "Female", NA))

lp02 <- ds %>% drop_na(gender) %>%
lollipop_chart(x = gender,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Gender distribution: Pakistan")+
  theme_bw()

lp02

bp01 <- ds %>% drop_na(gender) %>% 
  ggplot(aes(y = age, 
             x = gender))+
geom_boxplot(fill = "grey")+
  labs(y = "Age",
       x = "",
       title = "Age distribution by gender: Pakistan")+
  coord_flip()+
  theme_bw()

bp01



## ----error = F, message = F, warning = F----------------------------------------

table(ds$Province)
ds$province <- haven::as_factor(ds$Province)
table(ds$province)

lp03 <- ds %>% drop_na(province) %>%
lollipop_chart(x = province,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Province distribution: Pakistan")+
  theme_bw()

lp03



## ----error = F, message = F, warning = F----------------------------------------

ds$hh_wealth <- haven::as_factor(ds$Household_level_of_wealth)

ds$hh_wealth <- factor(ds$hh_wealth, 
                       levels=c("Much lower", "Slightly lower", "Average",
                                "Slightly higher", "Much higher"))

lp04 <- ds %>% drop_na(hh_wealth) %>%
lollipop_chart(x = hh_wealth,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Household wealth distribution: Pakistan")+
  theme_bw()

lp04



## ----error = F, warning=F, message=F--------------------------------------------

ds$ses1 <- haven::as_factor(ds$Socioeconomic_status)

ds$ses <- ifelse(ds$ses1 == "Lower middle", "Lower middle", 
          ifelse(ds$ses1 == "Middle", "Middle", 
          ifelse(ds$ses1 == "Uper middle", "Upper middle",
          ifelse(ds$ses1 == "Uper", "Upper", NA))))

ds$ses <- factor(ds$ses, 
                 levels=c("Lower middle", "Middle", "Upper middle", "Upper"))

lp05 <- ds %>% drop_na(ses) %>%
lollipop_chart(x = ses,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Socioeconomic status: Pakistan")+
  theme_bw()

lp05



## ----error = F, message = F, warning = F----------------------------------------

ds$income <- as.numeric(ds$Annual_income)

summary(ds$income)

## Remove outliers (greater than 10 million):
# ds <- ds[ds$income < 10000000, ]

# ds$income <- ifelse(ds$income < 10000000, ds$income, NA)

summary(ds$income)

ds %>% drop_na(income)%>%
ggplot(aes(x = income))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 80)+
  geom_textvline(label = "Mean = 1.2 M", 
                 xintercept = 1223422, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  scale_x_continuous(labels = scales::unit_format(unit = "M", 
                                                  scale = 1e-6))+
  labs(x = "Annual income", 
       y = "Frequency", 
       title = "Annual income distribution: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$fses <- as.numeric(ds$ses)

ds$fhhw <- as.numeric(ds$hh_wealth)

ds3 <- cbind.data.frame(ds$fses, ds$fhhw, ds$income)
ds3 <- na.omit(ds3)

ds3$Annual_income <- ds3$`ds$income`
ds3$Household_wealth <- ds3$`ds$fhhw`
ds3$SES <- ds3$`ds$fses`

mtx <- cor(ds3[, c(4:6)])

#corrplot(mtx, method = "number", number.cex = 1.0,
#         col=c("white", "darkred", "red",
#               "darkgrey", "blue", "darkblue"))




## ----error = F, message = F, warning = F----------------------------------------

ds$jobnature <- haven::as_factor(ds$Job_Nature)

lp06 <- ds %>% drop_na(jobnature) %>%
lollipop_chart(x = jobnature,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Nature of employment: Pakistan")+
  theme_bw()

lp06



## ----error = F, message = F, warning = F----------------------------------------

table(haven::as_factor(ds$Religious_Affiliation))

## Change label for protestant (it has typo):
val_label(ds$Religious_Affiliation, 2) <- "Christian (Protestant)"
ds$religion <- haven::as_factor(ds$Religious_Affiliation)

lp07 <- ds %>% drop_na(religion) %>%
lollipop_chart(x = religion,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Religious affiliation: Pakistan")+
  theme_bw()

lp07



## ----error = F, message = F, warning = F----------------------------------------

ds$married <- ifelse(ds$Marital_status == 1, "Unmarried",
              ifelse(ds$Marital_status == 2, "Married", "Other"))

table(ds$married)

bp01 <- ds %>% drop_na(married) %>%
  ggplot(aes(x = fct_rev(fct_infreq(married))))+
  geom_bar(fill = "black")+
  labs(x = "",
       y = "Frequency",
       title = "Marital status")+
  coord_flip()+
  theme_bw()

# bp01

lp08 <- ds %>% drop_na(married) %>%
lollipop_chart(x = married,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Marital status: Pakistan")+
  theme_bw()

lp08



## ----error = F, message = F, warning = F----------------------------------------

ds$children <- as.numeric(gsub("\\D", "", ds$Number_of_Children))

summary(ds$children)

ds %>% drop_na(children)%>%
ggplot(aes(x = children))+
  geom_bar(color = "black",
                 fill = "gray",
                 width = 0.75)+
  geom_textvline(label = "Mean = 2.00", 
                 xintercept = 2.00, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Number of children", 
       y = "Frequency", 
       title = "Number of children: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

eth <- as.data.frame(table(ds$Ethnicity))
eth$Var1 <- as.character(eth$Var1)
eth$ethnicity <- ifelse(eth$Freq < 5, "Other", eth$Var1)
l1 <- as.list(eth$ethnicity)

ds2 <- ds[, c("Ethnicity")]

ds2$ethnicity <- ifelse(ds2$Ethnicity %in% l1, ds2$Ethnicity, "Other")
ds2$ethnicity <- ifelse(ds2$ethnicity == "Pujnabi", "Punjabi", 
                 ifelse(ds2$ethnicity == "Christain", "Chrisitian", 
                 ifelse(ds2$ethnicity == "Urduspeaking", "Urdu speaking",
                        ds2$ethnicity)))

lp08 <- ds2 %>% drop_na(ethnicity) %>%
lollipop_chart(x = ethnicity,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Ethnic distribution: Pakistan")+
  theme_bw()

lp08




## ----error = F, message = F, warning = F----------------------------------------

ds$education <- as.character(haven::as_factor(ds$Level_of_education))

ds$education <- ifelse(ds$education=="Illetriate", "Illiterate",
                ifelse(ds$education=="Matric", "Matriculate",
                       ds$education))

ds$education <- factor(ds$education, 
                       levels = c("Illiterate", "Middle", "Matriculate", 
                                  "Intermediate", "Bachelor", "Master",
                                  "MPhil", "PhD"))

table(ds$education)

lp08 <- ds %>% drop_na(education) %>%
lollipop_chart(x = education,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Education distribution: Pakistan")+
  theme_bw()

lp08



## ----error = F, message = F, warning = F----------------------------------------

## Four ingroup fusion items:

# I have a deep emotional bond with the [ingroup].
ds$IGF01 <- as.numeric(ds$Q11_1)

# I am strong because of the [ingroup].
ds$IGF02 <- as.numeric(ds$Q11_2)

# I make the [ingroup] strong.	
ds$IGF03 <- as.numeric(ds$Q11_3)

# I am one with the [ingroup]
## TWO IDENTICAL ITEMS!!! 
ds$IGF04a <- as.numeric(ds$Q11_4)
ds$IGF04b <- as.numeric(ds$Q11_5)

table(ds$IGF04a)
table(ds$IGF04b)

corr.test(ds$IGF04a, ds$IGF04b)

## Four outgroup fusion items:

# I have a deep emotional bond with the [outgroup].
# DOES NOT EXIST!!!

## Assuming Q11_5 is first outgroup fusion item 
## (instead of duplicate of 4th ingroup fusion item):
ds$IGF04 <- as.numeric(ds$Q11_4)
ds$OGF01 <- as.numeric(ds$Q11_5)

# I am strong because of the [outgroup].
ds$OGF02 <- as.numeric(ds$Q11_6)

# I make the [outgroup] strong.	
ds$OGF03 <- as.numeric(ds$Q11_7)

# I am one with the [outgroup].
ds$OGF04 <- as.numeric(ds$Q11_8)


## Four ingroup identification items:

# I identify with the [ingroup].
ds$IGI01 <- as.numeric(ds$Q11_9)

# I have a lot in common with the [ingroup].
ds$IGI02 <- as.numeric(ds$Q11_10)

# I connect with the values of the [ingroup].
ds$IGI03 <- as.numeric(ds$Q11_11)

# I feel a sense of belonging with the [ingroup].
ds$IGI04 <- as.numeric(ds$Q11_12)


## Four outgroup identification items:

# I identify with the [outgroup].	
ds$OGI01 <- as.numeric(ds$Q11_13)

# I have a lot in common with the [outgroup].	
ds$OGI02 <- as.numeric(ds$Q11_14)

# I connect with the values of the [outgroup].	
ds$OGI03 <- as.numeric(ds$Q11_15)

# I feel a sense of belonging with the [outgroup].	
ds$OGI04 <- as.numeric(ds$Q11_16)




## ----error=F, message=F, warning=F----------------------------------------------

## Bonds dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03, ds$IGF04,
                          ds$IGI01, ds$IGI02, ds$IGI03, ds$IGI04,
                          ds$OGF01, ds$OGF02, ds$OGF03, ds$OGF04,
                          ds$OGI01, ds$OGI02, ds$OGI03, ds$OGI04)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:16)])



## ----error = F, message = F, warning = F----------------------------------------
corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----warning=F, message=F-------------------------------------------------------

KMO(r=cor(bonds))



## ----warning=F, message=F-------------------------------------------------------

cortest.bartlett(bonds)



## ----warning=F, message=F-------------------------------------------------------
# parallel <- fa.parallel(bonds)



## ----warning=F, message=F-------------------------------------------------------
fit01 <- factanal(bonds, 2, rotation="promax")
fit01



## ----warning=F, message=F-------------------------------------------------------

fit02 <- factanal(bonds, 4, rotation="promax")
fit02

## Remove 4th item from all subscales and retry:

## New dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03,
                              ds$IGI01, ds$IGI02, ds$IGI03,
                              ds$OGF01, ds$OGF02, ds$OGF03,
                              ds$OGI01, ds$OGI02, ds$OGI03)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:12)])

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Parallel test
# parallel <- fa.parallel(bonds)

## factor loadings: 4 factor model:
fit04 <- factanal(bonds, 4, rotation="promax")
print(fit04$loadings)



## ----error = F, message = F, warning = F----------------------------------------

## factor loadings: 3 factor model:
fit05 <- factanal(bonds, 3, rotation="promax")
print(fit05$loadings)



## ----error = F, message = F, warning=F------------------------------------------

igfusion <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03)

names(igfusion) <- sub('ds\\$', '', names(igfusion))

igfusion <- na.omit(igfusion)
mtx2 <- cor(igfusion[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alpha02 <- psych::alpha(igfusion)
alpha02



## ----warning=F, message=F-------------------------------------------------------

igidentification <- cbind.data.frame(ds$IGI01, ds$IGI02, ds$IGI03)

names(igidentification) <- sub('ds\\$', '', names(igidentification))

igidentification <- na.omit(igidentification)
mtx3 <- cor(igidentification[, c(1:3)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha03 <- psych::alpha(igidentification)
alpha03



## ----warning=F, message=F-------------------------------------------------------

ogbonds <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03,
                            ds$OGI01, ds$OGI02, ds$OGI03)

names(ogbonds) <- sub('ds\\$', '', names(ogbonds))

ogbonds <- na.omit(ogbonds)
mtx3 <- cor(ogbonds[, c(1:6)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha04 <- psych::alpha(ogbonds)
alpha04



## ----error = F, message = F, warning=F------------------------------------------

## BCL and BBL items:
# BCL_01:
# Seek out opportunities to bridge social divisions with their opponents, enemies, opposition groups, or other outgroups.   
# Variables: Q6.1, Not found

# BCL_02:
# Demonstrate willingness to compromise with their opponents, enemies, opposition groups, or other outgroups. 
# Variables: Q6.2, Q7.1

# BCL_03:
# Try to understand and empathize with their opponents, enemies, opposition groups, or other outgroups.  
# Variables: Q6.3, Q7.2

# BBL_01:
# Represent the interests of the communities and groups that they belong to even at the cost of other groups.
# Variables: Q6.4, Q7.3

# BBL_02:
# Focus on building stronger connections/relationships within the communities and groups they belong to rather than building stronger relationships with other groups across boundaries.
# Variables: Q6.5, (Q7.4 and 7.5)

# BBL_03:
# Try to gain benefits for the communities and groups they belong to even at the expense of other groups.
# Variables: Q6.6, Q7.6

### PROBLEM ###

## there is no item for EXP_BCL_01 (experience of BCL, item 01): 
# (Seek out opportunities to bridge social...)

## BUt, EXP_BBL_02 (endorsement of BBL, item 2) has two nearly identically worded items: Q7_4 and Q7_5:
# (Focus on building stronger relationships /connections.... )

## Going over this with research partners in Pakistan confirmed this. 
## So this scale for Pakistan will have a missing item (EXP_BCL_01)
## while one item (EXP_BBL_02) was asked twice, and only one will be retained

ds$ENDBCL01 <- as.numeric(ds$Q6_1)
ds$ENDBCL02 <- as.numeric(ds$Q6_2)
ds$ENDBCL03 <- as.numeric(ds$Q6_3)
ds$ENDBBL01 <- as.numeric(ds$Q6_4)
ds$ENDBBL02 <- as.numeric(ds$Q6_5)
ds$ENDBBL03 <- as.numeric(ds$Q6_6)

ds$EXPBCL01 <- NA
ds$EXPBCL02 <- as.numeric(ds$Q7_1)
ds$EXPBCL03 <- as.numeric(ds$Q7_2)
ds$EXPBBL01 <- as.numeric(ds$Q7_3)
## For EXP_BBL_02, 
# Q7.4: Focus on building stronger relationships...
# Q7.5: Focus on building stronger connections...
## Q7.4 was dropped and Q7.5 was used:
ds$EXPBBL02 <- as.numeric(ds$Q7_5)
ds$EXPBBL03 <- as.numeric(ds$Q7_6)

leadership <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03, 
                               ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03,
                               ds$EXPBCL02, ds$EXPBCL03,
                               ds$EXPBBL01, ds$EXPBBL02, ds$EXPBBL03)

names(leadership) <- sub('ds\\$', '', names(leadership))

leadership <- na.omit(leadership)
mtx1 <- cor(leadership[, c(1:11)])



## ----error = F, message = F, warning = F----------------------------------------

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))


## ----error = F, message = F, warning = F----------------------------------------

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(leadership))

## Bartlett's test of sphericity
cortest.bartlett(leadership)



## ----error = F, message = F, warning = F----------------------------------------

# parallel <- fa.parallel(leadership)



## ----error = F, warning = F, message = F----------------------------------------
fit02 <- factanal(leadership, 4, rotation="promax")
fit02



## ----error = F, message = F, warning=F------------------------------------------

end_bcl <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03)
end_bbl <- cbind.data.frame(ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03)
exp_bcl <- cbind.data.frame(ds$EXPBCL02, ds$EXPBCL03)
exp_bbl <- cbind.data.frame(ds$EXPBBL01, ds$EXPBBL02, ds$EXPBBL03)

names(end_bcl) <- sub('ds\\$', '', names(end_bcl))
names(end_bbl) <- sub('ds\\$', '', names(end_bbl))
names(exp_bcl) <- sub('ds\\$', '', names(exp_bcl))
names(exp_bbl) <- sub('ds\\$', '', names(exp_bbl))

end_bcl <- na.omit(end_bcl)
end_bbl <- na.omit(end_bbl)
exp_bcl <- na.omit(exp_bcl)
exp_bbl <- na.omit(exp_bbl)

mtx1 <- cor(end_bcl[, c(1:3)])
mtx2 <- cor(end_bbl[, c(1:3)])
mtx3 <- cor(exp_bcl[, c(1:2)])
mtx4 <- cor(exp_bbl[, c(1:3)])



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph01 <- psych::alpha(end_bcl)
alph01



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph01 <- psych::alpha(end_bbl)
alph01



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph01 <- psych::alpha(exp_bcl)
alph01



## ----error = F, message = F, warning=F------------------------------------------
## Reliability:
alph01 <- psych::alpha(exp_bbl)
alph01



## ----error = F, message = F, warning = F----------------------------------------

## Proper (usable) variables in the model:

ds$Age1 <- as.numeric(gsub("\\D", "", ds$Age))
ds$Age <- as.numeric(ifelse(ds$Age1 == "6465", 64.5,
                     ifelse(ds$Age1 == "806", 80.5, 
                     ifelse(ds$Age1 < 18, NA, ds$Age1))))

ds$Female <- ifelse(ds$gender == "Female", 1, 0)
ds$Married <- ifelse(ds$married == "Married", 1, 0)
ds$`SES-` <- ds$ses

ds$Endorse_BCL <- as.numeric((ds$ENDBCL01+ds$ENDBCL02+ds$ENDBCL03)/3)
ds$Endorse_BBL <- as.numeric((ds$ENDBBL01+ds$ENDBBL02+ds$ENDBBL03)/3)
ds$Experience_BCL <- as.numeric((ds$EXPBCL02+ds$EXPBCL03)/2)
ds$Experience_BBL <- as.numeric((ds$EXPBBL01+ds$EXPBBL02+ds$EXPBBL03)/3)

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03)/3)
ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03)/3)
ds$OG_Bonds <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03+
                             ds$OGI01+ds$OGI02+ds$OGI03)/6)



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

#lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+`SES-`,
#           data = ds)

#lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+`SES-`, 
#           data = ds)

#lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+`SES-`, 
#           data = ds)

#lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Bonds+Age+Female+Married+`SES-`, 
#           data = ds)


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03, lm04,
##           type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Empathic concern
ds$empathic_concern_01 <- as.numeric(ds$Q18_1)
ds$empathic_concern_01 <- ifelse(ds$empathic_concern_01 == 8, NA, ds$empathic_concern_01)

ds$empathic_concern_02a <- as.numeric(ds$Q18_2)
ds$empathic_concern_02 <- as.numeric(8 - ds$empathic_concern_02a)

ds$empathic_concern_03 <- as.numeric(ds$Q18_3)

ds$empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03)/3

summary(ds$empathic_concern)

ds %>% drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.70", 
                 xintercept = 5.70, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$perspective_taking_01 <- as.numeric(ds$Q18_4)
ds$perspective_taking_02 <- as.numeric(ds$Q18_5)
ds$perspective_taking_03 <- as.numeric(ds$Q18_6)
ds$perspective_taking_04 <- as.numeric(ds$Q18_7)
ds$perspective_taking_04 <- ifelse(ds$perspective_taking_04 == 37, NA,
                                   ds$perspective_taking_04)

ds$perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                          ds$perspective_taking_03+ds$perspective_taking_04)/4

summary(ds$perspective_taking)

ds %>% drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.30", 
                 xintercept = 5.30, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$og_coop_01 <- as.numeric(ds$Q12_1)
ds$og_coop_02 <- as.numeric(ds$Q12_2)

ds$og_cooperation <- (ds$og_coop_01+ds$og_coop_02)/2

summary(ds$og_cooperation)

ds %>% drop_na(og_cooperation)%>%
ggplot(aes(x = og_cooperation))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.50", 
                 xintercept = 5.50, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup cooperation score", 
       y = "Frequency", 
       title = "Outgroup cooperation")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$history_discrimination <- as.numeric(ds$Q12_3)

summary(ds$history_discrimination)

ds %>% drop_na(history_discrimination)%>%
ggplot(aes(x = history_discrimination))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 20)+
  geom_textvline(label = "Mean = 4.40", 
                 xintercept = 4.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perceived history of discrimination score", 
       y = "Frequency", 
       title = "Perceived history of discrimination")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$og_host_01 <- as.numeric(ds$Q12_4)
ds$og_host_02 <- as.numeric(ds$Q12_5)

ds$og_hostility <- (ds$og_host_01+ds$og_host_02)/2

summary(ds$og_hostility)

ds %>% drop_na(og_hostility)%>%
ggplot(aes(x = og_hostility))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 3.60", 
                 xintercept = 3.60, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup hostility score", 
       y = "Frequency", 
       title = "Outgroup hostility")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$fight_outgroup <- as.numeric(ds$Q12_6)

summary(ds$fight_outgroup)

ds %>% drop_na(fight_outgroup)%>%
ggplot(aes(x = fight_outgroup))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 2.20", 
                 xintercept = 2.20, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Willingness to fight outgroup score", 
       y = "Frequency", 
       title = "Willingness to fight outgroup")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds3 <- cbind.data.frame(ds$empathic_concern, ds$perspective_taking,
                        ds$og_cooperation, ds$history_discrimination,
                        ds$og_hostility, ds$fight_outgroup)
ds3 <- na.omit(ds3)

names(ds3) <- sub('ds\\$', '', names(ds3))

mtx <- cor(ds3[, c(1:6)])

corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F, eval = F------------------------------
## 
## ## Four regression models predicting endorsement and experience of BCL / BBL:
## 
## lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03, lm04,
##           type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F, eval = F------------------------------
## 
## lm01 <- lm(og_cooperation~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm02 <- lm(og_hostility~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm03 <- lm(fight_outgroup~IG_Fusion+IG_Identification+OG_Bonds+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03, type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_per_01 <- as.numeric(ds$Q16_1)
ds$religious_freedom_per_02a <- as.numeric(ds$Q16_2)
ds$religious_freedom_per_02 <- (8 - ds$religious_freedom_per_02a)
ds$religious_freedom_per_04 <- as.numeric(ds$Q16_3)
ds$religious_freedom_per_03 <- as.numeric(ds$Q16_4)
ds$religious_freedom_per_05 <- as.numeric(ds$Q16_5)
ds$religious_freedom_per_06 <- as.numeric(ds$Q16_6)
ds$religious_freedom_per_07 <- as.numeric(ds$Q16_7)
ds$religious_freedom_per_08 <- as.numeric(ds$Q16_8)

ds$sprf <- (ds$religious_freedom_per_01+ds$religious_freedom_per_02+
            ds$religious_freedom_per_03+ds$religious_freedom_per_04+
            ds$religious_freedom_per_05+ds$religious_freedom_per_06+
            ds$religious_freedom_per_07+ds$religious_freedom_per_08)/8

summary(ds$sprf)

ds %>% drop_na(sprf)%>%
ggplot(aes(x = sprf))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.20", 
                 xintercept = 5.20, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "SPRF score", 
       y = "Frequency", 
       title = "Social perception of religious freedom: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_exp_01a <- as.numeric(ds$Q17_1)
ds$religious_freedom_exp_01 <- (8 - ds$religious_freedom_exp_01a)

ds$religious_freedom_exp_02a <- as.numeric(ds$Q17_2)
ds$religious_freedom_exp_02 <- (8 - ds$religious_freedom_exp_02a)

ds$religious_freedom_exp_03a <- as.numeric(ds$Q17_3)
ds$religious_freedom_exp_03 <- (8 - ds$religious_freedom_exp_03a)

ds$religious_freedom_exp_04a <- as.numeric(ds$Q17_4)
ds$religious_freedom_exp_04 <- (8 - ds$religious_freedom_exp_04a)
ds$religious_freedom_exp_04 <- ifelse(ds$religious_freedom_exp_04 > 0 , ds$religious_freedom_exp_04, NA)

ds$religious_freedom_exp_05a <- as.numeric(ds$Q17_5)
ds$religious_freedom_exp_05 <- (8 - ds$religious_freedom_exp_05a)

ds$religious_freedom_exp_06a <- as.numeric(ds$Q17_6)
ds$religious_freedom_exp_06 <- (8 - ds$religious_freedom_exp_06a)

ds$exp_religious_freedom <- (ds$religious_freedom_exp_01+ds$religious_freedom_exp_02+
                             ds$religious_freedom_exp_03+ds$religious_freedom_exp_04+
                              ds$religious_freedom_exp_05+ds$religious_freedom_exp_06)/6

summary(ds$exp_religious_freedom)

ds %>% drop_na(exp_religious_freedom)%>%
ggplot(aes(x = exp_religious_freedom))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.40", 
                 xintercept = 5.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Experience of religious freedom score", 
       y = "Frequency", 
       title = "Experience of religious freedom: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$Religion <- ds$religion
ds2 <- ds %>% 
drop_na(Religion, exp_religious_freedom)%>%
 group_by(Religion) %>% 
 summarise(Exp_religious_freedom=mean(exp_religious_freedom),.groups = 'drop') %>%
  as.data.frame()
ds2

ds %>% drop_na(religion, exp_religious_freedom)%>%
ggplot(aes(y = exp_religious_freedom, 
           x = religion))+
  geom_boxplot()+
  labs(x = "", 
       y = "Experience of religious freedom score", 
       title = "Experience of religious freedom: Pakistan")+
  coord_flip()+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$pc01 <- as.character(haven::as_factor(ds$Q15))

ds$pc01 <- factor(ds$pc01, 
                       levels = c("Never", "Very rarely", "Rarely",
                                  "Sometimes", "Often", "Very Often", "Always"))


table(ds$pc01)

# never, very rarely, rarely
# sometimes, often, very often, always

ds$pc02 <- as.numeric(ds$pc01)

summary(ds$pc02)

ds %>% drop_na(pc02)%>%
ggplot(aes(x = pc02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.30", 
                 xintercept = 5.30, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Positive contact score", 
       y = "Frequency", 
       title = "Positive contact with outgroup: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$nc01 <- as.character(haven::as_factor(ds$Q14))

ds$nc01 <- factor(ds$nc01, 
                       levels = c("Never", "Very rarely", "Rarely",
                                  "Sometimes", "Often", "Very Often", "Always"))


table(ds$nc01)

# never, very rarely, rarely
# sometimes, often, very often, always

ds$nc02 <- as.numeric(ds$nc01)

summary(ds$nc02)

ds %>% drop_na(pc02)%>%
ggplot(aes(x = pc02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 2.90", 
                 xintercept = 2.90, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Negative contact score", 
       y = "Frequency", 
       title = "Negative contact with outgroup: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Feel outgroup: negative to positive:

ds$ogaf1 <- as.character(haven::as_factor(ds$Q13_1))

## convert first character only to upper case:
ds$ogaf1 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf1, perl = TRUE)

ds$og_aff_01 <- factor(ds$ogaf1, 
                       levels=c("Very negative", "Moderately negative",
                                 "A little negative", "Neutral", 
                                 "A little positive", "Moderately positive", 
                                 "Very positive"))
table(ds$og_aff_01)

## Feel Outgroup: Hostile to friendly:

ds$ogaf2 <- as.character(haven::as_factor(ds$Q13_2))

## convert first character only to upper case:
ds$ogaf2 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf2, perl = TRUE)

ds$og_aff_02 <- factor(ds$ogaf2, 
                       levels = c("Very hostile", "Moderately hostile", 
                                  "A little hostile", "Neutral", 
                                  "A little friendly", "Moderately friendly",
                                  "Very friendly"))

table(ds$og_aff_02)

## Feel Outgroup: Suspicious to trusting:

ds$ogaf3 <- as.character(haven::as_factor(ds$Q13_3))

## convert first character only to upper case:
ds$ogaf3 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf3, perl = TRUE)

ds$og_aff_03 <- factor(ds$ogaf3, 
                       levels = c("Very suspicious", "Moderately suspicious", 
                                  "A little suspicious", "Neutral",
                                  "A little trusting", "Moderately trusting",
                                  "Very trusting"))
table(ds$og_aff_03)

## Feel Outgroup: Contempt to respect:

ds$ogaf4 <- as.character(haven::as_factor(ds$Q13_4))

## convert first character only to upper case:
ds$ogaf4 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf4, perl = TRUE)

ds$og_aff_04 <- factor(ds$ogaf4, 
                       levels = c("A lot of contempt", "Moderate contempt",
                                  "A little contempt", "Neutral", 
                                  "A little respect", "Moderate respect",
                                  "A lot of respect"))

table(ds$og_aff_04)

## Feel Outgroup: Concerned to Unconcerned:

ds$ogaf5 <- as.character(haven::as_factor(ds$Q13_5))

## convert first character only to upper case:
ds$ogaf5 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf5, perl = TRUE)

ds$og_aff_05 <- factor(ds$ogaf5, 
                       levels = c("Very concerned", "Moderately concerned", 
                                  "A little concerned", "Neutral",
                                  "A little unconcerned", "Moderately unconcerned",
                                  "Very unconcerned"))
table(ds$og_aff_05)

## Feel Outgroup: Threatened to Relaxed:

ds$ogaf6 <- as.character(haven::as_factor(ds$Q13_6))

## convert first character only to upper case:
ds$ogaf6 <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf6, perl = TRUE)

ds$og_aff_06 <- factor(ds$ogaf6, 
                       levels = c("Very threatened", "Moderately threatened", 
                                  "A little threatened", "Neutral", 
                                  "A little relaxed", "Moderately relaxed", 
                                  "Very relaxed"))
table(ds$og_aff_06)



## ----error = F, message = F, warning = F----------------------------------------

ds$NegativeToPositive <- as.numeric(ds$og_aff_01) 
ds$HostileToFriendly <- as.numeric(ds$og_aff_02) 
ds$SuspiciousToTrusting <- as.numeric(ds$og_aff_03) 
ds$ContemptToRespect <- as.numeric(ds$og_aff_04) 
ds$ConcernedToUnconcerned <- as.numeric(ds$og_aff_05) 
ds$ThreatenedToRelaxed <- as.numeric(ds$og_aff_06) 

og_affect <- cbind.data.frame(ds$NegativeToPositive, ds$HostileToFriendly,
                              ds$SuspiciousToTrusting, ds$ContemptToRespect,
                              ds$ConcernedToUnconcerned, ds$ThreatenedToRelaxed)

names(og_affect) <- sub('ds\\$', '', names(og_affect))

og_affect <- na.omit(og_affect)
mtx1 <- cor(og_affect[, c(1:6)])

#corrplot(mtx1, method = "number", number.cex = 0.7,
#         col=c("white", "darkred", "red",
#               "darkgrey", "blue", "darkblue"))

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(og_affect))

## Bartlett's test of sphericity
#cortest.bartlett(og_affect)

## Parallel test
# parallel <- fa.parallel(og_affect)



## ----error = F, message = F, warning = F, eval = F------------------------------
## fit01 <- factanal(og_affect, 1, rotation="promax")
## fit01
## 
## ## Reliability analysis
## 
## # psych::alpha(og_affect)


## ----error = F, message = F, warning = F----------------------------------------

og_affect <- cbind.data.frame(ds$NegativeToPositive, ds$HostileToFriendly,
                              ds$SuspiciousToTrusting, ds$ContemptToRespect,
                              ds$ThreatenedToRelaxed)

names(og_affect) <- sub('ds\\$', '', names(og_affect))

og_affect <- na.omit(og_affect)
mtx1 <- cor(og_affect[, c(1:5)])

#corrplot(mtx1, method = "number", number.cex = 0.7,
#         col=c("white", "darkred", "red",
#               "darkgrey", "blue", "darkblue"))

## Kaiser-Meyer-Olkin (KMO) test of factorability
#KMO(r=cor(og_affect))

## Bartlett's test of sphericity
#cortest.bartlett(og_affect)

## Parallel test
# parallel <- fa.parallel(og_affect)

## Reliability analysis:
# psych::alpha(og_affect)



## ----error = F, message = F, warning = F----------------------------------------

ds$og_affect <- (ds$NegativeToPositive+ds$HostileToFriendly+
                 ds$SuspiciousToTrusting+ds$ContemptToRespect+
                 ds$ThreatenedToRelaxed)/5

summary(ds$og_affect)

ds %>% drop_na(og_affect)%>%
ggplot(aes(x = og_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup affect score", 
       y = "Frequency", 
       title = "Outgroup affect: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$freq_positive_contact <- ds$pc02
ds$freq_negative_contact <- ds$nc02
ds$perspectiveXdiscrimination <- ds$perspective_taking*ds$history_discrimination



## ----error = F, message = F, warning = F, eval = F------------------------------
## 
## ## Four regression models predicting endorsement and experience of BCL / BBL:
## 
## lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03, lm04,
##           type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F, eval = F------------------------------
## 
## ## Four regression models:
## 
## lm01 <- lm(og_cooperation~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm02 <- lm(og_hostility~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm03 <- lm(fight_outgroup~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm04 <- lm(og_affect~IG_Fusion+IG_Identification+OG_Bonds+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`,
##            data = ds)
## 


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03, lm04,
##           type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

ds$event_negative_affect <- as.numeric(ds$Q9_1)
table(ds$event_negative_affect)

ds$event_positive_affect_01 <- (8 - ds$event_negative_affect)
table(ds$event_positive_affect_01)

ds$event_positive_affect_02 <- as.numeric(ds$Q9_2)
table(ds$event_positive_affect_02)

ds$event_positive_affect <- ds$event_positive_affect_02

ds$event_episodic_recall_01 <- as.numeric(ds$Q9_3)
table(ds$event_episodic_recall_01)

ds$event_episodic_recall_02 <- as.numeric(ds$Q9_4)
table(ds$event_episodic_recall_02)

ds$event_shared_perception_01 <- as.numeric(ds$Q9_5)
table(ds$event_shared_perception_01)

ds$event_shared_perception_02 <- as.numeric(ds$Q9_6)
table(ds$event_shared_perception_02)

ds$event_event_reflection_01 <- as.numeric(ds$Q9_9)
# table(ds$event_event_reflection_01)
ds$event_event_reflection_01 <- ifelse(ds$event_event_reflection_01 > 7, NA,
                                     ds$event_event_reflection_01)
table(ds$event_event_reflection_01)

ds$event_event_reflection_02 <- as.numeric(ds$Q9_10)
#table(ds$event_event_reflection_02)
ds$event_event_reflection_02 <- ifelse(ds$event_event_reflection_02 > 7, NA,
                                     ds$event_event_reflection_02)
table(ds$event_event_reflection_02)

ds$event_transformative_indiv_01 <- as.numeric(ds$Q9_7)
table(ds$event_transformative_indiv_01)

ds$event_transformative_indiv_02 <- as.numeric(ds$Q9_8)
table(ds$event_transformative_indiv_02)

ds$event_transformative_group_01 <- as.numeric(ds$Q9_11)
table(ds$event_transformative_group_01)

ds$event_transformative_group_02 <- as.numeric(ds$Q9_12)
table(ds$event_transformative_group_02)

imagistic <- cbind.data.frame(ds$event_negative_affect, ds$event_positive_affect, 
                              ds$event_episodic_recall_01, ds$event_episodic_recall_02,
                              ds$event_shared_perception_01, ds$event_shared_perception_02,
                              ds$event_event_reflection_01, ds$event_event_reflection_02,
                              ds$event_transformative_indiv_01, ds$event_transformative_indiv_02,
                              ds$event_transformative_group_01, ds$event_transformative_group_02)

imagistic <- na.omit(imagistic)

names(imagistic) <- sub('ds\\$event\\_', '', names(imagistic))

mtx <- cor(imagistic[, c(1:12)])

corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("darkred", "red", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F----------------------------------------

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(imagistic))

## Bartlett's test of sphericity
cortest.bartlett(imagistic)

## Parallel test
# parallel <- fa.parallel(imagistic)



## ----error = F, message = F, warning = F----------------------------------------

fit01 <- factanal(imagistic, 2, rotation="promax")
fit01



## ----error = F, message = F, warning = F----------------------------------------
fit02 <- factanal(imagistic, 6, rotation="promax")
fit02


## ----error = F, message = F, warning = F----------------------------------------

## Reliability:
alph01 <- psych::alpha(imagistic)
alph01



## ----error = F, message = F, warning = F----------------------------------------

ds$imagistic <- (((ds$event_positive_affect)+
                 (ds$event_negative_affect)+ 
                ((ds$event_episodic_recall_01+ds$event_episodic_recall_02)/2)+
                ((ds$event_shared_perception_01+ds$event_shared_perception_02)/2)+
                ((ds$event_event_reflection_01+ds$event_event_reflection_02)/2)+
                ((ds$event_transformative_indiv_01+ds$event_transformative_indiv_02)/2)+
                ((ds$event_transformative_group_01+ds$event_transformative_group_02)/2)))

summary(ds$imagistic)

ds %>% drop_na(imagistic)%>%
ggplot(aes(x = imagistic))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 34.00", 
                 xintercept = 34.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Imagistic experience", 
       y = "Frequency", 
       title = "Imagistic experience: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Positive affect:
negative_affect <- cbind.data.frame(ds$event_negative_affect)
negative_affect <- na.omit(negative_affect)

## Visualization:
ds$event_negative_affect <- (ds$event_negative_affect)
summary(ds$event_negative_affect)

ds %>% drop_na(event_negative_affect)%>%
ggplot(aes(x = event_negative_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 5.10", 
                 xintercept = 5.10, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Negative affect about event", 
       y = "Frequency", 
       title = "Negative affect about event: Pakistan")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Positive affect:
positive_affect <- cbind.data.frame(ds$event_positive_affect)
positive_affect <- na.omit(positive_affect)

## Visualization:
ds$event_positive_affect <- (ds$event_positive_affect)
summary(ds$event_positive_affect)

ds %>% drop_na(event_positive_affect)%>%
ggplot(aes(x = event_positive_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 2.70", 
                 xintercept = 2.70, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Positive affect about event", 
       y = "Frequency", 
       title = "Positive affect about event: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Episodic recall:
episodic_recall <- cbind.data.frame(ds$event_episodic_recall_01, ds$event_episodic_recall_02)
episodic_recall <- na.omit(episodic_recall)

names(episodic_recall) <- sub('ds\\$event\\_', '', names(episodic_recall))

mtx <- cor(episodic_recall[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(episodic_recall)
summary(alph01)

## Visualization:
ds$event_episodic_recall <- ((ds$event_episodic_recall_01+ds$event_episodic_recall_02)/2)
summary(ds$event_episodic_recall)

ds %>% drop_na(event_episodic_recall)%>%
ggplot(aes(x = event_episodic_recall))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 5.70", 
                 xintercept = 5.70, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Episodic recall of event", 
       y = "Frequency", 
       title = "Episodic recall of event: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Shared perception:
shared_perception <- cbind.data.frame(ds$event_shared_perception_01, ds$event_shared_perception_02)
shared_perception <- na.omit(shared_perception)

names(shared_perception) <- sub('ds\\$event\\_', '', names(shared_perception))

mtx <- cor(shared_perception[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(shared_perception)
summary(alph01)

## Visualization:
ds$event_shared_perception <- ((ds$event_shared_perception_01+ds$event_shared_perception_02)/2)
summary(ds$event_shared_perception)

ds %>% drop_na(event_shared_perception)%>%
ggplot(aes(x = event_shared_perception))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 5.60", 
                 xintercept = 5.60, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Shared perception of event", 
       y = "Frequency", 
       title = "Shared perception of event: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Reflection:
event_reflection <- cbind.data.frame(ds$event_event_reflection_01, ds$event_event_reflection_02)
event_reflection <- na.omit(event_reflection)

names(event_reflection) <- sub('ds\\$event\\_', '', names(event_reflection))

mtx <- cor(event_reflection[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(event_reflection)
summary(alph01)

## Visualization:
ds$event_event_reflection <- ((ds$event_event_reflection_01+ds$event_event_reflection_02)/2)
summary(ds$event_event_reflection)

ds %>% drop_na(event_event_reflection)%>%
ggplot(aes(x = event_event_reflection))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 4.80", 
                 xintercept = 4.80, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Reflection of event", 
       y = "Frequency", 
       title = "Reflection of event: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

## Reflection:
event_transformative_indiv <- cbind.data.frame(ds$event_transformative_indiv_01, ds$event_transformative_indiv_02)
event_transformative_indiv <- na.omit(event_transformative_indiv)

names(event_transformative_indiv) <- sub('ds\\$event\\_', '', names(event_transformative_indiv))

mtx <- cor(event_transformative_indiv[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(event_transformative_indiv)
summary(alph01)

## Visualization:
ds$event_transformative_indiv <- ((ds$event_transformative_indiv_01+ds$event_transformative_indiv_02)/2)
summary(ds$event_transformative_indiv)

ds %>% drop_na(event_transformative_indiv)%>%
ggplot(aes(x = event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for individual: Pakistan")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

event_transformative_group <- cbind.data.frame(ds$event_transformative_group_01, ds$event_transformative_group_02)
event_transformative_group <- na.omit(event_transformative_group)

names(event_transformative_group) <- sub('ds\\$event\\_', '', names(event_transformative_group))

mtx <- cor(event_transformative_group[, c(1:2)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alph01 <- psych::alpha(event_transformative_group)
summary(alph01)

## Visualization:
ds$event_transformative_group <- ((ds$event_transformative_group_01+ds$event_transformative_group_02)/2)
summary(ds$event_transformative_group)

ds %>% drop_na(event_transformative_group)%>%
ggplot(aes(x = event_transformative_group))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 4.90", 
                 xintercept = 4.90, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for group: Pakistan")+
  theme_bw()




## -------------------------------------------------------------------------------

imagistic_subscales <- cbind.data.frame(ds$event_negative_affect, ds$event_positive_affect, 
                                        ds$event_episodic_recall, 
                                        ds$event_shared_perception, ds$event_event_reflection, 
                                        ds$event_transformative_indiv, ds$event_transformative_group)


imagistic_subscales <- na.omit(imagistic_subscales)

names(imagistic_subscales) <- sub('ds\\$event\\_', '', names(imagistic_subscales))

mtx <- cor(imagistic_subscales[, c(1:6)])

## Correlation plot:
corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("darkred", "red", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F, eval = F------------------------------
## 
## ## Four regression models predicting IG/OG Fusion/Identification:
## 
## lm01 <- lm(IG_Fusion~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
## event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm02 <- lm(IG_Identification~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
## event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
##            data = ds)
## 
## lm03 <- lm(OG_Bonds~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
## event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
##            data = ds)
## 


## ----error = F, message = F, warning = F, results = "hide", eval = F------------
## 
## ## Tabulated results:
## 
## stargazer(lm01, lm02,
##           lm03,
##           type = "html",
##           star.cutoffs = c(0.05, 0.01, 0.001),
##           out = "table1.html")
## 


## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")


## -------------------------------------------------------------------------------

fwrite(ds, file = "~/Desktop/oxford/data/cleands/dspak.csv")


