## ----error = F, warning=F, message=F--------------------------------------------

rm(list=ls())

## Install "pacman" package if not installed
# (remove the # symbol from the line below):
# install.packages("pacman")

## Load R packages:
pacman::p_load(data.table, tidyverse, haven, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm, readxl, textdata, pdftools, 
               stringr, lubridate, tidytext, kableExtra, 
               patchwork, dotwhisker, vtable, 
               corpus, wordcloud, wordcloud2, RColorBrewer)

## Import dataset:
ds <- fread("/home/gagan/Desktop/oxford/data/uganda/Uganda_Source_updated.csv")
#ds <- read_excel("~/Desktop/oxford/data/uganda/Uganda Data Entry.xlsx")

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
  geom_textvline(label = "Mean = 39.68", 
                 xintercept = 39.68, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Age", 
       y = "Frequency", 
       title = "Age distribution")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$gender <- ifelse(ds$Gender == "female", "Female",
             ifelse(ds$Gender == "Female", "Female",
             ifelse(ds$Gender == "male", "Male",
             ifelse(ds$Gender == "Male", "Male", NA))))


lp02 <- ds %>% drop_na(gender) %>%
lollipop_chart(x = gender,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Gender distribution")+
  theme_bw()

lp02

bp01 <- ds %>% drop_na(gender) %>% 
  ggplot(aes(y = age, 
             x = gender))+
geom_boxplot(fill = "grey")+
  labs(y = "Age",
       x = "",
       title = "Age distribution by gender")+
  coord_flip()+
  theme_bw()

bp01



## ----error = F, message = F, warning = F----------------------------------------

ds$province <- ifelse(ds$Province == "Average", NA,
                      ds$Province)

lp03 <- ds %>% drop_na(province) %>%
lollipop_chart(x = province,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Province distribution")+
  theme_bw()

lp03



## ----error = F, message = F, warning = F----------------------------------------

ds$hhw <- ds$Household_level_of_wealth

ds$hh_wealth <- ifelse(ds$hhw == "Average", "Average", 
                ifelse(ds$hhw == "much higher", "Much higher",
                ifelse(ds$hhw == "Much Higher", "Much higher",
                ifelse(ds$hhw == "much lower", "Much lower",
                ifelse(ds$hhw == "Much lower", "Much lower",
                ifelse(ds$hhw == "Much Lower", "Much lower",
                ifelse(ds$hhw == "slightly higher", "Slightly higher",
                ifelse(ds$hhw == "Slightly higher", "Slightly higher",
                ifelse(ds$hhw == "Slightly Higher", "Slightly higher",
                ifelse(ds$hhw == "Slightly lower", "Slightly lower",
                ifelse(ds$hhw == "Slightly Lower", "Slightly lower", NA)))))))))))

ds$hh_wealth <- factor(ds$hh_wealth, 
                       levels=c("Much lower", "Slightly lower", "Average",
                                "Slightly higher", "Much higher"))

lp04 <- ds %>% drop_na(hh_wealth) %>%
lollipop_chart(x = hh_wealth,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Household wealth distribution")+
  theme_bw()

lp04



## ----error = F, warning=F, message=F--------------------------------------------

ds$ses1 <- ds$Socioeconomic_status

ds$ses <- ifelse(ds$ses1 == "lower middle", "Lower middle",
          ifelse(ds$ses1 == "lower Middle", "Lower middle",
          ifelse(ds$ses1 == "Lower middle", "Lower middle",
          ifelse(ds$ses1 == "middle", "Middle",
          ifelse(ds$ses1 == "Middle", "Middle",
          ifelse(ds$ses1 == "upper middle", "Upper middle",
          ifelse(ds$ses1 == "Upper middle", "Upper middle",
          ifelse(ds$ses1 == "Upper Middle", "Upper middle", NA))))))))

ds$ses <- factor(ds$ses, 
                 levels=c("Lower middle", "Middle", "Upper middle"))

lp05 <- ds %>% drop_na(ses) %>%
lollipop_chart(x = ses,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Socioeconomic status")+
  theme_bw()

lp05



## ----error = F, message = F, warning = F----------------------------------------

ds$income1 <- as.numeric(gsub("\\D", "", ds$Annual_income))
ds$income <- ifelse(ds$income1 == "400000500000", ((400000+500000)/2), ds$income1)

ds$income <- ifelse(ds$income < 10000000, ds$income, NA)

summary(ds$income)

ds %>% drop_na(income)%>%
ggplot(aes(x = income))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 80)+
  geom_textvline(label = "Mean = 1.4 M", 
                 xintercept = 1663154, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  scale_x_continuous(labels = scales::unit_format(unit = "M", 
                                                  scale = 1e-6))+
  labs(x = "Annual income", 
       y = "Frequency", 
       title = "Annual income distribution")+
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
corrplot(mtx, method = "number")



## ----error = F, message = F, warning = F----------------------------------------

ds$jobnature <- ifelse(ds$Job_Nature == "government", "Government",
                ifelse(ds$Job_Nature == "Government", "Government",
                ifelse(ds$Job_Nature == "non-government", "Non-government",
                ifelse(ds$Job_Nature == "Non-government", "Non-government",
                ifelse(ds$Job_Nature == "Non-Government", "Non-government",
                ifelse(ds$Job_Nature == "Other", "Other",
                ifelse(ds$Job_Nature == "Others- Both government and NGO", "Other",
                ifelse(ds$Job_Nature == "retired", "Retired",
                ifelse(ds$Job_Nature == "Self-employed", "Self-employed",
                ifelse(ds$Job_Nature == "Self-Employed", "Self-employed", NA))))))))))

lp06 <- ds %>% drop_na(jobnature) %>%
lollipop_chart(x = jobnature,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Nature of employment")+
  theme_bw()

lp06



## ----error = F, message = F, warning = F----------------------------------------

ds$rel <- ds$Religious_Affiliation

ds$rel01 <- ifelse(str_detect(ds$rel, "testant") == T, "Protestant", "")

ds$rel02 <- ifelse(str_detect(ds$rel, "atholic") == T, "Catholic", "")

ds$rel03 <- ifelse(str_detect(ds$rel, "unni") == T, "Sunni", "")

ds$rel04 <- ifelse(str_detect(ds$rel, "hia") == T, "Shia", "")

ds$religion <- ifelse(ds$rel01 == "Protestant", "Christian: Protestant",
               ifelse(ds$rel02 == "Catholic", "Christian: Catholic",
               ifelse(ds$rel03 == "Sunni", "Muslim: Sunni",
               ifelse(ds$rel04 == "Shia", "Muslim: Shia", "Other"))))

lp07 <- ds %>% drop_na(religion) %>%
lollipop_chart(x = religion,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Religious affiliation")+
  theme_bw()

lp07



## ----error = F, message = F, warning = F----------------------------------------

ds$mrg00 <- ds$`19A. Married?`
ds$married <- ifelse(ds$mrg00 %in% c("no", "No"), "No", "Yes")
ds$mlength <- ds$`19B. How long married`

ds$mrg01 <- as.numeric(gsub("\\D", "", ds$`19B. How long married`))
ds$mrg02 <- removeNumbers(ds$`19B. How long married`)
ds$mrg03 <- ifelse(str_detect(ds$mrg02, "ear") == T, "Years", "")
ds$mrg04 <- ifelse(str_detect(ds$mrg02, "onth") == T, "Months", "")

ds$mrg011 <- ifelse(ds$mrg01 == 3034, ((30+34)/2), 
             ifelse(ds$mrg01 == 112, 1.5, ds$mrg01))

ds$mrg033 <- ifelse(ds$mlength == 6, "Years",
             ifelse(ds$mlength == 30, "Years",
             ifelse(ds$mlength == 64, "Years", ds$mrg03)))       
                   
ds$mrg044 <- ifelse(ds$mrg033 == "Years", "Years",
             ifelse(ds$mrg04 == "Months", "Months", NA))

ds$mrg_length <- ifelse(ds$mrg044 == "Years", (ds$mrg011*12), 
                 ifelse(ds$mrg044 == "Months", ds$mrg011, NA))

ds$married_01 <- ifelse(ds$married == "Yes", "Yes",
                 ifelse(ds$married == "No" & ds$mrg_length >= 0, "Yes", "No"))

ds <- ds %>% mutate_at(c('married_01'), ~replace_na(.,"No"))

ds2 <- ds[, c("married", "married_01", 
              "mlength", "mrg01", "mrg011",
              "mrg02", "mrg03", "mrg033", "mrg04", 
              "mrg044", "mrg_length")]

ds$married <- ds$married_01
ds$married_months <- ds$mrg_length

ds <- as.data.table(ds)

ds <- subset(ds, select = -c(married_01, mlength, mrg01, 
                            mrg011, mrg02, mrg03, 
                            mrg033, mrg04, mrg044, 
                            mrg_length))

ds$married <- ifelse(ds$married == "Yes", "Married",
              ifelse(ds$married == "No", "Not married", ""))

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
       title = "Marital status")+
  theme_bw()

lp08



## ----error = F, message = F, warning = F----------------------------------------

ds2$marriage_length <- (ds2$mrg_length/12)

summary(ds2$marriage_length)

ds2 %>% drop_na(marriage_length)%>%
ggplot(aes(x = marriage_length))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 17 years", 
                 xintercept = 17.00, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Years", 
       y = "Frequency", 
       title = "Length of marriage")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$spb1 <- ds$`19C. Same background?`

ds$spb <- ifelse(ds$spb1 == "Yes", "Same background",
          ifelse(ds$spb1 == "yes", "Same background",
          ifelse(ds$spb1 == "no", "Different background",
          ifelse(ds$spb1 == "No", "Different background",
          ifelse(ds$spb1 == "None", "Different background", NA)))))

lp09 <- ds %>% 
lollipop_chart(x = spb,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Spouse background")+
  theme_bw()

lp09



## ----error = F, message = F, warning = F----------------------------------------
ds$children <- as.numeric(gsub("\\D", "", ds$Number_of_Children))

summary(ds$children)

ds %>% drop_na(children)%>%
ggplot(aes(x = children))+
  geom_bar(color = "black",
                 fill = "gray",
                 width = 0.75)+
  geom_textvline(label = "Mean = 3.60", 
                 xintercept = 3.60, 
                 vjust = 1.3, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Number of children", 
       y = "Frequency", 
       title = "Number of children")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

table(ds$Ethnicity)



## ----error = F, message = F, warning = F----------------------------------------

head(table(ds$Level_of_education), n = 15)

ds$education <- ds$Level_of_education



## ----error = F, message = F, warning = F----------------------------------------

## Four ingroup fusion items:
ds$IGF01 <- as.numeric(ds$`Q11.1 Group Bonds`)
ds$IGF02 <- as.numeric(ds$`Q11.2 Group Bonds`)
ds$IGF03 <- as.numeric(ds$`Q11.3 Group Bonds`)
ds$IGF04 <- as.numeric(ds$`Q11.4 Group Bonds`)

## Four ingroup identification items:
ds$IGI01 <- as.numeric(ds$`Q11.9 Group Bonds`)
ds$IGI02 <- as.numeric(ds$`Q11.10 Group Bonds`)
ds$IGI03 <- as.numeric(ds$`Q11.11 Group Bonds`)
ds$IGI04 <- as.numeric(ds$`Q11.12 Group Bonds`)

## Four outgroup fusion items:
ds$OGF01 <- as.numeric(ds$`Q11.5 Group Bonds`)
ds$OGF02 <- as.numeric(ds$`Q11.6 Group Bonds`)
ds$OGF03 <- as.numeric(ds$`Q11.7 Group Bonds`)
ds$OGF04 <- as.numeric(ds$`Q11.8 Group Bonds`)

## Four outgroup identification items:
ds$OGI01 <- as.numeric(ds$`Q11.13 Group Bonds`)
ds$OGI02 <- as.numeric(ds$`Q11.14 Group Bonds`)
ds$OGI03 <- as.numeric(ds$`Q11.15 Group Bonds`)
ds$OGI04 <- as.numeric(ds$`Q11.16 Group Bonds`)

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


## ----error = F, message = F, warning=F------------------------------------------

igbonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03, ds$IGF04,
                            ds$IGI01, ds$IGI02, ds$IGI03, ds$IGI04)

names(igbonds) <- sub('ds\\$', '', names(igbonds))

igbonds <- na.omit(igbonds)
mtx2 <- cor(igbonds[, c(1:8)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alpha02 <- psych::alpha(igbonds)

## Cronbach's Alpha:
summary(alpha02)



## ----error = F, message = F, warning=F------------------------------------------

## Item statistics:
alpha02$item.stats



## ----error = F, message = F, warning=F------------------------------------------

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----warning=F, message=F-------------------------------------------------------

ogbonds <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03, ds$OGF04,
                            ds$OGI01, ds$OGI02, ds$OGI03, ds$OGI04)

names(ogbonds) <- sub('ds\\$', '', names(ogbonds))

ogbonds <- na.omit(ogbonds)
mtx3 <- cor(ogbonds[, c(1:8)])

corrplot(mtx3, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha03 <- psych::alpha(ogbonds)

## Cronbach's Alpha:
summary(alpha03)

## Item statistics:
alpha03$item.stats

### Reliability if each item is dropped:
alpha03$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

## Three ingroup fusion items:
ds$IGF01 <- as.numeric(ds$`Q11.1 Group Bonds`)
ds$IGF02 <- as.numeric(ds$`Q11.2 Group Bonds`)
ds$IGF03 <- as.numeric(ds$`Q11.3 Group Bonds`)

## Three outgroup fusion items:
ds$OGF01 <- as.numeric(ds$`Q11.5 Group Bonds`)
ds$OGF02 <- as.numeric(ds$`Q11.6 Group Bonds`)
ds$OGF03 <- as.numeric(ds$`Q11.7 Group Bonds`)

## Three ingroup identification items:
ds$IGI01 <- as.numeric(ds$`Q11.9 Group Bonds`)
ds$IGI02 <- as.numeric(ds$`Q11.10 Group Bonds`)
ds$IGI03 <- as.numeric(ds$`Q11.11 Group Bonds`)

## Three outgroup identification items:
ds$OGI01 <- as.numeric(ds$`Q11.13 Group Bonds`)
ds$OGI02 <- as.numeric(ds$`Q11.14 Group Bonds`)
ds$OGI03 <- as.numeric(ds$`Q11.15 Group Bonds`)

## Bonds dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03,
                          ds$IGI01, ds$IGI02, ds$IGI03,
                          ds$OGF01, ds$OGF02, ds$OGF03,
                          ds$OGI01, ds$OGI02, ds$OGI03)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:9)])


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
fit01 <- factanal(bonds, 4, rotation="promax")
fit01


## ----error = F, message = F, warning=F------------------------------------------

igfusion <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03)
names(igfusion) <- sub('ds\\$', '', names(igfusion))

igfusion <- na.omit(igfusion)
mtx2 <- cor(igbonds[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(igfusion)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

igidnt <- cbind.data.frame(ds$IGI01, ds$IGI02, ds$IGI03)
names(igidnt) <- sub('ds\\$', '', names(igidnt))

igidnt <- na.omit(igidnt)
mtx2 <- cor(igidnt[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))


## Reliability:
alpha02 <- psych::alpha(igidnt)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

ogfusion <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03)
names(ogfusion) <- sub('ds\\$', '', names(ogfusion))

ogfusion <- na.omit(ogfusion)
mtx2 <- cor(ogfusion[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(ogfusion)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

ogidnt <- cbind.data.frame(ds$OGI01, ds$OGI02, ds$OGI03)
names(ogidnt) <- sub('ds\\$', '', names(ogidnt))

ogidnt <- na.omit(ogidnt)
mtx2 <- cor(ogidnt[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))


## Reliability:
alpha02 <- psych::alpha(ogidnt)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

## Four ingroup fusion items:
ds$IGF01 <- as.numeric(ds$`Q11.1 Group Bonds`)
ds$IGF02 <- as.numeric(ds$`Q11.2 Group Bonds`)
ds$IGF03 <- as.numeric(ds$`Q11.3 Group Bonds`)
ds$IGF04 <- as.numeric(ds$`Q11.4 Group Bonds`)

## Three ingroup identification items:
ds$IGI01 <- as.numeric(ds$`Q11.9 Group Bonds`)
ds$IGI02 <- as.numeric(ds$`Q11.10 Group Bonds`)
ds$IGI03 <- as.numeric(ds$`Q11.11 Group Bonds`)

## Four outgroup fusion items:
ds$OGF01 <- as.numeric(ds$`Q11.5 Group Bonds`)
ds$OGF02 <- as.numeric(ds$`Q11.6 Group Bonds`)
ds$OGF03 <- as.numeric(ds$`Q11.7 Group Bonds`)
ds$OGF04 <- as.numeric(ds$`Q11.8 Group Bonds`)

## Four outgroup identification items:
ds$OGI01 <- as.numeric(ds$`Q11.13 Group Bonds`)
ds$OGI02 <- as.numeric(ds$`Q11.14 Group Bonds`)
ds$OGI03 <- as.numeric(ds$`Q11.15 Group Bonds`)
ds$OGI04 <- as.numeric(ds$`Q11.16 Group Bonds`)


## Bonds dataframe:
bonds <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03, ds$IGF04,
                          ds$IGI01, ds$IGI02, ds$IGI03,
                          ds$OGF01, ds$OGF02, ds$OGF03, ds$OGF04,
                          ds$OGI01, ds$OGI02, ds$OGI03, ds$OGI04)

names(bonds) <- sub('ds\\$', '', names(bonds))

bonds <- na.omit(bonds)
mtx1 <- cor(bonds[, c(1:15)])


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
fit01 <- factanal(bonds, 4, rotation="promax")
fit01


## ----error = F, message = F, warning=F------------------------------------------

igfusion <- cbind.data.frame(ds$IGF01, ds$IGF02, ds$IGF03, ds$IGF04)
names(igfusion) <- sub('ds\\$', '', names(igfusion))

igfusion <- na.omit(igfusion)
mtx2 <- cor(igbonds[, c(1:4)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(igfusion)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

igidnt <- cbind.data.frame(ds$IGI01, ds$IGI02, ds$IGI03)
names(igidnt) <- sub('ds\\$', '', names(igidnt))

igidnt <- na.omit(igidnt)
mtx2 <- cor(igidnt[, c(1:3)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))


## Reliability:
alpha02 <- psych::alpha(igidnt)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

ogfusion <- cbind.data.frame(ds$OGF01, ds$OGF02, ds$OGF03, ds$OGF04)
names(ogfusion) <- sub('ds\\$', '', names(ogfusion))

ogfusion <- na.omit(ogfusion)
mtx2 <- cor(ogfusion[, c(1:4)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(ogfusion)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

ogidnt <- cbind.data.frame(ds$OGI01, ds$OGI02, ds$OGI03, ds$OGI04)
names(ogidnt) <- sub('ds\\$', '', names(ogidnt))

names(ogidnt)

ogidnt <- na.omit(ogidnt)
mtx2 <- cor(ogidnt[, c(1:4)])

corrplot(mtx2, method = "number", number.cex = 0.7,
         col=c("darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Reliability:
alpha02 <- psych::alpha(ogidnt)

## Cronbach's Alpha:
summary(alpha02)

## Item statistics:
alpha02$item.stats

### Reliability if each item is dropped:
alpha02$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

## BCL and BBL items:

# BCL_01:
# Seek out opportunities to bridge social divisions with their opponents, enemies, opposition groups, or other outgroups.   
# Variables: Q6.1, Q7.1

# BCL_02:
# Demonstrate willingness to compromise with their opponents, enemies, opposition groups, or other outgroups. 
# Variables: Q6.2, Q7.2

# BCL_03:
# Try to understand and empathize with their opponents, enemies, opposition groups, or other outgroups.  
# Variables: Q6.3, Q7.3

# BBL_01:
# Represent the interests of the communities and groups that they belong to even at the cost of other groups.
# Variables: Q6.4, Q7.4

# BBL_02:
# Focus on building stronger connections within the communities and groups they belong to rather than building stronger relationships with other groups across boundaries.
# Variables: Q6.5, Q7.5

# BBL_03:
# Try to gain benefits for the communities and groups they belong to even at the expense of other groups.
# Variables: Q6.6, Q7.6

ds$ENDBCL01 <- as.numeric(ds$`Q6.1 Leadership Quality`)
ds$ENDBCL02 <- as.numeric(ds$`Q6.2 Leadership Quality`)
ds$ENDBCL03 <- as.numeric(ds$`Q6.3 Leadership Quality`)
ds$ENDBBL01 <- as.numeric(ds$`Q6.4 Leadership Quality`)
ds$ENDBBL02 <- as.numeric(ds$`Q6.5 Leadership Quality`)
ds$ENDBBL03 <- as.numeric(ds$`Q6.6 Leadership Quality`)

ds$EXPBCL01 <- as.numeric(ds$`Q7.1 Leadership Experience`)
ds$EXPBCL02 <- as.numeric(ds$`Q7.2 Leadership Experience`)
ds$EXPBCL03 <- as.numeric(ds$`Q7.3 Leadership Experience`)
ds$EXPBBL01 <- as.numeric(ds$`Q7.4 Leadership Experience`)
ds$EXPBBL02 <- as.numeric(ds$`Q7.5 Leadership Experience`)
ds$EXPBBL03 <- as.numeric(ds$`Q7.6 Leadership Experience`)

leadership <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03, 
                               ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03,
                               ds$EXPBCL01, ds$EXPBCL02, ds$EXPBCL03,
                               ds$EXPBBL01, ds$EXPBBL02, ds$EXPBBL03)

names(leadership) <- sub('ds\\$', '', names(leadership))

leadership <- na.omit(leadership)
mtx1 <- cor(leadership[, c(1:12)])



## ----error = F, message = F, warning = F----------------------------------------
corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))


## ----warning=F, message=F-------------------------------------------------------

KMO(r=cor(leadership))



## ----warning=F, message=F-------------------------------------------------------

cortest.bartlett(leadership)



## ----error = F, message = F, warning = F----------------------------------------
# parallel <- fa.parallel(leadership)



## ----error = F, warning = F, message = F----------------------------------------
fit02 <- factanal(leadership, 4, rotation="promax")
fit02


## ----error = F, message = F, warning = F----------------------------------------

md <- c(1.000, -0.188, 0.427, -0.182,
        -0.188, 1.000, 0.284, 0.505,
        0.427, 0.284, 1.000, 0.155,
        -0.182, 0.505, 0.155, 1.000)

mat1 <- matrix(md, nrow=4, ncol=4,byrow=TRUE)

colnames(mat1) <- c("Endorsement_BBL", "Experience_BCL", 
                    "Experience_BBL", "Endorsement_BCL")

rownames(mat1) <- c("Endorsement_BBL", "Experience_BCL", 
                    "Experience_BBL", "Endorsement_BCL")

corrplot(mat1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning=F------------------------------------------

end_bcl <- cbind.data.frame(ds$ENDBCL01, ds$ENDBCL02, ds$ENDBCL03)
end_bbl <- cbind.data.frame(ds$ENDBBL01, ds$ENDBBL02, ds$ENDBBL03)
exp_bcl <- cbind.data.frame(ds$EXPBCL01, ds$EXPBCL02, ds$EXPBCL03)
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
mtx3 <- cor(exp_bcl[, c(1:3)])
mtx4 <- cor(exp_bbl[, c(1:3)])



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph01 <- psych::alpha(end_bcl)
summary(alph01)

## Item statistics:
alph01$item.stats

### Reliability if each item is dropped:
alph01$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph02 <- psych::alpha(end_bbl)
summary(alph02)

## Item statistics:
alph02$item.stats

### Reliability if each item is dropped:
alph02$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph03 <- psych::alpha(exp_bcl)
summary(alph03)

## Item statistics:
alph03$item.stats

### Reliability if each item is dropped:
alph03$alpha.drop



## ----error = F, message = F, warning=F------------------------------------------

## Reliability:
alph04 <- psych::alpha(exp_bbl)
summary(alph04)

## Item statistics:
alph04$item.stats

### Reliability if each item is dropped:
alph04$alpha.drop



## ----error = F, message = F, warning = F----------------------------------------

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03+ds$IGF04)/4)

ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03+ds$IGI04)/4)

ds$OG_Fusion <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03+ds$OGF04)/4)

ds$OG_Identification <- as.numeric((ds$OGI01+ds$OGI02+ds$OGI03+ds$OGI04)/4)

ds3 <- cbind.data.frame(ds$IG_Fusion, ds$IG_Identification,
                        ds$OG_Fusion, ds$OG_Identification)
ds3 <- na.omit(ds3)

names(ds3) <- sub('ds\\$', '', names(ds3))

mtx <- cor(ds3[, c(1:4)])

corrplot(mtx, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



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
ds$Experience_BCL <- as.numeric((ds$EXPBCL01+ds$EXPBCL02+ds$EXPBCL03)/3)
ds$Experience_BBL <- as.numeric((ds$EXPBBL01+ds$EXPBBL02+ds$EXPBBL03)/3)

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03)/3)
ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03)/3)
ds$OG_Fusion <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03)/3)
ds$OG_Identification <- as.numeric((ds$OGI01+ds$OGI02+ds$OGI03)/3)



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`,
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)


## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



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
ds$Experience_BCL <- as.numeric((ds$EXPBCL01+ds$EXPBCL02+ds$EXPBCL03)/3)
ds$Experience_BBL <- as.numeric((ds$EXPBBL01+ds$EXPBBL02+ds$EXPBBL03)/3)

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03+ds$IGF04)/4)
ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03)/3)
ds$OG_Fusion <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03+ds$OGF04)/4)
ds$OG_Identification <- as.numeric((ds$OGI01+ds$OGI02+ds$OGI03+ds$OGI04)/4)



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`,
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+Age+Female+Married+`SES-`, 
           data = ds)


## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Empathic concern
ds$empathic_concern_01 <- as.numeric(ds$`Q18.1 Empathy`)
ds$empathic_concern_02a <- as.numeric(ds$`Q18.2 Empathy`)
ds$empathic_concern_02 <- as.numeric(8 - ds$empathic_concern_02a)
ds$empathic_concern_03 <- as.numeric(ds$`Q18.3 Empathy`)

ds$empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03)/3

summary(ds$empathic_concern)

ds %>% drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.60", 
                 xintercept = 5.6, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$perspective_taking_01 <- as.numeric(ds$`Q18.4 Empathy`)
ds$perspective_taking_02 <- as.numeric(ds$`Q18.5 Empathy`)
ds$perspective_taking_03 <- as.numeric(ds$`Q18.6 Empathy`)
ds$perspective_taking_04 <- as.numeric(ds$`Q18.7 Empathy`)


ds$perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                          ds$perspective_taking_03+ds$perspective_taking_04)/4

summary(ds$perspective_taking)

ds %>% drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.9", 
                 xintercept = 5.9, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$og_coop_01 <- as.numeric(ds$`Q12.1 Outgroup`)
ds$og_coop_02 <- as.numeric(ds$`Q12.2 Outgroup`)

ds$og_cooperation <- (ds$og_coop_01+ds$og_coop_02)/2

summary(ds$og_cooperation)

ds %>% drop_na(og_cooperation)%>%
ggplot(aes(x = og_cooperation))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 4.90", 
                 xintercept = 4.90, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup cooperation score", 
       y = "Frequency", 
       title = "Outgroup cooperation")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$history_discrimination <- as.numeric(ds$`Q12.3 Outgroup`)

summary(ds$history_discrimination)

ds %>% drop_na(history_discrimination)%>%
ggplot(aes(x = history_discrimination))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 20)+
  geom_textvline(label = "Mean = 3.30", 
                 xintercept = 3.30, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perceived history of discrimination score", 
       y = "Frequency", 
       title = "Perceived history of discrimination")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$og_host_01 <- as.numeric(ds$`Q12.4 Outgroup`)
ds$og_host_02 <- as.numeric(ds$`Q12.5 Outgroup`)

ds$og_hostility <- (ds$og_host_01+ds$og_host_02)/2

summary(ds$og_hostility)

ds %>% drop_na(og_hostility)%>%
ggplot(aes(x = og_hostility))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 2.70", 
                 xintercept = 2.7, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup hostility score", 
       y = "Frequency", 
       title = "Outgroup hostility")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$fight_outgroup <- as.numeric(ds$`Q12.6 Outgroup`)

summary(ds$fight_outgroup)

ds %>% drop_na(fight_outgroup)%>%
ggplot(aes(x = fight_outgroup))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 2.10", 
                 xintercept = 2.1, 
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
ds$Experience_BCL <- as.numeric((ds$EXPBCL01+ds$EXPBCL02+ds$EXPBCL03)/3)
ds$Experience_BBL <- as.numeric((ds$EXPBBL01+ds$EXPBBL02+ds$EXPBBL03)/3)

ds$IG_Fusion <- as.numeric((ds$IGF01+ds$IGF02+ds$IGF03+ds$IGF04)/4)
ds$IG_Identification <- as.numeric((ds$IGI01+ds$IGI02+ds$IGI03)/3)
ds$OG_Fusion <- as.numeric((ds$OGF01+ds$OGF02+ds$OGF03+ds$OGF04)/4)
ds$OG_Identification <- as.numeric((ds$OGI01+ds$OGI02+ds$OGI03+ds$OGI04)/4)


## Empathic concern
ds$empathic_concern_01 <- as.numeric(ds$`Q18.1 Empathy`)
ds$empathic_concern_02a <- as.numeric(ds$`Q18.2 Empathy`)
ds$empathic_concern_02 <- as.numeric(8 - ds$empathic_concern_02a)
ds$empathic_concern_03 <- as.numeric(ds$`Q18.3 Empathy`)

ds$empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03)/3


ds$perspective_taking_01 <- as.numeric(ds$`Q18.4 Empathy`)
ds$perspective_taking_02 <- as.numeric(ds$`Q18.5 Empathy`)
ds$perspective_taking_03 <- as.numeric(ds$`Q18.6 Empathy`)
ds$perspective_taking_04 <- as.numeric(ds$`Q18.7 Empathy`)


ds$perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                          ds$perspective_taking_03+ds$perspective_taking_04)/4

ds$og_coop_01 <- as.numeric(ds$`Q12.1 Outgroup`)
ds$og_coop_02 <- as.numeric(ds$`Q12.2 Outgroup`)

ds$og_cooperation <- (ds$og_coop_01+ds$og_coop_02)/2

ds$history_discrimination <- as.numeric(ds$`Q12.3 Outgroup`)

ds$og_host_01 <- as.numeric(ds$`Q12.4 Outgroup`)
ds$og_host_02 <- as.numeric(ds$`Q12.5 Outgroup`)

ds$og_hostility <- (ds$og_host_01+ds$og_host_02)/2

ds$fight_outgroup <- as.numeric(ds$`Q12.6 Outgroup`)



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(og_cooperation~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm02 <- lm(og_hostility~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(fight_outgroup~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+empathic_concern+perspective_taking+history_discrimination+Age+Female+Married+`SES-`, 
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_per_01 <- as.numeric(ds$`Q16.1 Religious Freedom`)
ds$religious_freedom_per_02a <- as.numeric(ds$`Q16.2 Religious Freedom`)
ds$religious_freedom_per_02 <- (8 - ds$religious_freedom_per_02a)
ds$religious_freedom_per_03 <- as.numeric(ds$`Q16.3 Religious Freedom`)
ds$religious_freedom_per_04 <- as.numeric(ds$`Q16.4 Religious Freedom`)
ds$religious_freedom_per_05 <- as.numeric(ds$`Q16.5 Religious Freedom`)
ds$religious_freedom_per_06 <- as.numeric(ds$`Q16.6 Religious Freedom`)
ds$religious_freedom_per_07 <- as.numeric(ds$`Q16.7 Religious Freedom`)
ds$religious_freedom_per_08 <- as.numeric(ds$`Q16.8 Religious Freedom`)

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
  geom_textvline(label = "Mean = 5.60", 
                 xintercept = 5.60, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "SPRF score", 
       y = "Frequency", 
       title = "Social perception of religious freedom")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$religious_freedom_exp_01a <- as.numeric(ds$`Q17.1 Life experience`)
ds$religious_freedom_exp_01 <- (8 - ds$religious_freedom_exp_01)

ds$religious_freedom_exp_02a <- as.numeric(ds$`Q17.2 Life experience`)
ds$religious_freedom_exp_02 <- (8 - ds$religious_freedom_exp_02)

ds$religious_freedom_exp_03a <- as.numeric(ds$`Q17.3 Life experience`)
ds$religious_freedom_exp_03 <- (8 - ds$religious_freedom_exp_03)

ds$religious_freedom_exp_04a <- as.numeric(ds$`Q17.4 Life experience`)
ds$religious_freedom_exp_04 <- (8 - ds$religious_freedom_exp_04)

ds$religious_freedom_exp_05a <- as.numeric(ds$`Q17.5 Life experience`)
ds$religious_freedom_exp_05 <- (8 - ds$religious_freedom_exp_05)

ds$religious_freedom_exp_06a <- as.numeric(ds$`Q17.6 Life experience`)
ds$religious_freedom_exp_06 <- (8 - ds$religious_freedom_exp_06)

ds$exp_religious_freedom <- (ds$religious_freedom_exp_01+ds$religious_freedom_exp_02+
                             ds$religious_freedom_exp_03+ds$religious_freedom_exp_04+
                              ds$religious_freedom_exp_05+ds$religious_freedom_exp_06)/6

summary(ds$exp_religious_freedom)

ds %>% drop_na(exp_religious_freedom)%>%
ggplot(aes(x = exp_religious_freedom))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.30", 
                 xintercept = 5.30, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Experience of religious freedom score", 
       y = "Frequency", 
       title = "Experience of religious freedom")+
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
       title = "Experience of religious freedom")+
  coord_flip()+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$pc <- ds$`Q15. Positve Contact`

# never, very rarely, rarely
# sometimes, often, very often, always

ds$pc01 <- ifelse(ds$pc == "never", "Never",
           ifelse(ds$pc == "Never", "Never",
           ifelse(ds$pc == "very rarely", "Very rarely",
           ifelse(ds$pc == "Very rarely", "Very rarely",
           ifelse(ds$pc == "Very Rarely", "Very rarely",
           ifelse(ds$pc == "rarely", "Rarely",
           ifelse(ds$pc == "Rarely", "Rarely",
           ifelse(ds$pc == "sometimes", "Sometimes",
           ifelse(ds$pc == "Sometimes", "Sometimes",
           ifelse(ds$pc == "often", "Often",
           ifelse(ds$pc == "Often", "Often",
           ifelse(ds$pc == "very often", "Very often",
           ifelse(ds$pc == "Very often", "Very often",
           ifelse(ds$pc == "Very Often", "Very often",
           ifelse(ds$pc == "Always", "Always", NA)))))))))))))))

ds$pc01 <- factor(ds$pc01, 
                  level = c("Never", "Very rarely", "Rarely",
                            "Sometimes", "Often", "Very often", "Always"))

table(ds$pc01)

ds$pc02 <- as.numeric(ds$pc01)

summary(ds$pc02)

ds %>% drop_na(pc02)%>%
ggplot(aes(x = pc02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 4.10", 
                 xintercept = 4.10, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Positive contact score", 
       y = "Frequency", 
       title = "Positive contact with outgroup")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$nc <- ds$`Q14. Negativ Outgroup`

ds$nc01 <- ifelse(ds$nc == "never", "Never",
           ifelse(ds$nc == "Never", "Never", 
           ifelse(ds$nc == "very rarely", "Very rarely", 
           ifelse(ds$nc == "Very rarely", "Very rarely",
           ifelse(ds$nc == "Very Rarely", "Very rarely", 
           ifelse(ds$nc == "Veyr rarely", "Very rarely",
           ifelse(ds$nc == "rarely", "Rarely", 
           ifelse(ds$nc == "Rarely", "Rarely",
           ifelse(ds$nc == "some times", "Sometimes", 
           ifelse(ds$nc == "sometimes", "Sometimes",
           ifelse(ds$nc == "Sometimes", "Sometimes", 
           ifelse(ds$nc == "often", "Often",
           ifelse(ds$nc == "verry often", "Very often", 
           ifelse(ds$nc == "very often", "Very often",
           ifelse(ds$nc == "Very often", "Very often", 
           ifelse(ds$nc == "Very Often", "Very often",
           ifelse(ds$nc == "always", "Always", 
           ifelse(ds$nc == "Always", "Always",
           ifelse(ds$nc == "Aways", "Always", NA)))))))))))))))))))
              
ds$nc01 <- factor(ds$nc01, 
                  level = c("Never", "Very rarely", "Rarely",
                            "Sometimes", "Often", "Very often", "Always"))

table(ds$nc01)

ds$nc02 <- as.numeric(ds$nc01)

summary(ds$nc02)

ds %>% drop_na(nc02)%>%
ggplot(aes(x = nc02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 2.40", 
                 xintercept = 2.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Negative contact score", 
       y = "Frequency", 
       title = "Negative contact with outgroup")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

ds$intergroup_marriage_01 <- as.numeric(ds$`Q20.1 Marriage`)
ds$intergroup_marriage_02 <- as.numeric(ds$`Q20.2 Marriage`)
ds$intergroup_marriage_03 <- as.numeric(ds$`Q20.3 Marriage`)
ds$intergroup_marriage_04 <- as.numeric(ds$`Q20.4 Marriage`)

ig_marriage <- cbind.data.frame(ds$intergroup_marriage_01, ds$intergroup_marriage_02,
                                ds$intergroup_marriage_03, ds$intergroup_marriage_04)

names(ig_marriage) <- sub('ds\\$', '', names(ig_marriage))

ig_marriage <- na.omit(ig_marriage)
mtx1 <- cor(ig_marriage[, c(1:4)])

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(ig_marriage))

## Bartlett's test of sphericity
cortest.bartlett(ig_marriage)

## Parallel test
# parallel <- fa.parallel(ig_marriage)



## ----error = F, message = F, warning = F----------------------------------------
fit01 <- factanal(ig_marriage, 1, rotation="promax")
fit01

## Reliability analysis
psych::alpha(ig_marriage)



## ----error = F, message = F, warning = F----------------------------------------

ds$endorse_intergroup_marriage <- (ds$intergroup_marriage_01+ds$intergroup_marriage_02+
                                   ds$intergroup_marriage_03+ds$intergroup_marriage_04)/4

summary(ds$endorse_intergroup_marriage)

ds %>% drop_na(endorse_intergroup_marriage)%>%
ggplot(aes(x = endorse_intergroup_marriage))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 30)+
  geom_textvline(label = "Mean = 5.40", 
                 xintercept = 5.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Intergroup marriage endorsement score", 
       y = "Frequency", 
       title = "Support for intergroup marriage")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

## Feel outgroup: negative to positive:

ds$ogaf1 <- ds$`Q13.1 Feel Outgroup- Negative to Positive`

ds$og_aff_01 <- ifelse(ds$ogaf1 == "very negative", "Very negative",
                ifelse(ds$ogaf1 == "Very negative", "Very negative",
                ifelse(ds$ogaf1 == "Very Negative", "Very negative",
                ifelse(ds$ogaf1 == "moderately negative", "Moderately negative",
                ifelse(ds$ogaf1 == "Moderately negative", "Moderately negative",
                ifelse(ds$ogaf1 == "Moderately Negative", "Moderately negative",
                ifelse(ds$ogaf1 == "a little negative", "A little negative",
                ifelse(ds$ogaf1 == "A little negative", "A little negative",
                ifelse(ds$ogaf1 == "neutral", "Neutral",
                ifelse(ds$ogaf1 == "Neutral", "Neutral",
                ifelse(ds$ogaf1 == "a little positive", "A little positive",
                ifelse(ds$ogaf1 == "A little positive", "A little positive",
                ifelse(ds$ogaf1 == "moderately positive", "Moderately positive",
                ifelse(ds$ogaf1 == "Moderately positive", "Moderately positive",
                ifelse(ds$ogaf1 == "Moderately Positive", "Moderately positive",
                ifelse(ds$ogaf1 == "very positive", "Very positive",
                ifelse(ds$ogaf1 == "Very positive", "Very positive",
                ifelse(ds$ogaf1 == "Very Positive", "Very positive", NA))))))))))))))))))

ds$og_aff_01 <- factor(ds$og_aff_01, 
                       levels=c("Very negative", "Moderately negative",
                                 "A little negative", "Neutral", 
                                 "A little positive", "Moderately positive", 
                                 "Very positive"))
table(ds$og_aff_01)


## Feel Outgroup: Hostile to friendly:

ds$ogaf2 <- (ds$`Q13.2 Feel Outgroup - Hostile to Friendly`)

ds$og_aff_02 <- ifelse(ds$ogaf2 == "very hostile", "Very hostile", 
                ifelse(ds$ogaf2 == "Very hostile", "Very hostile", 
                ifelse(ds$ogaf2 == "Very Hostile", "Very hostile", 
                ifelse(ds$ogaf2 == "moderately Hostile", "Moderately hostile", 
                ifelse(ds$ogaf2 == "Moderately hostile", "Moderately hostile", 
                ifelse(ds$ogaf2 == "Moderately Hostile", "Moderately hostile", 
                ifelse(ds$ogaf2 == "a little hostile", "A little hostile", 
                ifelse(ds$ogaf2 == "A little hostile", "A little hostile", 
                ifelse(ds$ogaf2 == "neutral", "Neutral", 
                ifelse(ds$ogaf2 == "Neutral", "Neutral", 
                ifelse(ds$ogaf2 == "a little friendly", "A little friendly", 
                ifelse(ds$ogaf2 == "A little friendly", "A little friendly", 
                ifelse(ds$ogaf2 == "A little Friendly", "A little friendly", 
                ifelse(ds$ogaf2 == "moderately friendly", "Moderately friendly", 
                ifelse(ds$ogaf2 == "Moderately friendly", "Moderately friendly", 
                ifelse(ds$ogaf2 == "Moderately Friendly", "Moderately friendly", 
                ifelse(ds$ogaf2 == "very friendly", "Very friendly", 
                ifelse(ds$ogaf2 == "Very friendly", "Very friendly", ""))))))))))))))))))
                
ds$og_aff_02 <- factor(ds$og_aff_02, 
                       levels = c("Very hostile", "Moderately hostile", 
                                  "A little hostile", "Neutral", 
                                  "A little friendly", "Moderately friendly",
                                  "Very friendly"))

table(ds$og_aff_02)


## Feel Outgroup: Suspicious to trusting:

ds$ogaf3 <- ds$`Q13.3 Feel Outgroup - Suspicious to Trusting`

ds$og_aff_03 <- ifelse(ds$ogaf3 == "very suspicious", "Very suspicious", 
                ifelse(ds$ogaf3 == "moderately suspicious", "Moderately suspicious",
                ifelse(ds$ogaf3 == "Moderately suspicious", "Moderately suspicious",
                ifelse(ds$ogaf3 == "a little suspicious", "A little suspicious",
                ifelse(ds$ogaf3 == "A little suspicious", "A little suspicious",
                ifelse(ds$ogaf3 == "A  little suspicious", "A little suspicious", 
                ifelse(ds$ogaf3 == "neutral", "Neutral",
                ifelse(ds$ogaf3 == "Neutral", "Neutral",
                ifelse(ds$ogaf3 == "a little respect", "A little trusting",
                ifelse(ds$ogaf3 == "a little trusting", "A little trusting",
                ifelse(ds$ogaf3 == "A little trusting", "A little trusting", 
                ifelse(ds$ogaf3 == "A little Trusting", "A little trusting",
                ifelse(ds$ogaf3 == "moderately trusting", "Moderately trusting",
                ifelse(ds$ogaf3 == "Moderately trusting", "Moderately trusting",
                ifelse(ds$ogaf3 == "very trusting", "Very trusting",
                ifelse(ds$ogaf3 == "Very trusting", "Very trusting", NA))))))))))))))))
                       
ds$og_aff_03 <- factor(ds$og_aff_03, 
                       levels = c("Very suspicious", "Moderately suspicious", 
                                  "A little suspicious", "Neutral",
                                  "A little trusting", "Moderately trusting",
                                  "Very trusting"))
table(ds$og_aff_03)


## Feel Outgroup: Contempt to respect:

ds$ogaf4 <- ds$`Q13.4 Feel Outgroup - Contempt to Respect`

ds$og_aff_04 <- ifelse(ds$ogaf4 == "A lot of contempt", "A lot of contempt",
                ifelse(ds$ogaf4 == "A lot of Contempt", "A lot of contempt",
                ifelse(ds$ogaf4 == "Moderate contempt", "Moderate contempt",
                ifelse(ds$ogaf4 == "a little contempt", "A little contempt",
                ifelse(ds$ogaf4 == "A little contempt", "A little contempt",
                ifelse(ds$ogaf4 == "neutral", "Neutral",
                ifelse(ds$ogaf4 == "Neutral", "Neutral",
                ifelse(ds$ogaf4 == "a little respect", "A little respect",
                ifelse(ds$ogaf4 == "A little respect", "A little respect",
                ifelse(ds$ogaf4 == "Moderate respect", "Moderate respect",
                ifelse(ds$ogaf4 == "a lot of respect", "A lot of respect",
                ifelse(ds$ogaf4 == "A lot of respect", "A lot of respect", NA))))))))))))
                       
ds$og_aff_04 <- factor(ds$og_aff_04, 
                       levels = c("A lot of contempt", "Moderate contempt",
                                  "A little contempt", "Neutral", 
                                  "A little respect", "Moderate respect",
                                  "A lot of respect"))

table(ds$og_aff_04)


## Feel Outgroup: Concerned to Unconcerned:

ds$ogaf5 <- ds$`Q13.5 Feel Outgroup - Concerned to Unconcerned`

table(ds$ogaf5)

ds$og_aff_05 <- ifelse(ds$ogaf5 == "very concerned", "Very concerned",
                ifelse(ds$ogaf5 == "Very concerned", "Very concerned", 
                ifelse(ds$ogaf5 == "Very Concerned", "Very concerned", 
                ifelse(ds$ogaf5 == "moderately concerned", "Moderately concerned", 
                ifelse(ds$ogaf5 == "Moderately concerned", "Moderately concerned", 
                ifelse(ds$ogaf5 == "a little concerned", "A little concerned",
                ifelse(ds$ogaf5 == "A little concerned", "A little concerned", 
                ifelse(ds$ogaf5 == "neutral", "Neutral", 
                ifelse(ds$ogaf5 == "Neutral", "Neutral", 
                ifelse(ds$ogaf5 == "A little uconcerned", "A little unconcerned", 
                ifelse(ds$ogaf5 == "a little unconcerned", "A little unconcerned",
                ifelse(ds$ogaf5 == "A little unconcerned", "A little unconcerned", 
                ifelse(ds$ogaf5 == "Moderately unconcerned", "Moderately unconcerned", 
                ifelse(ds$ogaf5 == "very unconcerned", "Very unconcerned", 
                ifelse(ds$ogaf5 == "Very unconcerned", "Very unconcerned", NA)))))))))))))))

ds$og_aff_05 <- factor(ds$og_aff_05, 
                       levels = c("Very concerned", "Moderately concerned", 
                                  "A little concerned", "Neutral",
                                  "A little unconcerned", "Moderately unconcerned",
                                  "Very unconcerned"))
table(ds$og_aff_05)

## Feel Outgroup: Threatened to Relaxed:

ds$ogaf6 <- ds$`Q13.6 Feel Outgroup - Threatened to Relaxed`

## convert first character only to upper case:
ds$ogaf6a <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$ogaf6, perl = TRUE)

ds$og_aff_06 <- ifelse(ds$ogaf6a == "Alittle relaxed", "A little relaxed", 
                ifelse(ds$ogaf6a == "N/a", NA, 
                ifelse(ds$ogaf6a == "Never", NA,ds$ogaf6a)))


ds$og_aff_06 <- factor(ds$og_aff_06, 
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

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(og_affect))

## Bartlett's test of sphericity
cortest.bartlett(og_affect)

## Parallel test
# parallel <- fa.parallel(og_affect)



## ----error = F, message = F, warning = F----------------------------------------
fit01 <- factanal(og_affect, 1, rotation="promax")
fit01

## Reliability analysis

psych::alpha(og_affect)


## ----error = F, message = F, warning = F----------------------------------------

og_affect <- cbind.data.frame(ds$NegativeToPositive, ds$HostileToFriendly,
                              ds$SuspiciousToTrusting, ds$ContemptToRespect,
                              ds$ThreatenedToRelaxed)

names(og_affect) <- sub('ds\\$', '', names(og_affect))

og_affect <- na.omit(og_affect)
mtx1 <- cor(og_affect[, c(1:5)])

corrplot(mtx1, method = "number", number.cex = 0.7,
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(og_affect))

## Bartlett's test of sphericity
cortest.bartlett(og_affect)

## Parallel test
# parallel <- fa.parallel(og_affect)

## Reliability analysis:
psych::alpha(og_affect)



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
  geom_textvline(label = "Mean = 4.40", 
                 xintercept = 4.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup affect score", 
       y = "Frequency", 
       title = "Outgroup affect")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$freq_positive_contact <- ds$pc02
ds$freq_negative_contact <- ds$nc02
ds$perspectiveXdiscrimination <- ds$perspective_taking*ds$history_discrimination

## Four regression models predicting endorsement and experience of BCL / BBL:

lm01 <- lm(Endorse_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm02 <- lm(Experience_BCL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(Endorse_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm04 <- lm(Experience_BBL~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models:

lm01 <- lm(og_cooperation~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm02 <- lm(og_hostility~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm03 <- lm(fight_outgroup~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)

lm04 <- lm(og_affect~IG_Fusion+IG_Identification+OG_Fusion+OG_Identification+freq_positive_contact+freq_negative_contact+empathic_concern+perspective_taking+history_discrimination+perspectiveXdiscrimination+Age+Female+Married+`SES-`, 
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

ds$event_negative_affect <- as.numeric(ds$`Q9.1 Feelings about Event`)
table(ds$event_negative_affect)

ds$event_positive_affect_01 <- (8 - ds$event_negative_affect)
table(ds$event_positive_affect_01)

ds$event_positive_affect_02 <- as.numeric(ds$`Q9.2 Feelings about Event`)
table(ds$event_positive_affect_02)

ds$event_positive_affect <- ds$event_positive_affect_02

ds$event_episodic_recall_01 <- as.numeric(ds$`Q9.3 Feelings about Event`)
table(ds$event_episodic_recall_01)

ds$event_episodic_recall_02 <- as.numeric(ds$`Q9.4 Feelings about Event`)
table(ds$event_episodic_recall_02)

ds$event_shared_perception_01 <- as.numeric(ds$`Q9.5 Feelings about Event`)
table(ds$event_shared_perception_01)

ds$event_shared_perception_02 <- as.numeric(ds$`Q9.6 Feelings about Event`)
table(ds$event_shared_perception_02)

ds$event_event_reflection_01 <- as.numeric(ds$`Q9.9 Feelings about Event`)
table(ds$event_event_reflection_01)

ds$event_event_reflection_02 <- as.numeric(ds$`Q9.10 Feelings about Event`)
table(ds$event_event_reflection_02)

ds$event_transformative_indiv_01 <- as.numeric(ds$`Q9.7 Feelings about Event`)
table(ds$event_transformative_indiv_01)

ds$event_transformative_indiv_02 <- as.numeric(ds$`Q9.8 Feelings about Event`)
table(ds$event_transformative_indiv_02)

ds$event_transformative_group_01 <- as.numeric(ds$`Q9.11 Feelings about Event`)
table(ds$event_transformative_group_01)

ds$event_transformative_group_02 <- as.numeric(ds$`Q9.12 Feelings about Event`)
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
         col=c("white", "darkred", "red",
               "darkgrey", "blue", "darkblue"))



## ----error = F, message = F, warning = F----------------------------------------

## Kaiser-Meyer-Olkin (KMO) test of factorability
KMO(r=cor(imagistic))

## Bartlett's test of sphericity
cortest.bartlett(imagistic)

## Parallel test
# parallel <- fa.parallel(imagistic)



## ----error = F, message = F, warning = F----------------------------------------

fit03 <- factanal(imagistic, 3, rotation="promax")
fit03



## ----error = F, message = F, warning = F----------------------------------------

fit06 <- factanal(imagistic, 6, rotation="promax")
fit06



## ----error = F, message = F, warning = F----------------------------------------

imagistic <- cbind.data.frame(ds$event_negative_affect, ds$event_positive_affect, 
                              ds$event_episodic_recall_01, ds$event_episodic_recall_02,
                              ds$event_shared_perception_01, ds$event_shared_perception_02,
                              ds$event_event_reflection_01, ds$event_event_reflection_02,
                              ds$event_transformative_indiv_01, ds$event_transformative_indiv_02,
                              ds$event_transformative_group_01, ds$event_transformative_group_02)


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
  geom_textvline(label = "Mean = 39.00", 
                 xintercept = 39.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Imagistic experience", 
       y = "Frequency", 
       title = "Imagistic experience: Uganda")+
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
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Negative affect about event", 
       y = "Frequency", 
       title = "Negative affect about event: Uganda")+
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
  geom_textvline(label = "Mean = 4.90", 
                 xintercept = 4.90, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Positive affect about event", 
       y = "Frequency", 
       title = "Positive affect about event: Uganda")+
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
  geom_textvline(label = "Mean = 6.50", 
                 xintercept = 6.50, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Episodic recall of event", 
       y = "Frequency", 
       title = "Episodic recall of event: Uganda")+
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
  geom_textvline(label = "Mean = 5.80", 
                 xintercept = 5.80, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Shared perception of event", 
       y = "Frequency", 
       title = "Shared perception of event: Uganda")+
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
  geom_textvline(label = "Mean = 6.20", 
                 xintercept = 6.20, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Reflection of event", 
       y = "Frequency", 
       title = "Reflection of event: Uganda")+
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
psych::alpha(event_transformative_indiv)

## Visualization:
ds$event_transformative_indiv <- ((ds$event_transformative_indiv_01+ds$event_transformative_indiv_02)/2)
summary(ds$event_transformative_indiv)

ds %>% drop_na(event_transformative_indiv)%>%
ggplot(aes(x = event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 6.10", 
                 xintercept = 6.10, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for individual: Uganda")+
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
  geom_textvline(label = "Mean = 5.40", 
                 xintercept = 5.40, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Transformative event score", 
       y = "Frequency", 
       title = "Transformative event for group: Uganda")+
  theme_bw()




## -------------------------------------------------------------------------------

imagistic_subscales <- cbind.data.frame(ds$event_positive_affect, ds$event_negative_affect,
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



## ----error = F, message = F, warning = F----------------------------------------

## Four regression models predicting IG/OG Fusion/Identification:

lm01 <- lm(IG_Fusion~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
           data = ds)

lm02 <- lm(IG_Identification~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
           data = ds)

lm03 <- lm(OG_Fusion~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
           data = ds)

lm04 <- lm(OG_Identification~event_positive_affect+event_negative_affect+event_episodic_recall+event_shared_perception+event_event_reflection+
event_transformative_indiv+event_transformative_group+Age+Female+Married+`SES-`,
           data = ds)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Tabulated results:

stargazer(lm01, lm02,
          lm03, lm04,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------
htmltools::includeHTML("table1.html")


## -------------------------------------------------------------------------------

fwrite(ds, file = "~/Desktop/oxford/data/cleands/dsuga.csv")



## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## ds02 <- fread("~/Desktop/oxford/data/uganda/Ethnic Data Uganda.csv")
## 
## ds02$education <- ds02$`Recoded Education`
## ds02$ethnicity <- ds02$`Recoded Category`
## 
## ds02a <- ds02[, c("Entry", "education", "ethnicity")]
## 
## ds <- merge(ds, ds02a, by="Entry")
## 
## ds$description <- ds$`Q8D. Description`
## 
## ds$ethnicity <- ifelse(ds$ethnicity == "luo", "Luo",
##                 ifelse(ds$ethnicity == "N/A", NA,
##                 ifelse(ds$ethnicity == "Nilo-Ham", "Nilotic",
##                 ifelse(ds$ethnicity == "Nubis", "Nilotic", ds$ethnicity))))
## 
## table(ds$ethnicity)
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## ## Import text related datasets:
## bing <- as.data.table(get_sentiments("bing"))
## stopwords <- as.data.table(stop_words)
## 
## ## Corpus of most common words:
## corpus1 <- ds %>%
##   unnest_tokens(word, description)
## 
## corpus1 <- as.data.table(corpus1)
## stopwords <- as.data.table(stopwords)
## 
## ## Remove stop words:
## corpus1 <- corpus1[!stopwords, on = "word"]
## 
## ## Merge corpus with data:
## df2 <- merge(corpus1, bing, by = "word")
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## a <- sort(table(corpus1$word), decreasing = T)
## 
## tblwc <- as.data.table(a)
## head(tblwc, n = 15)
## 
## wordcloud(words = tblwc$V1,
##                  freq = tblwc$N,
##                  min.freq = 1,
##                  max.words=300,
##                  random.order=F,
##                  rot.per=0.35,
##                  colors=brewer.pal(8, "Dark2"))


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## corpus2 <- corpus1[corpus1$ethnicity=="Bantu",]
## a <- sort(table(corpus2$word), decreasing = T)
## 
## tblwc <- as.data.table(a)
## head(tblwc, n = 15)
## 
## wordcloud(words = tblwc$V1,
##                  freq = tblwc$N,
##                  min.freq = 1,
##                  max.words=300,
##                  random.order=F,
##                  rot.per=0.35,
##                  colors=brewer.pal(8, "Dark2"))
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## corpus2 <- corpus1[corpus1$ethnicity=="Luo",]
## a <- sort(table(corpus2$word), decreasing = T)
## 
## tblwc <- as.data.table(a)
## head(tblwc, n = 15)
## 
## wordcloud(words = tblwc$V1,
##                  freq = tblwc$N,
##                  min.freq = 1,
##                  max.words=300,
##                  random.order=F,
##                  rot.per=0.35,
##                  colors=brewer.pal(8, "Dark2"))


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## corpus2 <- corpus1[corpus1$ethnicity=="Nilotic",]
## a <- sort(table(corpus2$word), decreasing = T)
## 
## tblwc <- as.data.table(a)
## head(tblwc, n = 15)
## 
## wordcloud(words = tblwc$V1,
##                  freq = tblwc$N,
##                  min.freq = 1,
##                  max.words=300,
##                  random.order=F,
##                  rot.per=0.35,
##                  colors=brewer.pal(8, "Dark2"))
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## df2 <- as.data.table(df2)
## 
## ## Positive - Negative words per entry:
## df2 <- df2[, .(positive = sum(sentiment == "positive"),
##                negative = sum(sentiment == "negative")),
##           by = "Entry"]
## 
## df2 <- df2[, sentiment_raw := (positive - negative)]
## 
## df3 <- merge(ds, df2, by = "Entry")
## 
## ## Create wordcount variable:
## df3$wordcount <- stringi::stri_count_words(df3$description)
## 
## df3 <- as.data.table(df3)
## 
## ## Standardized sentiment score for each entry:
## df3 <- df3[, sentiment := (sentiment_raw / wordcount)]
## ds <- df3
## 
## ds1 <- ds %>% drop_na(ethnicity)
## 
## ## Boxplot:
## 
## boxp1 <- ggplot(data = ds1,
##               aes(x = ethnicity,
##                   y = wordcount))+
##     geom_boxplot()+
## #    ylim(-0.1, 0.1)+
## labs(x="Ethnicity",
##        y = "Word count",
##        title = "Word count by ethnicity: Uganda")+
##   theme_bw()
## 
## boxp1
## 
## 
## ## Density plot
## 
## dp1 <- ggplot(data = ds1,
##               aes(x = wordcount,
##                   group = ethnicity,
##                   fill = ethnicity)) +
##       geom_density(adjust=1.5, alpha=.4)+
##       geom_vline(aes(xintercept=0),
##                color="black",
##                size=0.75,
##                linetype=4)+
##       #xlim(500, 500)+
##       labs(x = "Word count",
##            y = "Density",
##            title = "Word count by ethnicity: Uganda",
##            caption = "",
##            fill = "Ethnicity:")+
##         theme_bw()+
##       scale_fill_colorblind()
## 
## dp1
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## ## Boxplot:
## 
## boxp1 <- ggplot(data = ds1,
##               aes(x = ethnicity,
##                   y = sentiment))+
##     geom_boxplot()+
##     ylim(-0.1, 0.1)+
##     geom_hline(aes(yintercept=0),
##                color="red",
##                size=0.5)+
##   labs(x="Ethnicity",
##        y = "Standardized Bing Sentiment Score",
##        title = "Sentiment score by ethnicity: Uganda")+
##   theme_bw()
## 
## boxp1
## 
## 
## ## Density plot
## 
## dp01 <- ggplot(data = ds1,
##               aes(x = sentiment,
##                   group = ethnicity,
##                   fill = ethnicity)) +
##       geom_density(adjust=1.5, alpha=.4)+
##       geom_vline(aes(xintercept=0),
##                color="black",
##                size=0.75,
##                linetype=4)+
##       labs(x = "Standardized Bing Sentiment Score",
##            y = "Density",
##            title = "Sentiment score by ethnicity: Uganda",
##            caption = "",
##            fill = "Ethnicity:")+
##         theme_bw()+
##       scale_fill_colorblind()
## 
## dp01
## 


## ----error = F, message = F, warning = F, eval=F--------------------------------
## 
## ## Boxplot:
## 
## boxp02 <- ds1 %>% drop_na(gender) %>%
##   ggplot(aes(x = gender,
##              y = sentiment))+
##     geom_boxplot()+
##     ylim(-0.15, 0.15)+
##     geom_hline(aes(yintercept=0),
##                color="red",
##                size=0.5)+
##   labs(x = "Gender",
##        y = "Standardized Bing Sentiment Score",
##        title = "Sentiment score by gender: Uganda")+
##   theme_bw()
## 
## boxp02
## 
## 
## ## Density plot
## 
## dp02 <- ds1 %>% drop_na(gender) %>%
##   ggplot(aes(x = sentiment,
##              group = gender,
##              fill = gender)) +
##       geom_density(adjust=1.5, alpha=.4)+
##       geom_vline(aes(xintercept=0),
##                color="black",
##                size=0.75,
##                linetype=4)+
##       labs(x = "Standardized Bing Sentiment Score",
##            y = "Density",
##            title = "Sentiment score by gender: Uganda",
##            caption = "",
##            fill = "Gender:")+
##         theme_bw()+
##       scale_fill_colorblind()
## 
## dp02
## 

