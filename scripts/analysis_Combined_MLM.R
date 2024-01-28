## ----error = F, warning=F, message=F--------------------------------------------

options(digits = 2)
rm(list=ls())

## Install "pacman" package if not installed
# (remove the # symbol from the line below):
# install.packages("pacman")

## Load R packages:
pacman::p_load(data.table, tidyverse, haven, labelled, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm, readxl, patchwork, modelsummary,
               gt, lme4, car, lmerTest, 
               ggeffects, magrittr, broom, broom.mixed,
               backports, effects, interactions, plyr, sjPlot)


## Import datasets:
## These are newer datasets with new variables created in the individual analyses:
dsgmb <- fread("/home/gagan/Desktop/oxford/data/cleanedds/dsgmb.csv")
dsgmb$Country <- "Gambia"
dsgmb$id <- 1:nrow(dsgmb)
dsgmb$ID <- paste0("GMB",dsgmb$id)

dspak <- fread("/home/gagan/Desktop/oxford/data/cleanedds/dspak.csv")
dspak$Country <- "Pakistan"
dspak$id <- 1:nrow(dspak)
dspak$ID <- paste0("PAK",dspak$id)

dstza <- fread("/home/gagan/Desktop/oxford/data/cleanedds/dstza.csv")
dstza$Country <- "Tanzania"
dstza$id <- 1:nrow(dstza)
dstza$ID <- paste0("TZA",dstza$id)

dsuga <- fread("/home/gagan/Desktop/oxford/data/cleanedds/dsuga.csv")
dsuga$Country <- "Uganda"
dsuga$id <- 1:nrow(dsuga)
dsuga$ID <- paste0("UGA",dsuga$id)

## Correct asterisk pattern for stargazer tables:
starpattern <- "<em>&#42;p&lt;0.05;&nbsp;&#42;&#42;p&lt;0.01;&nbsp;&#42;&#42;&#42;p&lt;0.001</em>"

## List of variables to retain from all datasets:
list1 <- c("ID", "Country", "age", "gender", 
           "ses", "jobnature", "religion", "married",
           "IGF01", "IGF02", "IGF03", "IGI01", "IGI02", "IGI03", 
           "OGF01", "OGF02", "OGF03", "OGI01", "OGI02", "OGI03",
           "ENDBCL01", "ENDBCL02", "ENDBCL03", "ENDBBL01", "ENDBBL02", "ENDBBL03",
           "EXPBCL01", "EXPBCL02", "EXPBCL03", "EXPBBL01", "EXPBBL02", "EXPBBL03", 
           "empathic_concern_01", "empathic_concern_02", "empathic_concern_03",
           "perspective_taking_01", "perspective_taking_02", 
           "perspective_taking_03", "perspective_taking_04", "history_discrimination",
           "og_hostility", "og_cooperation", "fight_outgroup", "imagistic",
           "event_positive_affect", "event_negative_affect", "event_episodic_recall",
           "event_shared_perception", "event_event_reflection", 
           "event_transformative_indiv", "event_transformative_group",
           "freq_positive_contact", "freq_negative_contact", 
           "sprf", "exp_religious_freedom")

## Subset datasets to only the columns in the above list:
dsgmb1 <- dsgmb[, ..list1]
dspak1 <- dspak[, ..list1]
dstza1 <- dstza[, ..list1]
dsuga1 <- dsuga[, ..list1]

## Merged dataset with needed columns only
ds <- rbind(dsgmb1, dspak1, dstza1, dsuga1)

## Rename the "Event_" columns with title case:
ds01 <- ds[, 1:44]
ds02 <- ds[, !(2:44)]
colnames(ds02) <- stringr::str_to_title(colnames(ds02))
ds02$ID <- ds02$Id
ds02 <- ds02[, !1]
ds <- merge(ds01, ds02, by = "ID")
rm(ds01, ds02)



## ----error = F, message = F, warning = F----------------------------------------







## ----error = F, message = F, warning = F----------------------------------------

tbl01 <- table(ds$Country)

## Table of user language by country:
tbl01

## Sample size by country:

lp01 <- ds %>% 
  # drop_na(Country) %>%
  lollipop_chart(x = Country,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Sample size by country")+
  theme_bw()

lp01



## ----error = F, message = F, warning = F----------------------------------------

summary(ds$age)

ds %>% drop_na(age)%>%
ggplot(aes(x = age))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  geom_textvline(label = "Mean = 37.00", 
                 xintercept = 37.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Age", 
       y = "Frequency", 
       title = "Age distribution (full sample)")+
  theme_bw()

ds %>% drop_na(age)%>%
ggplot(aes(x = age))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 50)+
  labs(x = "Age", 
       y = "Frequency", 
       title = "Age distribution by country")+
  facet_wrap(~Country, nrow = 2)+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$gender <- ifelse(ds$gender == "Male", "Male",
             ifelse(ds$gender == "Female", "Female", NA))

lp02 <- ds %>% 
  drop_na(gender, age, Country) %>%
lollipop_chart(x = gender,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Gender distribution (full sample)")+
#  facet_wrap(~Country, nrow = 2)+
  theme_bw()

lp02


bp01 <- ds %>% drop_na(gender, age) %>% 
  ggplot(aes(y = age, 
             x = gender))+
geom_boxplot(fill = "grey")+
  labs(y = "Age",
       x = "",
       title = "Age and gender distribution by country")+
  facet_wrap(~Country, nrow = 2)+
  coord_flip()+
  theme_bw()

bp01



## ----error = F, message = F, warning = F----------------------------------------

ds$ses <- ifelse(ds$ses == "", NA, ds$ses)
table(ds$ses)

ds %>% drop_na(ses) %>%
lollipop_chart(x = ses,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Socioeconomic status (full sample)")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$jobnature <- ifelse(ds$jobnature == "", NA, ds$jobnature)

#sentence case:
ds$jobnature <- gsub("(\\D)(\\D+)", "\\U\\1\\L\\2", ds$jobnature, perl = TRUE)

ds$jobnature <- ifelse(ds$jobnature == "Non-government/self-employed", 
                       "Non-government", ds$jobnature)

table(ds$jobnature)

ds %>% drop_na(jobnature) %>%
lollipop_chart(x = jobnature,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Nature of employment (full sample)")+
  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$religion <- ifelse(ds$religion == "", NA, ds$religion)

ds$religion <- ifelse(ds$religion == "Christian (Catholic)", "Christian: Catholic", 
               ifelse(ds$religion == "Christian (Protestant)", "Christian: Protestant",
               ifelse(ds$religion == "Muslim (Shia)", "Muslim: Shia",
               ifelse(ds$religion == "Muslim (Sunni)", "Muslim: Sunni", ds$religion))))
table(ds$religion)

lp05 <- ds %>% drop_na(religion) %>%
lollipop_chart(x = religion,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Religious distribution (full sample)")+
  theme_bw()

lp05



## ----error = F, message = F, warning = F----------------------------------------

ds$married <- ifelse(ds$married == "Not married", "Unmarried", ds$married)
ds$married <- ifelse(ds$married == "", NA, ds$married)

table(ds$married)



## ----error = F, message = F, warning = F----------------------------------------

## Gambia:
eth <- as.data.frame(table(dsgmb$Ethnicity))
eth$Var1 <- as.character(eth$Var1)
eth$ethnicity <- ifelse(eth$Freq < 2, "Other", eth$Var1)
l1 <- as.list(eth$ethnicity)

dsgmb2 <- dsgmb[, c("Ethnicity")]

dsgmb2$ethnicity <- ifelse(dsgmb2$Ethnicity %in% l1, dsgmb2$Ethnicity, "Other")
dsgmb2$ethnicity <- ifelse(dsgmb2$ethnicity == "Serere", "Serer",
                 ifelse(dsgmb2$ethnicity == "", NA, dsgmb2$ethnicity))

table(dsgmb2$ethnicity)

ethgmb <- dsgmb2 %>% drop_na(ethnicity) %>%
lollipop_chart(x = ethnicity,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Ethnic distribution: Gambia")+
  theme_bw()

ethgmb

## Pakistan:
eth <- as.data.frame(table(dspak$Ethnicity))
eth$Var1 <- as.character(eth$Var1)
eth$ethnicity <- ifelse(eth$Freq < 7, "Other", eth$Var1)
l1 <- as.list(eth$ethnicity)

dspak2 <- dspak[, c("Ethnicity")]

dspak2$ethnicity <- ifelse(dspak2$Ethnicity %in% l1, dspak2$Ethnicity, "Other")
dspak2$ethnicity <- ifelse(dspak2$ethnicity == "Serere", "Serer",
                 ifelse(dspak2$ethnicity == "", NA, dspak2$ethnicity))

table(dspak2$ethnicity)

ethpak <- dspak2 %>% drop_na(ethnicity) %>%
lollipop_chart(x = ethnicity,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Ethnic distribution: Pakistan")+
  theme_bw()

ethpak

## Tanzania:
eth <- as.data.frame(table(dstza$Ethnicity))
eth$Var1 <- as.character(eth$Var1)
eth$ethnicity <- ifelse(eth$Freq < 7, "Other", eth$Var1)
l1 <- as.list(eth$ethnicity)

dstza2 <- dstza[, c("Ethnicity")]

dstza2$ethnicity <- ifelse(dstza2$Ethnicity %in% l1, dstza2$Ethnicity, "Other")
dstza2$ethnicity <- ifelse(dstza2$ethnicity == "Serere", "Serer",
                 ifelse(dstza2$ethnicity == "", NA, dstza2$ethnicity))

table(dstza2$ethnicity)

ethtza <- dstza2 %>% drop_na(ethnicity) %>%
lollipop_chart(x = ethnicity,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Ethnic distribution: Tanzania")+
  theme_bw()

ethtza

## Uganda:
eth <- as.data.frame(table(dsuga$Ethnicity))
eth$Var1 <- as.character(eth$Var1)
eth$ethnicity <- ifelse(eth$Freq < 7, "Other", eth$Var1)
l1 <- as.list(eth$ethnicity)

dsuga2 <- dsuga[, c("Ethnicity")]

dsuga2$ethnicity <- ifelse(dsuga2$Ethnicity %in% l1, dsuga2$Ethnicity, "Other")
dsuga2$ethnicity <- ifelse(dsuga2$ethnicity == "Serere", "Serer",
                 ifelse(dsuga2$ethnicity == "", NA, dsuga2$ethnicity))

table(dsuga2$ethnicity)

ethuga <- dsuga2 %>% drop_na(ethnicity) %>%
lollipop_chart(x = ethnicity,
               line_color = "black",
               point_color = "black")+
  labs(y = "Frequency",
       x = "",
       title = "Ethnic distribution: Uganda")+
  theme_bw()

ethuga



## ----error = F, message = F, warning=F------------------------------------------

ds$bbl <- (ds$ENDBBL01+ ds$ENDBBL02+ ds$ENDBBL03)/3
ds$bcl <- (ds$ENDBCL01+ ds$ENDBCL02+ ds$ENDBCL03)/3

summary(ds$bbl)

ds %>% drop_na(bbl)%>%
ggplot(aes(x = bbl))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Endorsement of BBL score", 
       y = "Frequency", 
       title = "Endorsement of BBL (full sample)")+
  theme_bw()


ds %>% drop_na(bbl, Country)%>%
ggplot(aes(x = bbl))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Endorsement of BBL score", 
       y = "Frequency", 
       title = "Endorsement of BBL by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(bbl)%>%
ggplot(aes(x = bbl,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
 labs(x = "Endorsement of BBL score", 
       y = "Frequency", 
       title = "Endorsement of BBL by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$bcl)

ds %>% drop_na(bcl)%>%
ggplot(aes(x = bcl))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Endorsement of BCL score", 
       y = "Frequency", 
       title = "Endorsement of BCL (full sample)")+
  theme_bw()


ds %>% drop_na(bcl, Country)%>%
ggplot(aes(x = bcl))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Endorsement of BCL score", 
       y = "Frequency", 
       title = "Endorsement of BCL by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(bcl)%>%
ggplot(aes(x = bcl,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
 labs(x = "Endorsement of BCL score", 
       y = "Frequency", 
       title = "Endorsement of BCL by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$igfusion <- (ds$IGF01+ds$IGF02+ds$IGF03)/3

summary(ds$igfusion)

ds %>% drop_na(igfusion)%>%
ggplot(aes(x = igfusion))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Ingroup fusion score", 
       y = "Frequency", 
       title = "Ingroup fusion (full sample)")+
  theme_bw()


ds %>% drop_na(igfusion, Country)%>%
ggplot(aes(x = igfusion))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Ingroup fusion score", 
       y = "Frequency", 
       title = "Ingroup fusion by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(igfusion)%>%
ggplot(aes(x = igfusion,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Ingroup fusion score", 
       y = "Frequency", 
       title = "Ingroup fusion by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$IGI01 <- ifelse(ds$IGI01 %in% 1:7, ds$IGI01, NA)
ds$igidentification <- (ds$IGI01+ds$IGI02+ds$IGI03)/3

summary(ds$igidentification)

ds %>% drop_na(igidentification)%>%
ggplot(aes(x = igidentification))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Ingroup identification score", 
       y = "Frequency", 
       title = "Ingroup identification (full sample)")+
  theme_bw()


ds %>% drop_na(igidentification, Country)%>%
ggplot(aes(x = igidentification))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Ingroup identification score", 
       y = "Frequency", 
       title = "Ingroup identification by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(igidentification)%>%
ggplot(aes(x = igidentification,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Ingroup identification score", 
       y = "Frequency", 
       title = "Ingroup identification by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$ogbonds <- (ds$OGI01+ds$OGI02+ds$OGI03+
               ds$OGF01+ds$OGF02+ds$OGF03)/6

summary(ds$ogbonds)

ds %>% drop_na(ogbonds)%>%
ggplot(aes(x = ogbonds))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 35)+
  geom_textvline(label = "Mean = 3.00", 
                 xintercept = 3.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Outgroup bonds score", 
       y = "Frequency", 
       title = "Outgroup bonds (full sample)")+
  theme_bw()


ds %>% drop_na(ogbonds, Country)%>%
ggplot(aes(x = ogbonds))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Outgroup bonds score", 
       y = "Frequency", 
       title = "Outgroup bonds by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(ogbonds)%>%
ggplot(aes(x = ogbonds,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 3.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
 labs(x = "Outgroup bonds score", 
       y = "Frequency", 
       title = "Outgroup bonds by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03)/3

summary(ds$empathic_concern)

ds %>% drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 35)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern (full sample)")+
  theme_bw()


ds %>% drop_na(empathic_concern, Country)%>%
ggplot(aes(x = empathic_concern))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(empathic_concern)%>%
ggplot(aes(x = empathic_concern,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
 labs(x = "Empathic concern score", 
       y = "Frequency", 
       title = "Empathic concern by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                          ds$perspective_taking_03+ds$perspective_taking_04)/4

summary(ds$perspective_taking)

ds %>% drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 35)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking (full sample)")+
  theme_bw()


ds %>% drop_na(perspective_taking, Country)%>%
ggplot(aes(x = perspective_taking))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 25)+
 labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(perspective_taking)%>%
ggplot(aes(x = perspective_taking,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
 labs(x = "Perspective taking score", 
       y = "Frequency", 
       title = "Perspective taking by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$history_discrimination)

ds %>% drop_na(history_discrimination)%>%
ggplot(aes(x = history_discrimination))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Perceived history of discrimination score", 
       y = "Frequency", 
       title = "Perceived history of discrimination (full sample)")+
  theme_bw()


ds %>% drop_na(history_discrimination, Country)%>%
ggplot(aes(x = history_discrimination))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 10)+
 labs(x = "Perceived history of discrimination score", 
       y = "Frequency", 
       title = "Perceived history of discrimination by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(history_discrimination)%>%
ggplot(aes(x = history_discrimination,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Perceived history of discrimination score", 
       y = "Frequency", 
       title = "Perceived history of discrimination by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_positive_affect)

ds %>% drop_na(Event_positive_affect)%>%
ggplot(aes(x = Event_positive_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 4.00", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Positive affect score", 
       y = "Frequency", 
       title = "Event: Positive affect (full sample)")+
  theme_bw()


ds %>% drop_na(Event_positive_affect, Country)%>%
ggplot(aes(x = Event_positive_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Positive affect score", 
       y = "Frequency", 
       title = "Event: Positive affect by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_positive_affect)%>%
ggplot(aes(x = Event_positive_affect,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 4.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Positive affect score", 
       y = "Frequency", 
       title = "Event: Positive affect by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_negative_affect)

ds %>% drop_na(Event_negative_affect)%>%
ggplot(aes(x = Event_negative_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Negative affect score", 
       y = "Frequency", 
       title = "Event: Negative affect (full sample)")+
  theme_bw()


ds %>% drop_na(Event_negative_affect, Country)%>%
ggplot(aes(x = Event_negative_affect))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Negative affect score", 
       y = "Frequency", 
       title = "Event: Negative affect by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_negative_affect)%>%
ggplot(aes(x = Event_negative_affect,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Negative affect score", 
       y = "Frequency", 
       title = "Event: Negative affect by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_episodic_recall)

ds %>% drop_na(Event_episodic_recall)%>%
ggplot(aes(x = Event_episodic_recall))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Episodic recall score", 
       y = "Frequency", 
       title = "Event: Episodic recall (full sample)")+
  theme_bw()


ds %>% drop_na(Event_episodic_recall, Country)%>%
ggplot(aes(x = Event_episodic_recall))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Episodic recall score", 
       y = "Frequency", 
       title = "Event: Episodic recall by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_episodic_recall)%>%
ggplot(aes(x = Event_episodic_recall,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Episodic recall score", 
       y = "Frequency", 
       title = "Event: Episodic recall by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_shared_perception)

ds %>% drop_na(Event_shared_perception)%>%
ggplot(aes(x = Event_shared_perception))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Shared perception score", 
       y = "Frequency", 
       title = "Event: Shared perception (full sample)")+
  theme_bw()


ds %>% drop_na(Event_shared_perception, Country)%>%
ggplot(aes(x = Event_shared_perception))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Shared perception score", 
       y = "Frequency", 
       title = "Event: Shared perception by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_shared_perception)%>%
ggplot(aes(x = Event_shared_perception,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Shared perception score", 
       y = "Frequency", 
       title = "Event: Shared perception by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$Event_reflection <- ds$Event_event_reflection
summary(ds$Event_reflection)

ds %>% drop_na(Event_reflection)%>%
ggplot(aes(x = Event_reflection))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 6.00", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Reflection score", 
       y = "Frequency", 
       title = "Event: Reflection (full sample)")+
  theme_bw()


ds %>% drop_na(Event_reflection, Country)%>%
ggplot(aes(x = Event_reflection))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Reflection score", 
       y = "Frequency", 
       title = "Event: Reflection by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_reflection)%>%
ggplot(aes(x = Event_reflection,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 6.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Reflection score", 
       y = "Frequency", 
       title = "Event: Reflection by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_transformative_indiv)

ds %>% drop_na(Event_transformative_indiv)%>%
ggplot(aes(x = Event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Transformative for individual score", 
       y = "Frequency", 
       title = "Event: Transformative for individual (full sample)")+
  theme_bw()


ds %>% drop_na(Event_transformative_indiv, Country)%>%
ggplot(aes(x = Event_transformative_indiv))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Transformative for individual score", 
       y = "Frequency", 
       title = "Event: Transformative for individual by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_transformative_indiv)%>%
ggplot(aes(x = Event_transformative_indiv,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Transformative for individual score", 
       y = "Frequency", 
       title = "Event: Transformative for individual by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

summary(ds$Event_transformative_group)

ds %>% drop_na(Event_transformative_group)%>%
ggplot(aes(x = Event_transformative_group))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 5.00", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Transformative for group score", 
       y = "Frequency", 
       title = "Event: Transformative for group (full sample)")+
  theme_bw()


ds %>% drop_na(Event_transformative_group, Country)%>%
ggplot(aes(x = Event_transformative_group))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Transformative for group score", 
       y = "Frequency", 
       title = "Event: Transformative for group by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_transformative_group)%>%
ggplot(aes(x = Event_transformative_group,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 5.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Transformative for group score", 
       y = "Frequency", 
       title = "Event: Transformative for group by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning=F------------------------------------------

ds$Event_imagistic <- ds$imagistic

summary(ds$Event_imagistic)

ds %>% drop_na(Event_imagistic)%>%
ggplot(aes(x = Event_imagistic))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
  geom_textvline(label = "Mean = 35.00", 
                 xintercept = 35.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(x = "Event: Imagistic score", 
       y = "Frequency", 
       title = "Event: Imagistic (full sample)")+
  theme_bw()


ds %>% drop_na(Event_imagistic, Country)%>%
ggplot(aes(x = Event_imagistic))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 15)+
 labs(x = "Event: Imagistic score", 
       y = "Frequency", 
       title = "Event: Imagistic by country")+
  facet_wrap(~Country)+
  theme_bw()

ds %>% drop_na(Event_imagistic)%>%
ggplot(aes(x = Event_imagistic,
           y = Country))+
  geom_boxplot(fill = "grey")+
  geom_textvline(label = " ", 
                 xintercept = 35.00, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+ 
  labs(x = "Event: Imagistic score", 
       y = "Frequency", 
       title = "Event: Imagistic by country")+
  #facet_wrap(~Country, nrow = 2)+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------
ds$SPRF <- ds$Sprf

## Varying intercept model with no predictors:
m00<- lmer(SPRF ~ 1 + (1 | Country), 
           data = ds)

summary(m00)



## ----error = F, message = F, warning = F----------------------------------------

tab_model(m00)



## ----error = F, message = F, warning = F----------------------------------------

## Create proper variable names:

ds$IG_Fusion <- (ds$IGF01+ds$IGF02+ds$IGF03)/3

ds$IG_Identification <- ds$igidentification

ds$OG_Bonds <- (ds$OGI01+ds$OGI02+ds$OGI03+
               ds$OGF01+ds$OGF02+ds$OGF03)/6

ds$Empathic_concern <- (ds$empathic_concern_01+ds$empathic_concern_02+
                        ds$empathic_concern_03)/3

ds$Perspective_taking <- (ds$perspective_taking_01+ds$perspective_taking_02+
                        ds$perspective_taking_03+ds$perspective_taking_03)/4

ds$Age <- ds$age

ds$Wealth_level <- as.factor(ifelse(ds$ses == "Lower middle", 1, 
                             ifelse(ds$ses == "Middle", 2,
                             ifelse(ds$ses == "Upper middle", 3,
                             ifelse(ds$ses == "Upper", 4, "")))))

ds$Female <- as.factor(ifelse(ds$gender == "Female", 1, 0))

ds$Married <- ds$married

ds$Perception_religious_freedom <- ds$SPRF

## Varying intercept models with individual-level predictors:
m01 <- lmer(Perception_religious_freedom~IG_Fusion+IG_Identification+OG_Bonds+Empathic_concern+
              Perspective_taking+Age+Female+Married+Wealth_level+
              (1 | Country), 
            data = ds)

summary(m01)
tab_model(m01)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Change class of all models so we can use stargazer():
class(m00) <- "lmerMod"
class(m01) <- "lmerMod"

## Tabulated results:
stargazer(m00, m01,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------

htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Varying intercept models with individual-level predictors:
m02 <- lmer(Perception_religious_freedom~Event_shared_perception+Event_episodic_recall+
              Event_reflection+Event_positive_affect+Event_negative_affect+
              Event_transformative_indiv+Event_transformative_group+
              Age+Female+Married+Wealth_level+
              (1 | Country), 
            data = ds)

summary(m02)
tab_model(m02)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Change class of all models so we can use stargazer():
class(m00) <- "lmerMod"
class(m01) <- "lmerMod"
class(m02) <- "lmerMod"

## Tabulated results:
stargazer(m00, m01, m02,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------

htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

summary(ds$SPRF)

ggplot(data = ds, 
       aes(x = SPRF)) +
  geom_histogram(color = "black",
                 bins = 20)+
  xlim(1, 7)+
  geom_textvline(label = "Mean = 5.0", 
                 xintercept = 5.0, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(y = "Frequency",
       x = "SPRF score", 
       title = "Social Perception of Religiour Freedom")+
  theme_bw()


ggplot(data = ds, 
       aes(x = SPRF)) +
  geom_histogram(color = "black",
                 bins = 20)+
  xlim(1, 7)+
  labs(y = "Frequency",
       x = "SPRF score", 
       title = "Social Perception of Religiour Freedom")+
  facet_wrap( ~ Country, nrow = 2) +
  theme_bw()

df01 <- ds %>% drop_na(SPRF)

tbl01 <- aggregate(df01$SPRF, 
                    by=list(df01$Country),
                    FUN=mean)
tbl01$Country <- tbl01$Group.1
tbl01$SPRF <- tbl01$x
tbl01 <- tbl01[, 3:4]
tbl01

ggplot(data = df01, 
       aes(x = SPRF, 
           y = Country)) +
  geom_boxplot(color = "black",
               fill = "grey")+
  xlim(1, 7)+
  geom_textvline(label = "Mean = 4.70", 
                 xintercept = 4.70, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(y = "",
       x = "SPRF score", 
       title = "Social Perceptions of Relgious Freedom")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------
ds$Exp_religious_freedom <- ds$Exp_religious_freedom

## Varying intercept model with no predictors:
m10<- lmer(Exp_religious_freedom ~ 1 + (1 | Country), 
           data = ds)

summary(m10)



## ----error = F, message = F, warning = F----------------------------------------

tab_model(m10)



## ----error = F, message = F, warning = F----------------------------------------

## Varying intercept models with individual-level predictors:
m11 <- lmer(Exp_religious_freedom~IG_Fusion+IG_Identification+OG_Bonds+Empathic_concern+
              Perspective_taking+Age+Female+Married+Wealth_level+
              (1 | Country), 
            data = ds)

summary(m11)
tab_model(m11)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Change class of all models so we can use stargazer():
class(m10) <- "lmerMod"
class(m11) <- "lmerMod"

## Tabulated results:
stargazer(m10, m11,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------

htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

## Varying intercept models with individual-level predictors:
m12 <- lmer(Exp_religious_freedom~Event_shared_perception+Event_episodic_recall+
              Event_reflection+Event_positive_affect+Event_negative_affect+
              Event_transformative_indiv+Event_transformative_group+
              Age+Female+Married+Wealth_level+
              (1 | Country), 
            data = ds)

summary(m12)
tab_model(m12)



## ----error = F, message = F, warning = F, results = "hide"----------------------

## Change class of all models so we can use stargazer():
class(m10) <- "lmerMod"
class(m11) <- "lmerMod"
class(m12) <- "lmerMod"

## Tabulated results:
stargazer(m10, m11, m12,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html")



## -------------------------------------------------------------------------------

htmltools::includeHTML("table1.html")



## ----error = F, message = F, warning = F----------------------------------------

df01 <- ds%>% drop_na(Exp_religious_freedom)
summary(df01$Exp_religious_freedom)

ggplot(data = df01, 
       aes(x = Exp_religious_freedom)) +
  geom_histogram(color = "black",
                 bins = 20)+
  xlim(1, 7)+
  geom_textvline(label = "Mean = 5.8", 
                 xintercept = 5.8, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(y = "Frequency",
       x = "Exp_religious_freedom score", 
       title = "Experience of Relgious Freedom")+
  theme_bw()


ggplot(data = ds, 
       aes(x = Exp_religious_freedom)) +
  geom_histogram(color = "black",
                 bins = 20)+
  xlim(1, 7)+
  labs(y = "Frequency",
       x = "Exp_religious_freedom score", 
       title = "Experience of Religious Freedom")+
  facet_wrap( ~ Country, nrow = 2) +
  theme_bw()

df01 <- ds %>% drop_na(Exp_religious_freedom)

tbl01 <- aggregate(df01$Exp_religious_freedom, 
                    by=list(df01$Country),
                    FUN=mean)
tbl01$Country <- tbl01$Group.1
tbl01$Exp_religious_freedom <- tbl01$x
tbl01 <- tbl01[, 3:4]
tbl01

ggplot(data = df01, 
       aes(x = Exp_religious_freedom, 
           y = Country)) +
  geom_boxplot(color = "black",
               fill = "grey")+
  xlim(1, 7)+
  geom_textvline(label = "Mean = 5.8", 
                 xintercept = 5.8, 
                 vjust = 1.1, 
                 lwd = 1.05, 
                 linetype = 2)+
  labs(y = "",
       x = "Exp_religious_freedom score", 
       title = "Experience of Relgious Freedom")+
  theme_bw()



## ----error = F, message = F, warning = F----------------------------------------

mp01 <- modelplot(m01,
                  coef_omit = 'SD (Observations)',
                  coef_rename = TRUE) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue", "black"))+
#  xlim(-3, 3)+
  labs(title = "Social perception of religious freedom", 
       x = "Coefficient estimates with95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp01


mp02 <- modelplot(m02,
                  coef_omit = 'SD (Observations)',
                  coef_rename = TRUE) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue", "black"))+
#  xlim(-3, 3)+
  labs(title = "Social perception of religious freedom", 
       x = "Coefficient estimates with95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp02

mp01a <- mp01 + theme(legend.position = "none")
ol2 <- mp01a / mp02
ol2



## ----error = F, message = F, warning = F----------------------------------------

mp11 <- modelplot(m11,
                  coef_omit = 'SD (Observations)',
                  coef_rename = TRUE) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue", "black"))+
#  xlim(-3, 3)+
  labs(title = "Experience of religious freedom", 
       x = "Coefficient estimates with95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp11


mp12 <- modelplot(m12,
                  coef_omit = 'SD (Observations)',
                  coef_rename = TRUE) +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue", "black"))+
#  xlim(-3, 3)+
  labs(title = "Experience of religious freedom", 
       x = "Coefficient estimates with95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp12

mp11a <- mp11 + theme(legend.position = "none")
ol2a <- mp11a / mp12
ol2a


