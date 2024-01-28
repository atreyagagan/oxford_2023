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
               corrplot, tm, readxl, patchwork, modelsummary)

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
           "event_transformative_indiv", "event_transformative_group")

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
#  geom_textvline(label = "Mean = 37.00", 
#                 xintercept = 37.00, 
#                 vjust = 1.1, 
#                 lwd = 1.05, 
#                 linetype = 2)+
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

# ds %>% 
#   drop_na(ses) %>%
#   group_by(ses, Country) %>% 
#   summarise(count = n()) %>% 
#   ggplot(aes(ses, count)) + 
#   geom_segment(aes(x=ses, 
#                    xend=ses, 
#                    y=0, 
#                    yend=count))+ 
#   geom_point()+
#   labs(x = "", 
#        y = "Frequency", 
#        title = "Socioeconomic status by country")+
#     facet_wrap(vars(Country), nrow = 2)+
#   coord_flip()+
#   theme_bw()
# 



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

# ds %>% 
#   drop_na(jobnature) %>%
#   group_by(jobnature, Country) %>% 
#   summarise(count = n()) %>% 
#   ggplot(aes(jobnature, count)) + 
#   geom_segment(aes(x=jobnature, 
#                    xend=jobnature, 
#                    y=0, 
#                    yend=count))+ 
#   geom_point()+
#   labs(x = "", 
#        y = "Frequency", 
#        title = "Nature of employment by country")+
#     facet_wrap(vars(Country), nrow = 2)+
#   coord_flip()+
#   theme_bw()
# 



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

# ds %>% 
#  drop_na(religion) %>%
#  group_by(religion, Country) %>% 
#  summarise(count = n()) %>% 
#  ggplot(aes(religion, count)) + 
#  geom_segment(aes(x=religion, 
#                   xend=religion, 
#                   y=0, 
#                   yend=count))+ 
#  geom_point()+
#  labs(x = "", 
#       y = "Frequency", 
#       title = "Religious distribution by country")+
#    facet_wrap(vars(Country), nrow = 2)+
#  coord_flip()+
#  theme_bw()




## ----error = F, message = F, warning = F----------------------------------------

ds$married <- ifelse(ds$married == "Not married", "Unmarried", ds$married)
ds$married <- ifelse(ds$married == "", NA, ds$married)

table(ds$married)

## Marital status (full sample):

# ds %>% 
#   drop_na(married) %>%
#   group_by(married) %>% 
#   summarise(count = n()) %>% 
#   ggplot(aes(married, count)) + 
#   geom_segment(aes(x=married, xend=married, 
#                    y=0, yend=count))+ 
#   geom_point()+
#   labs(x = "", 
#        y = "Frequency", 
#        title = "Marital status (full sample)")+
#   coord_flip()+
#   theme_bw()

## Marital status by country:
# ds %>% 
#   drop_na(married) %>%
#   group_by(married, Country) %>% 
#   summarise(count = n()) %>% 
#   ggplot(aes(married, count)) + 
#   geom_segment(aes(x=married, xend=married, 
#                    y=0, yend=count))+ 
#   geom_point()+
#   labs(x = "", 
#        y = "Frequency", 
#        title = "Marital status by country")+
#     facet_wrap(vars(Country), nrow = 2)+
#   coord_flip()+
#   theme_bw()



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

## Create proper variables to use in regression models:

ds$BCL <- ds$bcl
ds$BBL <- ds$bbl
ds$Ingroup_fusion <- ds$igfusion
ds$Ingroup_identification <- ds$igidentification
ds$Outgroup_bonds <- ds$ogbonds
ds$Age <- ds$age
ds$`Gender: ` <- factor(ds$gender,
                     levels = c("Male", "Female"))
ds$`SES: ` <- factor(ds$ses,
                     levels = c("Lower middle", "Middle",
                                "Upper middle", "Upper"))
ds$Empathic_concern <- ds$empathic_concern
ds$Perspective_taking <- ds$perspective_taking
ds$Perceived_discrimination <- ds$history_discrimination
ds$OG_hostility <- ds$og_hostility
ds$OG_cooperation <- ds$og_cooperation
ds$Fight_OG <- ds$fight_outgroup


## ----error = F, message = F, warning = F----------------------------------------

## Two regression models predicting endorsement of BBL vs BCL:

lm01 <- lm(BBL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
           Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
           data = ds)

lm02 <- lm(BCL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
           Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
           data = ds)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm01, lm02, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F----------------------------------------

dsgmb <- ds[ds$Country=="Gambia",]
dspak <- ds[ds$Country=="Pakistan",]
dstza <- ds[ds$Country=="Tanzania",]
dsuga <- ds[ds$Country=="Uganda",]

## Eight regression models predicting endorsement of BBL vs BCL for four countries

lm_gmb_01 <- lm(BBL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `,
                data = dsgmb)

lm_gmb_02 <- lm(BCL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsgmb)

lm_pak_01 <- lm(BBL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `,
                data = dspak)

lm_pak_02 <- lm(BCL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dspak)

lm_tza_01 <- lm(BBL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `,
                data = dstza)

lm_tza_02 <- lm(BCL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dstza)

lm_uga_01 <- lm(BBL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `,
                data = dsuga)

lm_uga_02 <- lm(BCL~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsuga)



## ----error = F, message = F, warning = F, results = "asis"----------------------
stargazer(lm_gmb_01, lm_gmb_02, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_pak_01, lm_pak_02, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)


## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_tza_01, lm_tza_02, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_uga_01, lm_uga_02, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

mp09 <- modelplot(lm01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Endorsement of BBL", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp10 <- modelplot(lm02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Endorsement of BCL", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp09 

mp10



## ----error = F, message = F, warning = F----------------------------------------

mp01 <- modelplot(lm_gmb_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BBL: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp02 <- modelplot(lm_gmb_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BCL: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp03 <- modelplot(lm_pak_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BBL: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp04 <- modelplot(lm_pak_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BCL: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp05 <- modelplot(lm_tza_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BBL: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp06 <- modelplot(lm_tza_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BCL: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp07 <- modelplot(lm_uga_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BBL: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp08 <- modelplot(lm_uga_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "BCL: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mpgam <- (mp01 / mp02)
mppak <- (mp03 / mp04)
mptza <- (mp05 / mp06)
mpuga <- (mp07 / mp08)

mpgam 

mppak 

mptza 
  
mpuga



## ----error = F, message = F, warning = F----------------------------------------

## Three regression models predicting OG related variables:

lm01 <- lm(OG_hostility~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
           Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
           data = ds)

lm02 <- lm(OG_cooperation~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
           Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
           data = ds)

lm03 <- lm(Fight_OG~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
           Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
           data = ds)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm01, lm02, lm03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F----------------------------------------

## Twelve regression models predicting endorsement of BBL vs BCL for four countries

lm_gmb_01 <- lm(OG_hostility~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsgmb)

lm_gmb_02 <- lm(OG_cooperation~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsgmb)

lm_gmb_03 <- lm(Fight_OG~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsgmb)

lm_pak_01 <- lm(OG_hostility~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dspak)

lm_pak_02 <- lm(OG_cooperation~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dspak)

lm_pak_03 <- lm(Fight_OG~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dspak)

lm_tza_01 <- lm(OG_hostility~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dstza)

lm_tza_02 <- lm(OG_cooperation~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dstza)

lm_tza_03 <- lm(Fight_OG~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dstza)

lm_uga_01 <- lm(OG_hostility~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsuga)

lm_uga_02 <- lm(OG_cooperation~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsuga)

lm_uga_03 <- lm(Fight_OG~Ingroup_fusion+Ingroup_identification+Outgroup_bonds+Empathic_concern+
                Perspective_taking+Perceived_discrimination+Age+`Gender: `+`SES: `, 
                data = dsuga)




## ----error = F, message = F, warning = F, results = "asis"----------------------
stargazer(lm_gmb_01, lm_gmb_02, lm_gmb_03, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_pak_01, lm_pak_02, lm_pak_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)


## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_tza_01, lm_tza_02, lm_tza_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_uga_01, lm_uga_02, lm_uga_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

mp09 <- modelplot(lm01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup hostility", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp10 <- modelplot(lm02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup cooperation", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp11a <- modelplot(lm03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Fight outgroup", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp09 

mp10

mp11a



## ----error = F, message = F, warning = F----------------------------------------

mp11 <- modelplot(lm_gmb_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup hostility: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp12 <- modelplot(lm_gmb_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup cooperation: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp13 <- modelplot(lm_gmb_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Fight outgroup: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp21 <- modelplot(lm_pak_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup hostility: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp22<- modelplot(lm_pak_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup cooperation: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp23 <- modelplot(lm_pak_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Fight outgroup: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp31 <- modelplot(lm_tza_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup hostility: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp32<- modelplot(lm_tza_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup cooperation: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp33 <- modelplot(lm_tza_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Fight outgroup: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp41 <- modelplot(lm_uga_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup hostility: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp42<- modelplot(lm_uga_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup cooperation: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp43 <- modelplot(lm_uga_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Fight outgroup: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)



## ----error = F, message = F, warning = F----------------------------------------

mp11 

mp12 

mp13



## ----error = F, message = F, warning = F----------------------------------------

mp21 

mp22

mp23




## ----error = F, message = F, warning = F----------------------------------------

mp31

mp32

mp33



## ----error = F, message = F, warning = F----------------------------------------

mp41

mp42

mp43



## ----error = F, message = F, warning = F----------------------------------------

## Three regression models predicting IG fusion/identification and OG bonds:

lm01 <- lm(Ingroup_fusion~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
           data = ds)

lm02 <- lm(Ingroup_identification~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
           data = ds)

lm03 <- lm(Outgroup_bonds~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
           data = ds)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm01, lm02, lm03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F----------------------------------------

## Twelve regression models predicting endorsement of BBL vs BCL for four countries

lm_gmb_01 <- lm(Ingroup_fusion~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsgmb)

lm_gmb_02 <- lm(Ingroup_identification~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsgmb)

lm_gmb_03 <- lm(Outgroup_bonds~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsgmb)

lm_pak_01 <- lm(Ingroup_fusion~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dspak)

lm_pak_02 <- lm(Ingroup_identification~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dspak)

lm_pak_03 <- lm(Outgroup_bonds~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dspak)

lm_tza_01 <- lm(Ingroup_fusion~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dstza)

lm_tza_02 <- lm(Ingroup_identification~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dstza)

lm_tza_03 <- lm(Outgroup_bonds~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dstza)

lm_uga_01 <- lm(Ingroup_fusion~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsuga)

lm_uga_02 <- lm(Ingroup_identification~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsuga)

lm_uga_03 <- lm(Outgroup_bonds~Event_positive_affect+Event_negative_affect+Event_episodic_recall+
             Event_shared_perception+Event_reflection+Event_transformative_indiv+
             Event_transformative_group+Age+`Gender: `+`SES: `, 
             data = dsuga)



## ----error = F, message = F, warning = F, results = "asis"----------------------
stargazer(lm_gmb_01, lm_gmb_02, lm_gmb_03, 
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_pak_01, lm_pak_02, lm_pak_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)


## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_tza_01, lm_tza_02, lm_tza_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

stargazer(lm_uga_01, lm_uga_02, lm_uga_03,
          type = "html", 
          star.cutoffs = c(0.05, 0.01, 0.001),
          out = "table1.html",
          notes = starpattern, 
          notes.append = F)



## ----error = F, message = F, warning = F, results = "asis"----------------------

mp09 <- modelplot(lm01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup fusion", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp10 <- modelplot(lm02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup identification", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp11a <- modelplot(lm03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup bonds", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp09 

mp10

mp11a



## ----error = F, message = F, warning = F----------------------------------------

mp11 <- modelplot(lm_gmb_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup fusion: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp12 <- modelplot(lm_gmb_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup identification: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp13 <- modelplot(lm_gmb_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup bonds: Gambia", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp21 <- modelplot(lm_pak_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup fusion: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp22<- modelplot(lm_pak_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup identification: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp23 <- modelplot(lm_pak_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup bonds: Pakistan", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)


mp31 <- modelplot(lm_tza_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup fusion: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp32<- modelplot(lm_tza_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup identification: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp33 <- modelplot(lm_tza_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup bonds: Tanzania", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp41 <- modelplot(lm_uga_01, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup fusion: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp42<- modelplot(lm_uga_02, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Ingroup identification: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)

mp43 <- modelplot(lm_uga_03, 
          coef_rename = TRUE,
          coef_omit = 'Interc') +
  aes(color = ifelse(p.value < 0.05, "Significant", "Not significant")) +
  scale_color_manual(values = c("red", "blue"))+
  labs(title = "Outgroup bonds: Uganda", 
       x = "OLS coefficients with 95% CI")+
  geom_vline(xintercept = 0, 
             linetype = 2)



## ----error = F, message = F, warning = F----------------------------------------

mp11 

mp12 

mp13



## ----error = F, message = F, warning = F----------------------------------------

mp21 

mp22

mp23




## ----error = F, message = F, warning = F----------------------------------------

mp31

mp32

mp33



## ----error = F, message = F, warning = F----------------------------------------

mp41

mp42

mp43

## write file to hard drive:
#fwrite(ds, file = "~/Desktop/oxford/data/cleanedds/combinedds.csv")


