
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
dsgmb <- fread("~/Desktop/oxford/data/cleands/dsgmb.csv")
dsgmb$Country <- "Gambia"
dsgmb$id <- 1:nrow(dsgmb)
dsgmb$ID <- paste0("GMB",dsgmb$id)
dsgmb$id  <- dsgmb$ID
dsgmb$country <- dsgmb$Country
dsgmb$ethnicity <- dsgmb$Ethnicity
dsgmb$event_reflection_01 <- dsgmb$event_event_reflection_01
dsgmb$event_reflection_02 <- dsgmb$event_event_reflection_02
dsgmb$imagistic_prompt <- dsgmb$`Q8D. Description`
sum(is.na(dsgmb$imagistic_prompt))


dspak <- fread("~/Desktop/oxford/data/cleands/dspak.csv")
dspak$Country <- "Pakistan"
dspak$id <- 1:nrow(dspak)
dspak$ID <- paste0("PAK",dspak$id)
dspak$ethnicity <- dspak$Ethnicity
dspak$event_reflection_01 <- dspak$event_event_reflection_01
dspak$event_reflection_02 <- dspak$event_event_reflection_02
dspak$id  <- dspak$ID
dspak$country <- dspak$Country
dspak$imagistic_prompt <- dspak$Q8_3
sum(is.na(dspak$imagistic_prompt))


dstza <- fread("~/Desktop/oxford/data/cleands/dstza.csv")
dstza$Country <- "Tanzania"
dstza$id <- 1:nrow(dstza)
dstza$ID <- paste0("TZA",dstza$id)
dstza$ethnicity <- dstza$Ethnicity
dstza$event_reflection_01 <- dstza$event_event_reflection_01
dstza$event_reflection_02 <- dstza$event_event_reflection_02
dstza$id  <- dstza$ID
dstza$country <- dstza$Country
dstza$imagistic_prompt <- dstza$`Q8D. Description`
sum(is.na(dstza$imagistic_prompt))


dsuga <- fread("~/Desktop/oxford/data/cleands/dsuga.csv")
dsuga$Country <- "Uganda"
dsuga$id <- 1:nrow(dsuga)
dsuga$ID <- paste0("UGA",dsuga$id)
dsuga$ethnicity <- dsuga$Ethnicity
dsuga$event_reflection_01 <- dsuga$event_event_reflection_01
dsuga$event_reflection_02 <- dsuga$event_event_reflection_02
dsuga$id  <- dsuga$ID
dsuga$country <- dsuga$Country

dsuga$imagistic_prompt <- dsuga$`Q8D. Description`
sum(is.na(dsuga$imagistic_prompt))


## List of variables to retain from all datasets:
list1 <- c("id", "country", "age", "gender", 
           "ses", "fses", "religion", "married", 
           "education", "ethnicity", 
           "IGF01", "IGF02", "IGF03", "IGF04",
           "IGI01", "IGI02", "IGI03", "IGI04",
           "OGF01", "OGF02", "OGF03", "OGF04",
           "OGI01", "OGI02", "OGI03", "OGI04",
           "ENDBCL01", "ENDBCL02", "ENDBCL03", 
           "ENDBBL01", "ENDBBL02", "ENDBBL03",
           "empathic_concern_01", "empathic_concern_02", "empathic_concern_03",
           "perspective_taking_01", "perspective_taking_02", 
           "perspective_taking_03", "perspective_taking_04",
           "imagistic_prompt",
           "event_positive_affect", "event_negative_affect", 
           "event_episodic_recall_01", "event_episodic_recall_02", 
           "event_shared_perception_01", "event_shared_perception_02", 
           "event_reflection_01", "event_reflection_02", 
           "event_transformative_indiv_01", "event_transformative_indiv_02", 
           "event_transformative_group_01", "event_transformative_group_02",
           "religious_freedom_per_01", "religious_freedom_per_02", "religious_freedom_per_03", 
           "religious_freedom_per_04", "religious_freedom_per_05", "religious_freedom_per_06", 
           "religious_freedom_per_07", "religious_freedom_per_08", 
           "religious_freedom_exp_01", "religious_freedom_exp_02", "religious_freedom_exp_03", 
           "religious_freedom_exp_04", "religious_freedom_exp_05", "religious_freedom_exp_06",
           "history_discrimination", "freq_positive_contact", "freq_negative_contact",
           "fight_outgroup", "og_host_01", "og_host_02", "og_hostility", "og_coop_01",
           "og_coop_02", "og_cooperation", "NegativeToPositive", "HostileToFriendly", 
           "SuspiciousToTrusting", "ContemptToRespect", "ThreatenedToRelaxed", "og_affect")
           
## Subset datasets to only the columns in the above list:
dsgmb1 <- dsgmb[, ..list1]
dspak1 <- dspak[, ..list1]
dstza1 <- dstza[, ..list1]
dsuga1 <- dsuga[, ..list1]

## Merged dataset with needed columns only
ds <- rbind(dsgmb1, dspak1, dstza1, dsuga1)

 fwrite(dsgmb1, file = "~/Dropbox/2023_Gagan/Paper02/data/dsgmb.csv")
 fwrite(dspak1, file = "~/Dropbox/2023_Gagan/Paper02/data/dspak.csv")
 fwrite(dstza1, file = "~/Dropbox/2023_Gagan/Paper02/data/dstza.csv")
 fwrite(dsuga1, file = "~/Dropbox/2023_Gagan/Paper02/data/dsuga.csv")

 