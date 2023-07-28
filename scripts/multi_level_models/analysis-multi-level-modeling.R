

## Multi-level modelling

# load libraries:
pacman::p_load(data.table, tidyverse, haven, labelled, vtable, 
               psych, scales, weights, clipr, forcats,
               stargazer, ggthemes, ggcharts, geomtextpath,
               corrplot, tm, gt, lme4, lmerTest, car, 
               ggeffects, magrittr, broom, broom.mixed)

# load example data:
load("/home/gagan/Desktop/Intro-multilevel-with-R/Data/multilevel_data.rda")

# Join with more school-level variables (sector)
df <- left_join(stud_data, school_data, by="school")

ds$country <- ds$Country

# Plot BCL endorsement vs IG Fusion score in each of the 8 countries
p01 <- ggplot(ds, aes(x = IG_Fusion, 
                    y = Endorse_BCL)) +
    geom_point(shape = 1) + 
    geom_smooth(method ="loess",
                color = "blue", 
                se=FALSE) + 
    geom_smooth(method = "lm", 
                color = "black", 
                se = TRUE) + 
    facet_wrap(~ country, nrow = 4) + 
  theme_bw() 
 
# See the plot
p01


## Create nested dataframe:

nested.df <- df %>% 
  group_by(school) %>%
  nest()

nested.df

nested.ds <- ds %>% 
  group_by(country) %>%
  nest()

nested.ds

class(nested.df$data)

class(nested.ds$data)

# For example, look at the data for school 1224:

## Still nested
nested.df %>%
  filter(school=="1224") %>%
  dplyr::select(school, data) 

nested.ds %>%
  filter(country == "USA") %>%
  dplyr::select(country, data) 

## Unnested
nested.df %>%
  filter(school=="1224") %>%
  dplyr::select(school, data) %>%
  unnest(cols = c(data))

nested.ds %>%
  filter(country == "USA") %>%
  dplyr::select(country, data) %>%
  unnest(cols = c(data))

lmodels <- nested.ds %>%
  pull(data) %>%
  purrr::map(~ lm(Endorse_BCL ~ IG_Fusion, data= .x))

# lmodels is now a list of fitted linear models, one for each country

head(lmodels)




