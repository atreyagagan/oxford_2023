
## Gambia:

summary(dsgmb$IGF04)

p04 <- dsgmb %>% drop_na(IGF04)%>%
ggplot(aes(x = IGF04))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dsgmb$IGF03)

p03 <- dsgmb %>% drop_na(IGF03)%>%
ggplot(aes(x = IGF03))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

summary(dsgmb$IGF02)

p02 <- dsgmb %>% drop_na(IGF02)%>%
ggplot(aes(x = IGF02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dsgmb$IGF01)

p01 <- dsgmb %>% drop_na(IGF01)%>%
ggplot(aes(x = IGF01))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

pgmb <- (p01 | p02) / (p03 | p04) + plot_annotation("Gambia")

pgmb

## Pakistan:


summary(dspak$IGF04)

p04 <- dspak %>% drop_na(IGF04)%>%
ggplot(aes(x = IGF04))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dspak$IGF03)

p03 <- dspak %>% drop_na(IGF03)%>%
ggplot(aes(x = IGF03))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

summary(dspak$IGF02)

p02 <- dspak %>% drop_na(IGF02)%>%
ggplot(aes(x = IGF02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dspak$IGF01)

p01 <- dspak %>% drop_na(IGF01)%>%
ggplot(aes(x = IGF01))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

ppak <- (p01 | p02) / (p03 | p04) + plot_annotation("Pakistan")

ppak

## Tanzania:

summary(dstza$IGF04)

p04 <- dstza %>% drop_na(IGF04)%>%
ggplot(aes(x = IGF04))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
  #coord_flip()+
  theme_bw()


summary(dstza$IGF03)

p03 <- dstza %>% drop_na(IGF03)%>%
ggplot(aes(x = IGF03))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
  #coord_flip()+
  theme_bw()

summary(dstza$IGF02)

p02 <- dstza %>% drop_na(IGF02)%>%
ggplot(aes(x = IGF02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
  #coord_flip()+
  theme_bw()


summary(dstza$IGF01)

p01 <- dstza %>% drop_na(IGF01)%>%
ggplot(aes(x = IGF01))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
  #coord_flip()+
  theme_bw()

ptza <- (p01 | p02) / (p03 | p04) + plot_annotation("Tanzania")


ptza

## Uganda:


summary(dsuga$IGF04)

p04 <- dsuga %>% drop_na(IGF04)%>%
ggplot(aes(x = IGF04))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dsuga$IGF03)

p03 <- dsuga %>% drop_na(IGF03)%>%
ggplot(aes(x = IGF03))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

summary(dsuga$IGF02)

p02 <- dsuga %>% drop_na(IGF02)%>%
ggplot(aes(x = IGF02))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()


summary(dsuga$IGF01)

p01 <- dsuga %>% drop_na(IGF01)%>%
ggplot(aes(x = IGF01))+
  geom_histogram(color = "black",
                 fill = "gray",
                 bins = 7)+
 # coord_flip()+
  theme_bw()

puga <- (p01 | p02) / (p03 | p04) + plot_annotation("Uganda")

pgmb 
ppak 
ptza  
puga



