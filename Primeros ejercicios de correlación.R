install.packages("Lahman")
library(Lahman)
library(dplyr)
library(ggplot2)
Teams %>% filter(yearID%in% 1961:2001) %>%
  ggplot(aes(R/G, AB/G))+
  geom_point (alpha=0.5)

Teams %>% filter(yearID%in% 1961:2001) %>%
  ggplot(aes(R/G, E/G))+
  geom_point (alpha=0.5)
  
Teams %>% filter(yearID%in% 1961:2001) %>%
  ggplot(aes(X3B/G, X2B/G))+
  geom_point (alpha=0.5)
-------------------------------
primer_paso <-Teams %>% filter(yearID%in% 1961:2001) %>%
  runs<-as.numeric(primer_paso$R)
game<-as.numeric(primer_paso$G)
at_bat<- as.numeric(primer_paso$AB)
miux <-mean(runs/game)
sigmax <-sd(runs/game)
miuy<-mean(at_bat/game)
sigmay<-sd(at_bat/game)
coefx<-((runs/game)-miux/sigmax)
coefy<- ((at_bat/game)-miuy/sigmay)
cc_rg_ab<- sum(coefx*coefy)
#tengo que revisar bien la formula de correlación para hacerlo de la forma larga.
cor(runs/game, at_bat/game)
cor(primer_paso$W/primer_paso$G, primer_paso$E/primer_paso$G)
cor(primer_paso$X2B/primer_paso$G, primer_paso$X3B/primer_paso$G)
