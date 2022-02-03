library(Lahman)
library(broom)
library(tidyverse)

dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = HR/G,
         R = R/G) %>%
  select(lgID, HR, BB, R) 

dat %>% 
  group_by(lgID) %>% 
  do(tidy(lm(R ~ HR, data = .), conf.int = T)) %>% 
  filter(term == "HR") 
--------------------------------------------------
  library(HistData)
set.seed(1)
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton %>%
  group_by(pair) %>%
  summarize(n = n())

galtonf_s<-galton %>% filter(pair == "father_son")
galtonf_s<-as.data.frame(galtonf_s)
cor(galtonf_s$parentHeight, galtonf_s$childHeight, use = "complete.obs")

galtonf_d<-galton %>% filter(pair == "father_daughter") 
galtonf_d<-as.data.frame(galtonf_d)
cor(galtonf_d$parentHeight, galtonf_d$childHeight, use = "complete.obs")

galtonm_s<-galton %>% filter(pair == "mother_son")
galtonm_s<-as.data.frame(galtonm_s)
cor(galtonm_s$parentHeight, galtonm_s$childHeight, use = "complete.obs")

galtonm_d<-galton %>% filter(pair == "mother_daughter")
galtonm_d<-as.data.frame(galtonm_d)
cor(galtonm_d$parentHeight, galtonm_d$childHeight, use = "complete.obs")
#o también se puede hacer así:
galton %>%
  group_by(pair) %>%
  summarize(cor = cor(parentHeight, childHeight)) %>%
  filter(cor == min(cor))
--------------------------------------------------------------
galton %>% 
  group_by(pair) %>% 
  do(tidy(lm(parentHeight~childHeight, data = .), conf.int=TRUE)) %>% 
  view()


galton %>%
  group_by(pair) %>%
  do(tidy(lm(childHeight ~ parentHeight, data = .), conf.int = TRUE)) %>%
  filter(term == "parentHeight" & p.value < .05) %>% 
  ggplot(aes(galton))
