library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
set.seed(1989)
female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

mu_x<-mean(female_heights$mother)
s_x <- sd(female_heights$mother)
mu_y<-mean(female_heights$daughter)
s_y<-sd(female_heights$daughter)
r<-cor(female_heights$daughter, female_heights$mother)
m<-r*s_y/s_x
b<-mu_x-m*mu_y
b3<-mu_y-m*mu_x
y1<-b+m
y2<-b+m+m
m2<-y2-y1
v<-(r^2)

mu_y + r*(60 - mu_x)/s_x*s_y
