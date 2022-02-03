library(HistData)
library(tidyverse)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}
beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() +
  geom_line(aes(beta1, rss))
-----------------------------------------------------------------------
  B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse %>% ggplot(aes(beta_0, beta_1))+geom_point()+geom_point(aes(beta_0,beta_1))
------------------------------------------------------------------------------
  model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

--------------------------------------------------------------
  galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")
-------------------------------------------
  library(Lahman)
library(broom)
Teams_small <- Teams %>% filter(yearID %in% 1961:2001)
Teams_small %>% 
  mutate(R_per_game = R/G, BB_per_game = BB/G, HR_per_game = HR/G) %>% 
  do(tidy(lm(R_per_game ~ BB_per_game + HR_per_game, data = .)))
--------------------------------------------
 
library(HistData)
data("GaltonFamilies")
 set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
options(digits = 3)    # report 3 significant digits
female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
model1<- lm(mother ~ daughter, data = female_heights)
predictions1 <- predict(model1, interval = c("confidence"), level = 0.95)
-------------------------------------------------------------
  library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_01 <-Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb, stint)


player_groups <- bat_01 %>% 
  group_by(playerID) %>%
  mutate(mean_singles = mean(singles)/stint, mean_bb = mean(bb)/stint) 

player_groups_s<- player_groups %>% filter(mean_singles>0.2)
player_groups_bb<-player_groups %>% filter(mean_bb>0.2)

player_groups_bb$playerID[!duplicated(player_groups_bb$playerID)]
player_groups_s$playerID[!duplicated(player_groups_s$playerID)]

#o lo puedes hacer así
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
sum(bat_99_01$mean_singles > 0.2)
sum(bat_99_01$mean_bb > 0.2)
-----
  bat_2 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_1 <-Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb) 
  
 bat_99_02<- inner_join(bat_2,bat_99_01, by="playerID") 
corsin <-cor(bat_99_02$singles, bat_99_02$mean_singles)
corbb<- cor(bat_99_02$bb, bat_99_02$mean_bb)
bat_99_02 %>% 
  ggplot(aes(mean_singles, singles)) + 
  geom_point(alpha=0.5)
bat_99_02 %>% 
  ggplot(aes(mean_bb, bb)) + 
  geom_point(alpha=0.5)

lm_s<- lm(singles~mean_singles, data=bat_99_02)
lm_b<- lm(bb~mean_bb, data=bat_99_02)                 
