library(tidyverse)
library(broom)
library(Lahman)
Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G, RPG = R/G, HRPG= HR/G )

tidy(lm(avg_attendance ~ RPG  , data = Teams_small), conf.int = T)

tidy(lm( avg_attendance ~ HRPG, data = Teams_small), conf.int = T)

tidy(lm( avg_attendance ~ W, data = Teams_small), conf.int = T)

tidy(lm( avg_attendance ~ yearID, data = Teams_small), conf.int = T)

cor(x = Teams_small$W, y = Teams_small$RPG)

cor(x = Teams_small$W, y = Teams_small$HRPG)

strat_teams <- Teams_small %>%
  mutate(strat = round(W/10)) %>%
  group_by(strat) %>%
  filter(strat %in% 5:10)

strat_teams %>%  filter(strat == 8) %>% count()

# calculate slope of regression line after stratifying by R per game
strat_teams %>%  
  group_by(strat) %>%
  summarize(slope = cor(R/G, avg_attendance)*sd(avg_attendance)/sd(R/G))

# calculate slope of regression line after stratifying by HR per game
strat_teams %>%  
  group_by(strat) %>%
  summarize(slope = cor(HR/G, avg_attendance)*sd(avg_attendance)/sd(HR/G))
----------------------
  
  fit <- Teams_small %>%
  lm(avg_attendance ~ RPG + HRPG + W + yearID, data = .)

guess <- data.frame(
  RPG = 5,
  HRPG = 1.2,
  W = 80,
  yearID = 2002
)
predict(fit, guess)

guess2 <- data.frame(
  RPG = 5,
  HRPG = 1.2,
  W = 80,
  yearID = 1960
)
predict(fit, guess2)
------------------------------
  pred <- Teams %>% filter(yearID == 2002) %>% 
  mutate(avg_attendance = attendance/G, RPG = R/G, HRPG= HR/G ) %>% 
  mutate(pred_attendance = predict(fit, data.frame(RPG = RPG, HRPG = HRPG, W = W, yearID = yearID)))

cor(pred$attendance, pred$pred_attendance)

