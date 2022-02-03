library(Lahman)
library(broom)
library(tidyverse)

dat <- Teams %>% filter(yearID %in% 1971) %>%
  select(HR, BB, R) 

dat %>% 
  do(tidy(lm(R ~ HR + BB, data = dat), conf.int = T))

#Interpret the p-values for the estimates using a cutoff of 0.05.
# The p-value for HR is less than 0.05, but the p-value of BB is greater than 0.05 (0.06),
#so the evidence is not strong enough to suggest that BB has a significant effect on runs at a p-value cutoff of 0.05.
------------------------------------
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")
------------------------------------------
  res2 <- res %>% 
  filter(term =="BB") %>% 
  do(tidy(lm(estimate ~ yearID, data = .))) %>%
  ungroup()

res %>%
  filter(term == "BB") %>%
  lm(estimate ~ yearID, data = .) %>%
  tidy() %>%
  filter(term == "yearID") %>%
  pull(estimate)