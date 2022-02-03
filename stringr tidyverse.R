library (stringr)
library (tidyverse)


animals <- c("moose", "monkey", "meerkat", "mountain lion")

pattern<- "^mo*"
pattern <- "^mo?"
pattern<- "^mo+"
pattern <-"^moo*"

str_detect(animals, pattern)
----------------------------------------------------------------
schools <- c("U. Kentucky", "Univ New Hampshire", "Univ. of Massachusetts", "University Georgia",
             "U California","California State University")
final <- schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")
--------------------------------------------------------------------
  problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")
----------------------------------------------------------------------
  converted <- problems %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
converted[!index]
---------------------------------------------------
library(dslabs)
data("gapminder")
dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  dat <- gapminder %>% filter(region == "Middle Africa") %>% 
  mutate(country_short = recode(country, 
                                c("Central African Republic", "Congo, Dem. Rep.", "Equatorial Guinea"),
                                c("CAR", "DRC", "Eq. Guinea")))
dat
