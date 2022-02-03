library (dslabs)
library(lubridate)
data ("brexit_polls")
head (brexit_polls)
sum(month(brexit_polls$startdate) == 4)
round_date(brexit_polls$enddate, unit="week")

print(table (weekdays(brexit_polls$enddate))
      
table(weekdays(brexit_polls$enddate))

data("movielens")
head (movielens)
x<-(as_datetime(movielens$timestamp))
yx<-year(x)
hx<-hour(x)

table(yx)
table (hx)

dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
names(which.max(reviews_by_year))    # name of year with most reviews
reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews
---------------------------------------------------------------------
library(tidyverse)
library(dplyr)
library(gutenbergr)
library(tidytext)
options(digits = 3)

gutenberg_metadata

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))

gutenberg_works(title == "Pride and Prejudice", languages = "en")
gutenberg_works(title == "Pride and Prejudice")$gutenberg_id
----------------------------------------------------------------
 pd <-gutenberg_download(gutenberg_id = 1342)
  
orgullo<- data_frame(pd)
orgullo %>% unnest_tokens(word, text)


book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)

words %>% filter(!word %in% stop_words$word ) %>% nrow

words <- words %>% anti_join(stop_words)
nrow(words)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

wd_nw <- words %>% filter(!word %in% stop_words$word)
wd_nn <-filter(wd_nw$word, !str_detect(wd_nw$word, "\\d"))
nrow(wd_nn)
anti_join(wd_nn, words)
-----------------------------
  top50<-words%>%
  count(word) %>%
  top_n(50, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))  
-----------------------------
  afinn <- get_sentiments("afinn")
afinn_sentiments <- inner_join(words, afinn)
afinn_sentiments%>% filter(value == 4)
mean (afinn_sentiments$value >0 )