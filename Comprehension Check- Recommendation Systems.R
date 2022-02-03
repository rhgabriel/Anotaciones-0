#The following exercises all work with the movielens data, which can be loaded using the following code:
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")
---------------------------------------------------------------------------------
#Q1
#Compute the number of ratings for each movie and then plot it against the year
#the movie came out using a boxplot for each year. Use the square root
#transformation on the y-axis (number of ratings) when creating your plot.
#What year has the highest median number of ratings?
  movielens %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
--------------------------------------------------------------------------------
#Q2
#We see that, on average, movies that came out after 1993 get more ratings. 
#We also see that with newer movies, starting in 1993, the number of ratings decreases
#with year: the more recent a movie is, the less time users have had to rate it.
  
#Among movies that came out in 1993 or later, select the top 25 movies with the
#highest average number of ratings per year (n/year), and caculate the average rating 
#of each of them. To calculate number of ratings per year, use 2018 as the end year.
#What is the average rating for the movie The Shawshank Redemption?

  movielens %>% filter(title=="Shawshank Redemption, The") %>%
  summarize(mean_rating = mean(rating))

#What is the average number of ratings per year for the movie Forrest Gump?

movielens %>% filter(title=="Forrest Gump") %>%
  summarize(n_ratings_per_year = n()/(2018-1994))

#respuesta del problema 
movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate))
--------------------------------------------------------------------------------
  #Q3
#From the table constructed in Q2, we can see that the most frequently rated
#movies tend to have above average ratings. This is not surprising: more people
#watch popular movies. To confirm this, stratify the post-1993 movies by ratings
#per year and compute their average ratings.
#To calculate number of ratings per year, use 2018 as the end year. Make a plot
#of average rating versus ratings per year and show an estimate of the trend.
#What type of trend do you observe?
  
  movielens %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating))+
  geom_point()+
  geom_smooth()
--------------------------------------------------------------------------------
  #Q4
#Suppose you are doing a predictive analysis in which you need to fill in the
#missing ratings with some value.
#Given your observations in the exercise in Q3, which of the following strategies
#would be most appropriate?
  
#Because a lack of ratings is associated with lower ratings, it would be most
#appropriate to fill in the missing value with a lower value than the average.
#You should try out different values to fill in the missing value and evaluate
#prediction in a test set.
--------------------------------------------------------------------------------
#Q5
#The movielens dataset also includes a time stamp.
#This variable represents the time and data in which the rating was provided.
#The units are seconds since January 1, 1970. Create a new column date with the date.
#Which code correctly creates this new column?
  
  movielens <- mutate(movielens, date = as_datetime(timestamp))
--------------------------------------------------------------------------------
#Q6
#Compute the average rating for each week and plot this average against date.
#Hint: use the round_date() function before you group_by().
#What type of trend do you observe?

  movielens %>% mutate(date = round_date(date, unit = "week")) %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
#There is some evidence of a time effect on average rating.
--------------------------------------------------------------------------------
#Q8
#The movielens data also has a genres column. This column includes every genre
#that applies to the movie. Some movies fall under several genres. Define a 
#category as whatever combination appears in this column. Keep only categories 
#with more than 1,000 ratings. Then compute the average and standard error for 
#each category. Plot these as error bar plots. Which genre has the lowest average rating?
  
  movielens %>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

  