library(dslabs)
library (tidyverse)
library(broom)
data("research_funding_rates")
research_funding_rates

#Construct a two-by-two table of gender (men/women) by award status (awarded/not) 
#using the total numbers across all disciplines.
#What is the number of men not awarded?

men_awards <- research_funding_rates %>% 
  select(applications_men, awards_men, awards_total)
tidy(men_awards)

sum(men_awards$applications_men)- sum(men_awards$awards_men)

women_awards <- research_funding_rates %>% 
  select(applications_women, awards_women, awards_total)
tidy(women_awards)

sum(women_awards$applications_women)- sum(women_awards$awards_women)

#la respuesta mostraba hacerlo así:
two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

#Use the two-by-two table from Question 1 to compute the percentages of men awarded versus women awarded.
#What is the percentage of men awarded?
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

#Run a chi-squared test on the two-by-two table to determine whether the difference in the two success rates
#is significant. (You can use tidy() to turn the output of chisq.test() into a data frame as well.)
#What is the p-value of the difference in funding rate?

two_by_two %>% select(-awarded) %>% chisq.test() %>% tidy() %>% pull(p.value)

#There may be an association between gender and funding.
#But can we infer causation here? Is gender bias causing this observed difference?
#The response to the original paper claims that what we see here is similar to the UC Berkeley admissions example.
#Specifically they state that this 
#"could be a prime example of Simpson’s paradox; if a higher percentage of women apply for grants"
#"in more competitive scientific disciplines, then an analysis across all disciplines could incorrectly"
#"show 'evidence' of gender inequality."

#To settle this dispute, use this dataset with number of applications, awards, and success rate for each gender:

dat <- research_funding_rates %>% 
  mutate(discipline = reorder(discipline, success_rates_total)) %>%
  rename(success_total = success_rates_total,
         success_men = success_rates_men,
         success_women = success_rates_women) %>%
  gather(key, value, -discipline) %>%
  separate(key, c("type", "gender")) %>%
  spread(type, value) %>%
  filter(gender != "total")
dat

#To check if this is a case of Simpson's paradox, plot the success rates versus disciplines,
#which have been ordered by overall success, with colors to denote the genders and size to
#denote the number of applications.

dat %>% ggplot(aes(discipline,awards/success, col=gender, size=applications))+
  geom_point()


dat %>% 
  ggplot(aes(discipline, success, size = applications, color = gender)) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()









