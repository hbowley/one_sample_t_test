library(tidyverse)
library(datarium)
library(tidymodels)

head(mice2)

## calculate the observed statistic

observed_statistic <- mice2 %>% 
  mutate(difference = after - before) %>% 
  specify(response = difference) %>% 
  calculate(stat = "mean")

## generate the null distribution and test statistic

null_dist_1_sample <- mice2 %>% 
  mutate(difference = after - before) %>% 
  specify(response = difference) %>% 
  hypothesize(null = "point", mu = 0) %>% 
  calculate(stat = "mean") %>% 
  visualize() +
  shade_p_value(observed_statistic, direction = "two-sided")

null_dist_1_sample

p_value_1_sample <- null_dist_1_sample %>% 
  get_p_value(
    obs_stat = observed_statistic,
    direction = "two-sided"
    )

p_value_1_sample



null_dist_1_sample <- mice2 %>% 
  mutate(difference = after - before) %>% 
  t_test(response = difference, mu = 0)

null_dist_1_sample <- mice2 %>% 
  mutate(difference = after - before) %>% 
  specify(response = difference) %>% 
  hypothesize(null = "point", mu = 0) %>% 
  calculate(stat = "t") %>% 
  dplyr::pull()

pt(null_dist_1_sample, df = nrow(mice2)-1, lower.tail = FALSE)*2

null_dist_1_sample
## base R

one_sample <- t.test(mice2$before, mice2$after, paired = TRUE)

one_sample


# base r version ----------------------------------------------------------

#http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
