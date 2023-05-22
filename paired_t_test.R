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

null_dist_1_sample_paired %>% 
  visualize() + 
  shade_p_value(observed_statistic,
                direction = "two-sided")








# base r version ----------------------------------------------------------

#http://www.sthda.com/english/wiki/paired-samples-t-test-in-r
