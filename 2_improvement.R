library(tidyverse)
library(ggsci)
library(reshape2)

load("data/dataset_HP_AO3.RData")
harry <- harry %>% 
  filter(language == "English") %>% 
  filter(between(date_year, 2010, 2019))  # because AO3 was made public in November 2009

# ------------------------------------------------------------

#  analysis for improvement

# First, we can use two indexes for measuring improvement: (i) the absolute number of kudos
# and (ii) the ratio kudos/hits:

tot_kudos_hits <- harry_aggr %>%
  distinct(index, .keep_all = TRUE) %>%
  group_by(date_year) %>%
  summarise(sum_kudos= sum(kudos, na.rm = T), sum_hits= sum(hits, na.rm = T))

# The ratio kudos/hits is possibly a better index of improvement, but it needs
# to be weighted with the total number of hits, as it may favor stories with very
# few hits. To do so, we use:

# True Bayesian average (from http://www.ebc.cat/2015/01/05/how-to-rank-restaurants/)
# the score (S) of a story is calculated as:
# S = wK + (1 - w) * K_av
# where K is the index (ratio kudos/hits of the story), K_av is the average of the index
# in that year, and w is a "weighting" parameter, calculated as:
# w = H / (H + H_av), with H the number of hits for that story, and H_av is the
# average number of hits for that year.

harry_weighted <- harry_aggr %>%
  distinct(index, .keep_all = TRUE) %>%
  select(index, kudos, hits, date_year) %>%
  mutate(kudos = log1p(kudos)) %>% # we log-transform kudos and hits as they need to be normally distributed to apply TBA
  mutate(hits = log1p(hits)) %>%
  group_by(date_year) %>%
  mutate(kudos_av = mean(kudos, na.rm = TRUE)) %>%
  mutate(hits_av = mean(hits, na.rm = TRUE)) %>%
  mutate(kudos_hits_av = mean(kudos/hits, na.rm = TRUE)) %>%
  mutate(w = hits / (hits + hits_av)) %>%
  mutate(score = w * kudos/hits + (1 - w) * kudos_hits_av)


tot_kudos_hits %>%
  mutate(weighted = summarise(harry_weighted, sum_scores = mean(score, na.rm = T))$sum_scores) %>%
  mutate(ratio_kudos_hits = sum_kudos/sum_hits) %>%
  select(-sum_hits) %>%
  pivot_longer(-date_year) %>%
  ggplot(aes(x = date_year, y = value)) +
    geom_point() +
    geom_line() + 
    geom_smooth(color="black") +
    facet_wrap(vars(name), scales = "free") +
    scale_x_continuous(breaks= c(2010,2012,2014,2016,2018))+
    theme_bw() +
    labs(x = "Year", y = "" ) +
    ggsave("analysis/plots/2_1_improvement_total.pdf", width = 12, height = 5)

#
# PERCENTILES ANALYSIS
#

harry_weighted <- harry_weighted %>%  
  mutate(score_p = ntile(score, 100)) 

# 10 percentiles window

lower_percentile <- 1 
higher_percentile <- 10

harry_low <- harry_weighted %>%
  filter(between(score_p, lower_percentile, higher_percentile)) %>%
  group_by(date_year, score_p) %>%
  summarise(mean_s = mean(score, na.rm = TRUE))

lower_percentile <- 46
higher_percentile <- 55

harry_mid <- harry_weighted %>%
  filter(between(score_p, lower_percentile, higher_percentile)) %>%
  group_by(date_year, score_p) %>%
  summarise(mean_s = mean(score, na.rm = TRUE))

lower_percentile <- 91
higher_percentile <- 100

harry_high <- harry_weighted %>%
  filter(between(score_p, lower_percentile, higher_percentile)) %>%
  group_by(date_year, score_p) %>%
  summarise(mean_s = mean(score, na.rm = TRUE))

cumulative <- bind_rows(harry_high, harry_mid, harry_low) %>%
  add_column(Rank = as_factor(rep(c("High score (91-100th percentile)",
                                    "Mid score (46-55th percentile)",
                                    "Low score (1-10th percentile)"), each = 100)))

cumulative %>%
  ggplot(aes(x = date_year, y = mean_s, colour = Rank)) +
  geom_point() +
  scale_color_futurama() +
  geom_smooth(method = "lm") +
  theme_bw() +
  scale_x_continuous(breaks= c(2010,2012,2014,2016,2018)) +
  labs( y = "Average weighted scores", x = "Year") +
  ggsave("analysis/plots/2_2_improvement_percentiles.pdf", width = 8, height = 5)

## Code to run slope analysis at each percentile (with 10 percentiles windows) 
# adapted from 1_accumulation.R

get_slope <- function(my_distr){
  # prepare variables for linear model
  x <- my_distr$date_year
  y <- my_distr$mean_s
  # generate model
  test <- lm(y ~ x)
  # get slope
  return(test$coefficients[2])
}

score_slope <- numeric()
percentiles <- 1:91 # this depends on the size of the percentiles we use
for(my_percentile in percentiles){
  
  lower_percentile <- my_percentile 
  higher_percentile <- my_percentile+9 # this depends on the size of the percentiles we use
  
  harry_perc <- harry_weighted %>%
    filter(between(score_p, lower_percentile, higher_percentile)) %>%
    group_by(date_year, score_p) %>%
    summarise(mean_s = mean(score, na.rm = TRUE))
  
  # get slope
  score_slope[my_percentile] <- get_slope(harry_perc)
  
  print(my_percentile)
  
}

tibble(percentiles = 1:91, linear_model_slope = score_slope) %>%
  ggplot(aes(x = percentiles, y = linear_model_slope)) +
    geom_line(size = 1.5) +
    scale_color_futurama() +
    scale_x_continuous(breaks = seq(1,91,by=10)) +
    ylim(c(0, 0.015)) +
    theme_bw() +
    labs(y = "Slope of the trend between 2010 and 2019", x = "Lower bound of score's 10-percentile window") +
    ggsave("analysis/plots/2_3_improvement_all_percentiles.pdf", width = 6, height = 5)
