library(tidyverse)
library(lubridate)
library(ggsci)
library(reshape2)
library(RColorBrewer)

# We use here the tidy version (saved as RData object to save space)
# created with the script tidy_up_corpus.R. In the script tidy_up_corpus.R 
# the function read_and_clean() also exclude stories 
# with less than 10 words, and stories published in AO3 in the year 2020
load("data/dataset_HP_AO3.RData")

# check languages - 90% is English
# harry %>%
#   group_by(language) %>%
#   summarise(n = n_distinct(index)) %>%
#   mutate(prop = prop.table(n)) %>% 
#   arrange(desc(n))

# we exclude stories that are not in English, obtaining a total of N=196726 stories
harry <- harry %>% 
  filter(language == "English")

# Backdated stories --------------------------------------------------------------

# column "year_fix" will assign to backdated stories the backdated year, while not
# backdated ones will keep the year assigned based on "link". This is useful for
# queries on themes within the HP fandom beyond AO3, since stories were published
# outside AO3 before being imported and having articulated tags added to them.
# New reliable date range starting from 2000
harry <- harry %>%
  mutate(year_fix = year(date)) %>% 
  mutate(backdated = ifelse(date_year >= (year_fix + 1), TRUE, FALSE)) %>% 
  mutate(year_fix = ifelse(backdated==FALSE, date_year, year_fix))

# Aggregate synonyms ------------------------------------------------------
# remember this is a quite conservative estimate of accumulation, if we do not
# consider synonyms it would be higher.

# load the synonyms files
characters_syn <- read_csv("data/characters_synonyms.csv")
relationships_syn <- read_csv("data/relationships_synonyms.csv")
freeforms_syn <- read_csv("data/freeforms_synonyms.csv")
common_freeform_syn <- read_csv("data/common_freeforms_synonyms.csv")

merge_freeforms_syn <- bind_rows(freeforms_syn, common_freeform_syn)


# transform the synonyms files in a tidy format
char_dictionary <- characters_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)
ship_dictionary <- relationships_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)
free_dictionary <- merge_freeforms_syn %>%
  pivot_longer(-canonical_tag, names_to = "n_syn", values_to = "synonym", values_drop_na = TRUE)


# aggregate synonyms
harry_char <- harry %>%
  filter(tag_type=="characters") %>%
  left_join(char_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

harry_ships <- harry %>%
  filter(tag_type=="relationships") %>%
  left_join(ship_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))

harry_free <- harry %>%
  filter(tag_type=="freeforms") %>%
  left_join(free_dictionary, by = c("tag" ="synonym")) %>%
  mutate(canonical_tag = ifelse(is.na(canonical_tag), tag, canonical_tag))


# build dataset including aggregated synonyms (includes only character, relationship, and freeform tags)
harry_aggr <- bind_rows(harry_char, harry_ships, harry_free) 


# Optional: count the number of synonyms that have been aggregated and identify them

# harry_aggr %>%
#   group_by(tag_type) %>%
#   summarise(n = length(n_syn[!is.na(n_syn)]))
# 
# count_syn_tot <- harry_aggr %>% # group by canonical tag
#   count(canonical_tag, tag_type, n_syn, sort = TRUE) %>%
#   drop_na() %>% 
#   group_by(canonical_tag) %>% 
#   summarise(tot = sum(n))
#             
# count_syn <- harry_aggr %>% # show single synonyms
#   count(canonical_tag, tag_type, n_syn, sort = TRUE) %>%
#   drop_na()
# 
# freeforms_syn[132,50]

# Accumulation analysis ---------------------------------------------------

# start from 2002 because 2000 and 2001 have only 21 and 55 stories
harry_aggr <- harry_aggr %>%
  filter(between(year_fix, 2002, 2019)) 
  
# total number of stories by year:
harry_aggr %>%
  group_by(year_fix) %>%
  summarise(count = n_distinct(index)) %>%
  ggplot(aes(x = year_fix, y = count)) +
    geom_col() +
    scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
    scale_y_continuous(labels = function(x) format(x, scientific = FALSE)) +
    labs(x = "Year", y = "Number of stories") +
    theme_bw() +
    ggsave("analysis/plots/1_1_number_of_stories.pdf", width = 5, height = 5)

# total number of different tags by year
harry_aggr %>% 
  group_by(year_fix, tag_type) %>%
  summarise(count = n_distinct(canonical_tag)) %>%
  ggplot(aes(x = year_fix, y = count)) +
    geom_point() +
    geom_line() +
    scale_color_jco() +
    scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
    facet_wrap(vars(tag_type), scales = "free") +
    labs(x = "Year", y = "Count") +
    theme_bw() +
    ggsave("analysis/plots/1_2_accumulation_totals.pdf", width = 12, height = 5)

## SPECIFIC TAGS ANALYSIS 

# For each character, or group of characters, we can show that the number of 
# freeforms and relationships tags increase, ans there is more diversity in stories.
# Here an example for Harry and freeforms tags with data extracted from the "linked-potter"
# knowledge base.

# Harry:
# tagOf_harry <- read_csv("data/tagOf_harry.csv", quote="")
# 
# tagOf_harry <- tagOf_harry %>% 
#   mutate(tagOf = "Harry Potter") %>% 
#   filter(tagOf_first_row != "Harry Potter") %>% 
#   rename(isTaggedAs = tagOf_first_row)
# 
# harry_aggr_tagOf <- harry_aggr %>% 
#   filter(tag %in% tagOf_harry$isTaggedAs) 
# 
# freeform_groups <- read_csv("data/freeform_groups_harry.csv")
# 
# syn_count_tagOf <- harry_aggr_tagOf %>%
#   left_join(freeform_groups, by = "canonical_tag") %>% 
#   group_by(year_fix) %>% 
#   mutate(n_stories_y = n_distinct(index)) %>% 
#   count(year_fix, tag, tag_type, canonical_tag, freeform_group, n_stories_y, n_syn) %>%
#   group_by(year_fix, canonical_tag, freeform_group, n_stories_y) %>% 
#   summarise(tot = sum(n))
# 
# syn_count_tagOf %>% 
#   ggplot(aes(x = year_fix, y = tot, fill = canonical_tag)) +
#   geom_bar(position="fill", stat="identity") +
#   scale_fill_igv() +
#   labs(x = "Year", y = "Proportion of stories", title = "Harry Potter") +
#   scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
#  theme_bw()

# With the same logic we can do it for all other characters. Most of this material can go in 
# supplementary material. One to show in the main manuscript is the relationships of the three
# characters, Harry, Hermione, and Draco.
# NOTE: Federico will do an interactive version of this. 

# HARRY:
ships_harry <- read_csv("data/ships_harry.csv", quote="")

ships_harry <- ships_harry %>% 
  mutate(hasParticipant = "Harry Potter") %>% 
  filter(hasParticipant_first_row != "Harry Potter") %>% 
  rename(participatesTo = hasParticipant_first_row)

harry_aggr_ships <- harry_aggr %>% 
  filter(tag %in% ships_harry$participatesTo)

# count stories per year
syn_count_harry_ships <- harry_aggr_ships %>%
  group_by(year_fix) %>% 
  mutate(n_stories_y = n_distinct(index)) %>% 
  count(year_fix, tag, tag_type, canonical_tag, n_stories_y, n_syn) %>%
  group_by(year_fix, canonical_tag, n_stories_y) %>% 
  summarise(tot = sum(n))%>% 
  mutate(Character = "Harry", avg_count = tot/n_stories_y) %>% 
  filter(tot >= 10) 

# DRACO
ships_draco <- read_csv("data/ships_draco.csv", quote="")

ships_draco <- ships_draco %>% 
  mutate(hasParticipant = "Draco Malfoy") %>% 
  filter(hasParticipant_first_row != "Draco Malfoy") %>% 
  rename(participatesTo = hasParticipant_first_row)

harry_aggr_draco_ships <- harry_aggr %>% 
  filter(tag %in% ships_draco$participatesTo)

# count stories per year
syn_count_draco_ships <- harry_aggr_draco_ships %>%
  group_by(year_fix) %>% 
  mutate(n_stories_y = n_distinct(index)) %>% 
  count(year_fix, tag, tag_type, canonical_tag, n_stories_y, n_syn) %>%
  group_by(year_fix, canonical_tag, n_stories_y) %>% 
  summarise(tot = sum(n)) %>% 
  mutate(Character = "Draco", avg_count = tot/n_stories_y) %>% 
  filter(tot >= 10)

# HERMIONE:
ships_hermione <- read_csv("data/ships_hermione.csv", quote="")

ships_hermione <- ships_hermione %>% 
  mutate(hasParticipant = "Hermione Granger") %>% 
  filter(hasParticipant_first_row != "Hermione Granger") %>% 
  rename(participatesTo = hasParticipant_first_row)

harry_aggr_hermione_ships <- harry_aggr %>% 
  filter(tag %in% ships_hermione$participatesTo)

# count stories per year
syn_count_hermione_ships <- harry_aggr_hermione_ships %>%
  group_by(year_fix) %>% 
  mutate(n_stories_y = n_distinct(index)) %>% 
  count(year_fix, tag, tag_type, canonical_tag, n_stories_y, n_syn) %>%
  group_by(year_fix, canonical_tag, n_stories_y) %>% 
  summarise(tot = sum(n)) %>% 
  mutate(Character = "Hermione", avg_count = tot/n_stories_y) %>% 
  filter(tot >= 10) 

# PUT TOGETHER:
combine_ships <- bind_rows(syn_count_harry_ships, syn_count_hermione_ships, syn_count_draco_ships) %>% 
  arrange(Character, year_fix, desc(tot))

# generate a colour palette for each character
n_colors <- length(unique(combine_ships$canonical_tag[combine_ships$Character == "Draco"]))
n_colors2 <- length(unique(combine_ships$canonical_tag[combine_ships$Character == "Harry"]))
n_colors3 <- length(unique(combine_ships$canonical_tag[combine_ships$Character == "Hermione"]))
mypal <- colorRampPalette(brewer.pal(9, "RdPu"))(n_colors)[order(sample(1:n_colors, n_colors))] # for legibility, shuffle palette order
mypal2 <- colorRampPalette(brewer.pal(9, "BuGn"))(n_colors2)[order(sample(1:n_colors2, n_colors2))]
mypal3 <- colorRampPalette(brewer.pal(9, "YlOrBr"))(n_colors3)[order(sample(1:n_colors3, n_colors3))]

combine_ships %>% 
  ggplot(aes(x = year_fix, y = tot, fill = interaction(as.factor(canonical_tag), Character))) +
  geom_bar(stat = "identity", position = "fill") +
  scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
  scale_fill_manual(values = c(mypal, mypal2, mypal3)) +
  labs(x = "Year", y = "Proportion of stories") +
  theme_bw() +
  theme(legend.position = "none") +
  ggsave("analysis/plots/1_3_accumulation_relationships_growth.pdf", width = 8, height = 5)

## -----------------------------------------------------------------------

# Calculate number of tags for each story

harry_aggr <- harry_aggr %>%
  group_by(index, hits, kudos, bookmarks, year_fix) %>%
  summarise(characters_n = n_distinct(canonical_tag[tag_type == "characters"]), # get the number of canonical "character" tags for the story
            relationships_n = n_distinct(canonical_tag[tag_type == "relationships"]),
            freeforms_n = n_distinct(canonical_tag[tag_type == "freeforms"]))

# Optional: check how many stories don't use tags for each category -- for some years it can reach 30-50%

# harry_aggr %>%
#   group_by(year_fix) %>% 
#   summarise(zero_char = n_distinct(index[characters_n == 0]), prop_char = zero_char / n_distinct(index),
#             zero_ship = n_distinct(index[relationships_n == 0]), prop_ship = zero_ship / n_distinct(index),
#             zero_free = n_distinct(index[freeforms_n == 0]), prop_free = zero_free / n_distinct(index),) 
  

# Calculate percentiles

harry_aggr <- harry_aggr %>%  
  group_by(year_fix) %>% # calculate per year to be sure to have stories from all years for each percentile
  mutate(hits_p = ntile(hits, 100)) %>%
  mutate(kudos_p = ntile(kudos, 100)) %>%
  mutate(bookmarks_p = ntile(bookmarks, 100))

# Optional: check the number of NAs per year for each metric
# Number of NAs is 1.7~10% for hits, 1~19% for kudos, 16~54% for bookmarks

# check_na <- harry %>%
#   group_by(year_fix) %>%
#   distinct(index, .keep_all = TRUE) %>%
#   summarise(unique_stories = n(),
#             NA_hits = sum(is.na(hits)), prop_NA_hits = NA_hits / unique_stories,
#             NA_kudos = sum(is.na(kudos)), prop_NA_kudos = NA_kudos / unique_stories,
#             NA_bookmarks = sum(is.na(bookmarks)), prop_NA_bookmarks = NA_bookmarks / unique_stories)


## Count unique tags per percentile interval
# Specify the metric to be used in group_by (e.g. here kudos) and percentiles intervals
# of same length for all clusters (e.g. here 10)

# KUDOS

# low kudos
lower_percentile <- 1 
higher_percentile <- 10

harry_low <- harry_aggr %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

# mid kudos
lower_percentile <- 46
higher_percentile <- 55

harry_mid <- harry_aggr %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

# high kudos
lower_percentile <- 91
higher_percentile <- 100

harry_high <- harry_aggr %>%
  filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
  group_by(year_fix, kudos_p) %>%
  mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
         n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
         n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
  group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
  summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
            min(kudos), max(kudos)) %>%
  mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
         Freeforms = free_tags_count/n_stories_y_p_free) %>%
  pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")

# PROPER STAT MODEL HERE:
# Compare linear and exponential fitting.
cumulative <- bind_rows(harry_high, harry_mid, harry_low) %>%
  add_column(Rank = as_factor(rep(c("High kudos (91-100th percentile)",
                                    "Mid kudos (46-55th percentile)",
                                    "Low kudos (1-10th percentile)"), each = 540)))

# replace below High/Mid/Low kudos:
trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Freeforms") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))

trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Characters") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))


trend_to_fit <- cumulative %>%
  filter(Rank == "Low kudos (1-10th percentile)", tag_type == "Relationships") 
y <- trend_to_fit$count
x <- trend_to_fit$year_fix
AIC(lm(y ~ x))
AIC(lm(y ~ x + I(x^2)))

# compare slopes of percentiles
cumulative %>%
  ggplot(aes(x = year_fix, y = count, colour = Rank)) +
    geom_point() +
    geom_smooth(method = "lm", formula = y ~ x + I(x^2)) + # according to the test, it is better an exponential fit.
    scale_color_jco() +
    scale_x_continuous(breaks= c(2002,2006,2010,2014,2018)) +
    facet_wrap(vars(tag_type), scales = "free") +
    labs(x = "Year", y = "Average count") +
    theme_bw() +
    ggsave("analysis/plots/1_4_accumulation_percentiles.pdf", width = 15, height = 5)

# Linear model on slopes --------------------------------------------------

### Calculate slope on all percentiles
# using linear model
# on KUDOS

# function for linear model
get_slope <- function(my_distr){
  
  # prepare variables for linear model
  x <- my_distr$year_fix
  y <- my_distr$count
  
  # generate model
  test <- lm(y ~ x)
  # get slope
  return(test$coefficients[2])

}

# moving window of 10 percentiles
characters_slope <- numeric()
relationships_slope <- numeric()
freeforms_slope <- numeric()
percentiles <- 1:91 # this depends on the size of the percentiles we use
for(my_percentile in percentiles){
  
  lower_percentile <- my_percentile 
  higher_percentile <- my_percentile+9 # this depends on the size of the percentiles we use
  
  harry_perc <- harry_aggr %>%
    filter(between(kudos_p, lower_percentile, higher_percentile)) %>%
    group_by(year_fix, kudos_p) %>%
    mutate(n_stories_y_p_char = n_distinct(index[characters_n != 0]), # for every year, for each percentile, calculate number of stories having tags of a certain category
           n_stories_y_p_ships = n_distinct(index[relationships_n != 0]),
           n_stories_y_p_free = n_distinct(index[freeforms_n != 0])) %>%
    group_by(year_fix, kudos_p, n_stories_y_p_char, n_stories_y_p_ships, n_stories_y_p_free) %>%
    summarise(char_tags_count = sum(characters_n), ships_tags_count = sum(relationships_n), free_tags_count = sum(freeforms_n),
              min(kudos), max(kudos)) %>%
    mutate(Characters = char_tags_count/n_stories_y_p_char, Relationships = ships_tags_count/n_stories_y_p_ships,
           Freeforms = free_tags_count/n_stories_y_p_free) %>%
    pivot_longer(Characters:Freeforms, names_to = "tag_type", values_to = "count")
  
  # get Kudos distribution per characters
  my_distr <- harry_perc[which(harry_perc$tag_type == "Characters"),]
  # get slope
  characters_slope[my_percentile] <- get_slope(my_distr)
  
  # get Kudos distribution per relationships
  my_distr <- harry_perc[which(harry_perc$tag_type == "Relationships"),]
  # get slope
  relationships_slope[my_percentile] <- get_slope(my_distr)
  
  # get Kudos distribution per freeforms
  my_distr <- harry_perc[which(harry_perc$tag_type == "Freeforms"),]
  # get slope
  freeforms_slope[my_percentile] <- get_slope(my_distr)
  
  print(my_percentile)
  
}

# plot  
my_df <- data.frame(percentiles = c(percentiles), Freeforms = freeforms_slope, Characters = characters_slope, Relationships = relationships_slope)
my_df_melt <- melt(my_df, id.vars = "percentiles")
names(my_df_melt) <- c("percentiles", "Tag", "linear_model_slope")
ggplot(my_df_melt, mapping = aes(x = percentiles, y = linear_model_slope)) +
  geom_line(aes(color = Tag), size = 1.5) +
  scale_color_aaas() +
  scale_x_continuous(breaks = seq(1,91,by=10)) +
  ylim(c(0, 0.65)) +
  theme_bw() +
  labs(y = "Slope of the trend between 2002 and 2019", x = "Lower bound of 10-percentile window") +
  ggsave("analysis/plots/1_5_accumulation_all_percentiles.pdf", width = 6, height = 5)

# all of the above can be done using bookmarks or hits as metric
