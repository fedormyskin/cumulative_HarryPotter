library(tidyverse)
library(scales)

read_and_clean <- function(){
  # (notice the file is not part of the GitHub repository because exceeds the size limit):
  harry <- read_csv("Full_HP_AO3_tags.csv") %>%
    rename(index = X1) 
  
  # The "date" information in the data indicates the last modification. We want instead when the story was first written.
  # So we use the information from the link, as they are progressive.  
  first_stories <- read_csv(file = "first_stories.csv") %>%
    mutate(link = as.integer(str_remove(link, "/works/"))) %>%
    arrange(link)
  
  harry <- harry %>%
    mutate(link = as.integer(str_remove(link, "/works/"))) %>%
    arrange(link) %>%
    add_column(date_year = 2020) # as the stories that are not modified in the loop below are the 2020 ones.
  
  for(i in 1:(dim(first_stories)[1] - 1)){
    harry[harry$link >= first_stories$link[i] & harry$link < first_stories$link[i+1],]$date_year <- first_stories$year[i] 
  }
  
  harry <- harry %>%
    filter(words > 10) %>% # we exclude stories with less than 10 words
    filter(date_year != 2020) # and stories from 2020
  # return the data:
  harry
}

# load the data:
harry<- read_and_clean()

## Tidy up tibble

# set variables
harry_index <- character()
harry_tag <- character()
harry_tag_type <- character()
counter <- 1
multiplier <- character()

## Main iteration (might take a couple of minutes...)
for(line in 1:length(harry$index)){
  
  #print(line)
  
  tmp_df <- harry[line,]
  
  ## Split all tags, one by one (boring but efficient)
  # rating
  rating_tmp <- unlist(strsplit(tmp_df$rating, ", "))
  names(rating_tmp) <- rep("rating", length(rating_tmp))
  
  # archive_warning
  archive_warning_tmp <- unlist(strsplit(tmp_df$archive_warning, ", "))
  names(archive_warning_tmp) <- rep("archive_warning", length(archive_warning_tmp))
  
  # category
  category_tmp <- unlist(strsplit(tmp_df$category, ", "))
  names(category_tmp) <- rep("category", length(category_tmp))
  
  # complete (no need to split)
  complete_tmp <- tmp_df$complete
  names(complete_tmp) <- "complete"
  
  # fandoms
  fandoms_tmp <- unlist(strsplit(tmp_df$fandoms, ", "))
  names(fandoms_tmp) <- rep("fandoms", length(fandoms_tmp))
  
  # relationships
  relationships_tmp <- unlist(strsplit(tmp_df$relationships, ", "))
  names(relationships_tmp) <- rep("relationships", length(relationships_tmp))
  
  # characters
  characters_tmp <- unlist(strsplit(tmp_df$characters, ", "))
  names(characters_tmp) <- rep("characters", length(characters_tmp))
  
  # freeforms
  freeforms_tmp <- unlist(strsplit(tmp_df$freeforms, ", "))
  names(freeforms_tmp) <- rep("freeforms", length(freeforms_tmp))
  
  # merge in a single named vector
  all_tags <- c(rating_tmp, archive_warning_tmp, category_tmp, complete_tmp, fandoms_tmp, relationships_tmp, characters_tmp, freeforms_tmp)
  # remove all NAs
  all_tags <- all_tags[which(!is.na(all_tags))]
  
  # check if the story has any tag (it should have...)
  if(length(all_tags) < 1){
    print("ERROR! No tags!!!!")
    break
  }
  
  # gradually merge tags into a single vector
  for(my_tag in 1:length(all_tags)){
    
    harry_index[counter] <- tmp_df$index
    harry_tag[counter] <- all_tags[my_tag]
    harry_tag_type[counter] <- names(all_tags)[my_tag]
    
    counter <- counter+1
    
    # little trick: use a string to keep track of how many tags (how many "x") are present for a work
    if(my_tag == 1)
      multiplier[line] <- "x"
    if(my_tag > 1)
      multiplier[line] <- paste(multiplier[line], "x")
    
  } 
}

# add the multiplier to the tibble
harry$multiplier <- multiplier
# separate tibble using multiplier (might take some time)
harry <- separate_rows(harry, multiplier, sep = " ", convert = FALSE)
# check consistency
length(harry_index) == length(harry$index)
# double check consistency (answer should be "0")
length(which(harry_index!=harry$index))

# delete silly multiplier
harry$multiplier <- NULL

# delete all tags
harry <- harry[,-(5:12)]

# add tags in tidy format
harry$tag <- harry_tag
harry$tag_type <- harry_tag_type

# save to R environment (to save space...)
save(harry, file = "Harry_tidy.RData", version = 2) # version 2 for compatibility with R <3.6
