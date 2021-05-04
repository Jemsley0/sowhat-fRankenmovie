### About ----

# The Dataset

# The Goal

# The Methodology

### package load and script set up ----

# load pacman
library(pacman)

# use pacman package to load other packages
p_load(tidyr, rio, dplyr, tidyverse)

### variable setting ----

setpath <- "C:/Users/JosephEmsley/Documents/Github/sowhat-fRankenmovie/Data"

### ETL ----

# the movies we will be using must fit certain criteria
#   1. a USA based film (to account for "market reach" of movies as it would apply to gross)
#   2. a 20 year time period (1999-2019)

imdb_movies <- read.csv(paste(setpath,"imdb movies.csv", sep="/"), header = TRUE) %>% 
  mutate(year = as.numeric(substr(year, nchar(year)-4+1,nchar(year))),
         date_published = as.Date(date_published),
         genre = str_remove_all(genre,","),
         worlwide_gross_income = as.numeric(trimws(str_remove_all(worlwide_gross_income,"[^[:alnum:]]"))),
         usa_gross_income = as.numeric(trimws(str_remove_all(usa_gross_income,"[^[:alnum:]]"))),
         worldwide_gross = ifelse(is.na(worlwide_gross_income),usa_gross_income,worlwide_gross_income)) %>%
  separate(genre, c("genre2","genre3","genre4"), sep = " ", remove = FALSE) %>% 
  separate(country, "usacheck", sep = " ", remove = FALSE) %>% 
  mutate(usacheck = trimws(str_remove_all(usacheck,"[^[:alnum:]]"))) %>% 
  filter(usacheck == "USA") %>% 
  filter(year >= 1999) %>% 
  filter(year <= 2019) %>% 
  filter(!is.na(worldwide_gross)) %>% 
  separate(actors, c("lead","supporting"), sep = ", ", remove = FALSE)

### Analysis ----

# see which leads are related to the highest gross
lm_lead <- lm(worldwide_gross ~ lead, data = imdb_movies)
lm_anova <- anova(lm_lead)
lm_summary <- summary(lm_lead)
lead_coef <- summary(lm_lead)$coefficients
lead_coef <- cbind(lead = rownames(lead_coef), lead_coef) 

# get movie broken out by genre
genre_df<-imdb_movies %>% 
  select(imdb_title_id,genre2,genre3,genre4) %>% 
  gather(a, genre, genre2:genre4)
genre_df <- na.omit(genre_df) %>% 
  select(-a)

genre_list = genre_df %>% 
  select(genre) %>% 
  distinct()

temp_genre <- genre_df %>% 
  mutate(ct = 1) %>% 
  spread(imdb_title_id,ct)
imdb_genres <- data.frame(t(temp_genre[-1]))
colnames(imdb_genres) <- temp_genre[,1]
imdb_genres[is.na(imdb_genres)] <- 0
imdb_genres <- cbind(imdb_title_id = rownames(imdb_genres), imdb_genres)

imdb_movies <- left_join(imdb_movies,imdb_genres)
imdb_movies_gen <- imdb_movies

# get movie broken out by actors
actors_df<-imdb_movies %>% 
  select(imdb_title_id,lead,supporting) %>% 
  gather(a, actors, lead:supporting)
actors_df <- na.omit(actors_df) %>% 
  select(-a)

actors_list = actors_df %>% 
  select(actors) %>% 
  distinct()

temp_actors <- actors_df %>% 
  mutate(ct = 1) %>% 
  distinct() %>% 
  spread(imdb_title_id,ct)
imdb_actors <- data.frame(t(temp_actors[-1]))
colnames(imdb_actors) <- temp_actors[,1]
imdb_actors[is.na(imdb_actors)] <- 0
imdb_actors <- cbind(imdb_title_id = rownames(imdb_actors), imdb_actors)

imdb_movies <- left_join(imdb_movies,imdb_actors)

lead_reg_df <- imdb_movies_gen %>% 
  select(-imdb_title_id,-title,-writer,-original_title,-year,-genre,-date_published,-duration,-country,-language,-actors,-description,-avg_vote,-votes,-budget,-usa_gross_income,-metascore,-reviews_from_users,-reviews_from_critics,-genre2,-genre3,-genre4,-worlwide_gross_income, -usacheck)

lead_merge <- lead_reg_df %>% 
  select(-supporting) %>% 
  rename('actor'='lead')
sup_merge <-lead_reg_df %>% 
  select(-lead) %>% 
  rename('actor'='supporting')

actor_reg_df <- rbind(lead_merge, sup_merge)

#big regression o'clock
lm_lead2 <- lm(worldwide_gross ~ ., data = actor_reg_df)
lm_anova2 <- anova(lm_lead2)
lm_summary2 <- summary(lm_lead2)
lead_coef2 <- summary(lm_lead2)$coefficients
lead_coef2 <- cbind(lead = rownames(lead_coef2), lead_coef2) 

# I think what I need to do is break out the top 8 actors and then only look at the top 100 actors by # of films in. Then do the regression on that

### Clear ----

# clear global environment
rm(list=ls())

# unload Packages
p_unload(all)

# Clear Console
cat('\014')
