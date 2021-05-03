# load pacman
library(pacman)

# use pacman package to load other packages
p_load(tidyr, rio, dplyr, tidyverse)

setpath <- "C:/Users/JosephEmsley/Dropbox (AMEND Consulting)/AMEND Internal Business/So What/2021_q1_movies/Data"

imdb_movies <- read.csv(paste(setpath,"imdb movies.csv", sep="/"), header = TRUE) %>% 
  mutate(year = as.numeric(substr(year, nchar(year)-4+1,nchar(year))),
         date_published = as.Date(date_published),
         genre = str_remove_all(genre,","),
         worlwide_gross_income = as.numeric(trimws(str_remove_all(worlwide_gross_income,"[^[:alnum:]]"))),
         usa_gross_income = as.numeric(trimws(str_remove_all(usa_gross_income,"[^[:alnum:]]"))),
         worldwide_gross = ifelse(is.na(worlwide_gross_income),usa_gross_income,worlwide_gross_income)) %>%
  separate(genre, c("genre2","genre3","genre4"), sep = " ", remove = FALSE) %>% 
  filter(grepl("USA",country)) %>% 
  filter(year >= 1999) %>% 
  filter(year <= 2019) %>% 
  filter(!is.na(worldwide_gross)) %>% 
  separate(actors, c("lead","supporting"), sep = ", ", remove = FALSE)

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
  select(-imdb_title_id,-title,-writer,-original_title,-year,-genre,-date_published,-duration,-country,-language,-actors,-description,-avg_vote,-votes,-budget,-usa_gross_income,-metascore,-reviews_from_users,-reviews_from_critics,-genre2,-genre3,-genre4,-worlwide_gross_income)

#big regression o'clock
lm_lead2 <- lm(worldwide_gross ~ ., data = lead_reg_df)
lm_anova2 <- anova(lm_lead2)
lm_summary2 <- summary(lm_lead2)
lead_coef2 <- summary(lm_lead2)$coefficients
lead_coef2 <- cbind(lead = rownames(lead_coef2), lead_coef2) 










# #I want to see if certain movies are affected by the personal life (num divorces) of the associated actors
# imdb_names <- read.csv(paste(setpath,"imdb names.csv", sep="/"), header = TRUE)
# imdb_ratings <- read.csv(paste(setpath,"imdb ratings.csv", sep="/"), header = TRUE)
# imdb_principals <- read.csv(paste(setpath,"imdb title_principals.csv", sep="/"), header = TRUE)
# 
# 
# #credits are stored as a list of a list - needs extraction
# credits <- read.csv(paste(setpath,"credits.csv", sep="/"), header = TRUE)
# # fuck the rest of these datasets. I'm only doing the imdb dataset
# keywords <- read.csv(paste(setpath,"keywords.csv", sep="/"), header = TRUE)
# links <- read.csv(paste(setpath,"links.csv", sep="/"), header = TRUE)
# links_small <- read.csv(paste(setpath,"links_small.csv", sep="/"), header = TRUE)
# movies_metadata <- read.csv(paste(setpath,"movies_metadata.csv", sep="/"), header = TRUE)
# netflix_titles <- read.csv(paste(setpath,"netflix_titles.csv", sep="/"), header = TRUE)
# ratings <- read.csv(paste(setpath,"ratings.csv", sep="/"), header = TRUE)
# ratings_small <- read.csv(paste(setpath,"ratings_small.csv", sep="/"), header = TRUE)
# tmdb_credits <- read.csv(paste(setpath,"tmdb_5000_credits.csv", sep="/"), header = TRUE)
# tmdb_movies <- read.csv(paste(setpath,"tmdb_5000_movies.csv", sep="/"), header = TRUE)


export(imdb_movies,file="C:/Users/JosephEmsley/Dropbox (AMEND Consulting)/AMEND Internal Business/So What/fRanken.xlsx")
export(lead_coef,file="C:/Users/JosephEmsley/Dropbox (AMEND Consulting)/AMEND Internal Business/So What/fRankenleadslm.xlsx")

### clear global environment
rm(list=ls())

####Clear Packages
p_unload(all)

#####Clear Console
cat('\014')
