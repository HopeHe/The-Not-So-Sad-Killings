library(tidyverse)
library(stringr)
movie <- read_csv("kill_df")

for(i in 1:length(movie$movie_list)){
  movie$Director[i] <- str_split(movie$info[i], "Actors:")[[1]][1]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "MPAA Rating:")[[1]][1]
  rest <- str_split(rest, "MPAA Rating:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}

##Apparently there are tons of empty entries I need to re-try to fill...
## Firstly I need to add the data in the actors sections 
which(is.na(movie$Actors)) 
## Some nodes have "Voices" instead if "Actors"
for(o in 1:4){
  i <- which(is.na(movie$Actors))[o]
  movie$Director[i] <- str_split(movie$info[i], "Voices:")[[1]][1]
  rest <- str_split(movie$info[i], "Voices:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "MPAA Rating:")[[1]][1]
  rest <- str_split(rest, "MPAA Rating:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}


i <- 177
movie$Director[i] <- str_split(movie$info[i], "Voices:")[[1]][1]
rest <- str_split(movie$info[i], "Voices:")[[1]][2]
movie$Actors[i] <- str_split(rest, "MPAA Rating:")[[1]][1]
rest <- str_split(rest, "MPAA Rating:")[[1]][2]
movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
rest <- str_split(rest, "Genre:")[[1]][2]
movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
rest <- str_split(rest, "Subjects:")[[1]][2]
movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 


i <- 348
movie$Director[i] <- str_split(movie$info[i], "Voices:")[[1]][1]
rest <- str_split(movie$info[i], "Voices:")[[1]][2]
movie$Actors[i] <- str_split(rest, "MPAA Rating:")[[1]][1]
rest <- str_split(rest, "MPAA Rating:")[[1]][2]
movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
rest <- str_split(rest, "Genre:")[[1]][2]
movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
rest <- str_split(rest, "Subjects:")[[1]][2]
movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 

## Alright...move on, we have some entries of `Ratings` to work on  
which(is.na(movie$Ratings))
changelist <- which(is.na(movie$Ratings))
for(o in 1:length(changelist)){
  i <- changelist[o]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "USA Rating:")[[1]][1]
  rest <- str_split(rest, "USA Rating:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}
## YEAHHHHH ONLY 17 left
which(is.na(movie$Ratings))
changelist <- which(is.na(movie$Ratings))
for(o in 1:length(changelist)){
  i <- changelist[o]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "Original Rating:")[[1]][1]
  rest <- str_split(rest, "Original Rating:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}

## 8 to go yeeeehahahawwww
which(is.na(movie$Ratings))
changelist <- which(is.na(movie$Ratings))
for(o in 1:length(changelist)){
  i <- changelist[o]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "Rating:")[[1]][1]
  rest <- str_split(rest, "Rating:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}

## Last 5!!!
which(is.na(movie$Ratings))
changelist <- which(is.na(movie$Ratings))
for(o in 1:length(changelist)){
  i <- changelist[o]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "MPAA:")[[1]][1]
  rest <- str_split(rest, "MPAA:")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}

## 4...
which(is.na(movie$Ratings))
changelist <- which(is.na(movie$Ratings))
for(o in 1:length(changelist)){
  i <- changelist[o]
  rest <- str_split(movie$info[i], "Actors:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "MPAA")[[1]][1]
  rest <- str_split(rest, "MPAA")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:")[[1]][1]
  rest <- str_split(rest, "Subjects:")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}

## LAST THREE DOESN'T HAVE ANY ENOUGH INFOMATION
## So we are done extracting information from the info 
movie$date <- 
  movie$date %>%
  str_extract("[0-9]{4}") %>%
  as.numeric()
movie$Director <- 
  movie$Director %>%
  str_replace("Director: ","") %>%
  str_replace("Directors:","") %>%
  str_trim()
movie$Entire.kill <-
  movie$kill %>%
  str_extract("[0-9]+") %>%
  as.numeric()
## The Ratings 
ratings <- c("G", "PG", "PG-13", "R", "NC-17")
movie$Ratings <- 
  movie$Ratings %>%
  str_extract("G|PG|PG-13|R|NC-17|GP|X|M|unrated|Unrated|Approved")
movie[68, 8] <- "Unrated"
movie$Ratings[57] <- "Unrated"
movie$Ratings[69] <- "Unrated"
movie$Ratings[232] <- "Unrated"
movie$Ratings[243] <- "Unrated"
## done with the Genres
movie$Genre <- map_chr(str_split(movie$Genre , "Subject"),1)
movie <- separate(movie, Genre, into = c("g1","g2","g3"), sep = ",")
movie <- gather(movie, `g1`:`g3`, key = "g", value = "Genre")
movie$Genre <- 
  movie$Genre %>%
  str_trim() %>%
  str_replace("biographies", "biography") %>%
  str_replace("fantasty", "fantasy") %>%
  str_replace("sci/fi", "sci-fi")
movie$Genre[69] <- "Ungiven"
movie$Genre[232] <- "Ungiven"
movie$Genre[243] <- "Ungiven"
movie <- filter(movie, !is.na(Genre))

movie <- movie %>% 
  select(name, date, kill, Director, Actors, Ratings, Entire.kill, Genre)
write_csv(movie, na = "movie_tidy1_df")
