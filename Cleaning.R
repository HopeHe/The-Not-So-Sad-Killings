library(tidyverse)
library(stringr)
library(rvest)
movie <- read_csv("kill_df")

for(i in 1:length(movie$movie_list)){
  movie$Director[i] <- str_split(movie$info[i], "Actors:|Voices:")[[1]][1]
  rest <- str_split(movie$info[i], "Actors:|Voices:")[[1]][2]
  movie$Actors[i] <- str_split(rest, "MPAA Rating:|USA Rating:|Original Rating:|Rating:|MPAA:|MPAA")[[1]][1]
  rest <- str_split(rest, "MPAA Rating:|USA Rating:|Original Rating:|Rating:|MPAA:|MPAA")[[1]][2]
  movie$Ratings[i] <- str_split(rest, "Genre:")[[1]][1]
  rest <- str_split(rest, "Genre:")[[1]][2]
  movie$Genre[i] <- str_split(rest, "Subjects:|Subject")[[1]][1]
  rest <- str_split(rest, "Subjects:|Subject")[[1]][2]
  movie$Subject[i] <- str_split(rest, "Links")[[1]][1] 
}
## LAST THREE DOESN'T HAVE ANY ENOUGH INFOMATION
## So we are done extracting information from the info 

## date
movie$date <- 
  movie$date %>%
  str_extract("[0-9]{4}") %>%
  as.numeric()

## director
movie$Director <- 
  movie$Director %>%
  str_replace("Director: ","") %>%
  str_replace("Directors:","") %>%
  str_trim()

movie <- separate(movie, Director, into = c("d1","d2","d3","d4","d5"), sep = ",")
movie <- gather(movie, `d1`:`d5`, key = "d", value = "director")

movie$director <- 
  movie$director %>%
  str_replace("Director:|Fake trailers:", "") %>%
  str_trim()
movie <- filter(movie, !is.na(director))

## Killed figures 
movie$`Total killed` <-
  movie$kill %>%
  str_extract("[0-9]+") %>%
  as.numeric()

## The Ratings 
movie_temp <- movie$Ratings
## Because the extract function might extract "R" out of "Rated"
movie$Ratings <- 
  movie$Ratings %>%
  str_replace("Rated", "")
movie$Ratings <- 
  movie$Ratings %>%
  str_extract("PG-13|R|NC-17|GP|X|M|unrated|Unrated|Approved")
ratinglist <- which(is.na(movie$Ratings))
for(i in 1:length(ratinglist)){
  o <- ratinglist[i]
  movie$Ratings[o] <-
    movie_temp[o] %>%
    str_extract("PG")
}
which(is.na(movie$Ratings))
## I manually replaced some entries

movie$Ratings[68] <- "Unrated"
movie$Ratings[57] <- "Unrated"
movie$Ratings[69] <- "Unrated"
movie$Ratings[225] <- "Unrated"
movie$Ratings[232] <- "Unrated"
movie$Ratings[243] <- "Unrated"
movie$Ratings[320] <- "G"

## done with the Genres
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

## actors
movie <- separate(movie, Actors, into = c("a1","a2","a3","a4","a5","a6","a7","a8","a9","a10"), sep = ",")
movie <- gather(movie, `a1`:`a10`, key = "A", value = "Cast")
movie$`Cast` <- 
  movie$`Cast` %>%
  str_trim() 
movie <- filter(movie, !is.na(`Cast`))

## Subject
url <- "http://www.moviebodycounts.com/charts-subjects.htm"
subjectlist <- 
  url %>%
  read_html() %>%
  html_nodes("td center a") %>%
  html_text() %>%
  str_replace("\r\n", " ") %>%
  str_to_lower() 

subjectlist <- 
  as.character(str_split(subjectlist, "/", simplify = T)) %>% 
  str_trim()
subjectlist[3] <- "tv"
subjectlist[4] <- "rides"
subjectlist[43] <- "games"

subjectlist <- subset(subjectlist, subjectlist!="")
subjectlist <- paste(subjectlist, collapse = "|")

movie$subject <- 
  movie$Subject %>%
  str_extract_all(subjectlist) 

for(i in 1:length(movie$subject)){
  movie$subject[[i]] <- paste(movie$subject[[i]], collapse = ",")
}

movie <- separate(movie, subject, into = c("s1","s2","s3", "s4"), sep = ",")
movie <- gather(movie, `s1`:`s4`, key = "s", value = "Movie Subject")
movie <- filter(movie, !is.na(`Movie Subject`))

movie <- select(movie, name, date, director, Ratings, `Total killed`, Genre, Cast, `Movie Subject`)

write_csv(movie, path = "movie_tidy1_df")

