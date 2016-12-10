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

## Alright...move on, we have 169 entries of `Ratings` to work on  
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

