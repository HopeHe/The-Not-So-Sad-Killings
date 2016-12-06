## test project

library(tidyverse)
library(rvest)
library(XML)
library(stringr)

url_root <- "http://www.moviebodycounts.com"

## To extract the index of movie dictionary
index_link <- 
  paste(url_root, "movies.htm", sep = "/") %>% 
  read_html() %>%
  html_nodes("div div tr+ tr a") %>%
  html_attr("href")

## And we make a list of movies we want to scrap data from
n <- length(index_link)
movie_list <- rep(NA, 0)

for(i in 1:n){
  url <- paste(url_root, index_link[i], sep = "/")
  index_list <- 
    url %>%
    read_html() %>%
    html_nodes("table+ div a") %>%
    html_attr("href")
  
  movie_list <- c(movie_list, index_list)
}

movie_df <- as.data.frame(movie_list, stringsAsFactors = FALSE)
movie_df %>% filter(!is.na(str_extract(.$movie_list, "http")))
## As we can see here above, there are few entries which we need to fix on
movie_df[288, 1] <- "Gladiator.htm"
movie_df[290, 1] <- "Goldfinger.htm"
movie_df[296, 1] <- "Grosse_Pointe_Blank.htm"
movie_df[397, 1] <- "Lion_Witch_Wardrobe.htm"

## Then we can loop through the `movie_list` and obtain the movie details
N <- length(movie_df$movie_list)
for(i in 293:300){
  url <- paste(url_root, movie_df$movie_list[294], sep = "/")
  test <- 
    url %>%
    read_html() %>%
    html_nodes("div td td td span") %>%
    html_text() %>%
    str_replace_all("\r\n", " ")
  
  test <- subset(test, test!= "")
  
  movie_df$name[i] <- test[1]
  if(is.na(test[3])) {
    movie_df$date[i] <- test[2]
  } else {
    movie_df$date[i] <- test[3] 
  }
}

for(i in 1:N){
  url <- paste(url_root, movie_list[N], sep = "/")
  test <- 
     url %>%
     read_html() %>%
     html_nodes("td div") %>%
     html_text() %>%
    str_replace_all("\r\n", " ")
 
  c(test[4], test[7], test[8], test[9])
}

test %>%
  html_text()

br+ div > font
