## test project

library(tidyverse)
library(rvest)
library(XML)


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

## Then we can loop through the `movie_list` and obtain the movie details
 
