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
  Sys.sleep(0.5)
}

movie_df <- as.data.frame(movie_list, stringsAsFactors = FALSE)
movie_df %>% filter(!is.na(str_extract(.$movie_list, "http")))
## As we can see here above, there are few entries which we need to fix on
movie_df[146, 1] <- "Gladiator.htm"
movie_df[148, 1] <- "Goldfinger.htm"
movie_df[154, 1] <- "Grosse_Pointe_Blank.htm"
movie_df[255, 1] <- "Lion_Witch_Wardrobe.htm"

## Then we can loop through the `movie_list` and obtain the movie details
N <- length(movie_df$movie_list)
for(i in 1:N){
  url <- paste(url_root, movie_df[i,1], sep = "/")
  test <- 
     url %>%
     read_html() %>%
     html_nodes("td div") %>%
     html_text() %>%
    str_trim() %>%
    str_replace_all("\r\n", " ") %>%
    str_replace_all(" +", " ") %>%
    str_replace("For guidelines on how the counts are conducted please see here", "")
    
  test <- subset(test, test!= "")
 
  movie_df$name[i] <- test[2]
  movie_df$date[i] <- test[3] 
  movie_df$info[i] <- test[4]
  movie_df$kill[i] <- test[5] 
  
  
  tryCatch({                  
    scrape_test(url)
  },
  error = function(e) {
    Sys.sleep(1)
    e
  })
}

## After scrapping from the websites, I identified a quite amount of entries with `wrong` nodes
changelist <- c(4, 12, 14, 18, 23, 24, 25, 26, 34, 45, 46, 50, 54, 69, 82, 92, 94, 96, 100, 102, 104, 106, 114, 116, 131, 158, 183, 184, 197, 202, 203, 204, 205, 207, 
                211, 217, 222,223, 224, 225, 229, 232, 233, 236,238, 241, 243, 246, 247, 248, 251, 252, 254, 264, 269, 299, 305, 308, 314, 317, 362, 375, 377, 378, 384
                ,385, 395, 396, 397) 
## And I randomly picked some wrong entries and found the correct nodes to select  
for(i in 1:length(changelist)){
  w <- changelist[i]
  url <- paste(url_root, movie_df[w,1], sep = "/")
  test <- 
    url %>%
    read_html() %>%
    html_nodes("td div") %>%
    html_text() %>%
    str_trim() %>%
    str_replace_all("\r\n", " ") %>%
    str_replace_all(" +", " ") %>%
    str_replace("For guidelines on how the counts are conducted please see here", "")
  
  test <- subset(test, test!= "")
  movie_df$kill[w] <- test[6] 
  
  tryCatch({                  
    scrape_test(url)
  },
  error = function(e) {
    Sys.sleep(1)
    e
  })
}
## I fixed the following ith entry, and left 7 to work on yeahhh
changedlist <- c(12, 14, 18, 23, 24, 34, 45, 46, 54, 69, 82, 92, 94, 96, 100, 102, 104, 106, 116, 131, 158, 183, 184, 197, 202, 203, 204, 205, 207, 217, 223, 224, 225,
                 229, 232, 233, 236, 238, 241, 243, 246, 247, 248,251, 252, 254, 264, 269, 299, 305, 308, 314, 317, 362, 375, 377, 378, 384, 385, 395,396, 397)
table(changelist %in% changedlist) 

## A second list developed
changelist2 <- rep(NA,0)
changed <- (changelist %in% changedlist)
for(i in 1:length(changelist)){
  if(changed[i] == FALSE) changelist2 <- c(changelist2, changelist[i])
}

for(i in 1:length(changelist2)){
  w <- changelist2[i]
  url <- paste(url_root, movie_df[w,1], sep = "/")
  test <- 
    url %>%
    read_html() %>%
    html_nodes("td div") %>%
    html_text() %>%
    str_trim() %>%
    str_replace_all("\r\n", " ") %>%
    str_replace_all(" +", " ") %>%
    str_replace("For guidelines on how the counts are conductedplease see here", "")
  
  test <- subset(test, test!= "")
  movie_df$kill[w] <- test[5] 
}

changedlist2 <- c(4,25)
table(changelist2 %in% changedlist2) 

## The third try... on the last five entries
changelist3 <- rep(NA,0)
changed <- (changelist2 %in% changedlist2)
for(i in 1:length(changelist2)){
  if(changed[i] == FALSE) changelist3 <- c(changelist3, changelist2[i])
}

w <- changelist3[1]
url <- paste(url_root, movie_df[w,1], sep = "/")
test <- 
  url %>%
  read_html() %>%
  html_nodes("td div") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("\r\n", " ") %>%
  str_replace_all(" +", " ") %>%
  str_replace("For guidelines on how the counts are conductedplease see here", "")

test <- subset(test, test!= "")
movie_df$kill[w] <- test[6] 

w <- changelist3[2]
url <- paste(url_root, movie_df[w,1], sep = "/")
test <- 
  url %>%
  read_html() %>%
  html_nodes("td div") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("\r\n", " ") %>%
  str_replace_all(" +", " ") %>%
  str_replace("For guidelines on how the counts are conducted please see here", "")

test <- subset(test, test!= "")
movie_df$kill[w] <- test[5] 

## A diffcult one 
w <- changelist3[3]
url <- paste(url_root, movie_df[w,1], sep = "/")
test <- 
  url %>%
  read_html() %>%
  html_nodes("td div") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("\r\n", " ") %>%
  str_replace_all(" +", " ") %>%
  str_replace("For guidelines on how the counts are conducted please see here", "")

test <- subset(test, test!= "")
test[1] <- 
  test[1] %>%
  str_replace("Equilibrium (2002)", "") %>%
  str_replace("Director: Kurt Wimmer Actors: Christian Bale, Taye Diggs, Sean Bean MPAA Rating: R for violence Genre: action, sci/fi Subjects: dystopian Links: IMDb, Fansite, Wikipedia", "") %>%
  str_replace("Home : Movies: E : Equilibrium", "") %>%
  str_trim()
movie_df$kill[w] <- test[1]

w <- changelist3[4]
url <- paste(url_root, movie_df[w,1], sep = "/")
test <- 
  url %>%
  read_html() %>%
  html_nodes("td div") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("\r\n", " ") %>%
  str_replace_all(" +", " ") %>%
  str_replace("For guidelines on how the counts are conducted please see here", "")

test <- subset(test, test!= "")
movie_df$kill[w] <- test[7]

## Also a hard one...
w <- changelist3[5]
url <- paste(url_root, movie_df[w,1], sep = "/")
test <- 
  url %>%
  read_html() %>%
  html_nodes("td div") %>%
  html_text() %>%
  str_trim() %>%
  str_replace_all("\r\n", " ") %>%
  str_replace_all(" +", " ") %>%
  str_replace("For guidelines on how the counts are conducted please see here", "")

test <- subset(test, test!= "")
test
movie_df$kill[w] <- test[7]

## Here we have a first draft of dataset ready to be cleaned
write_csv(movie_df, na = "kill_df")
