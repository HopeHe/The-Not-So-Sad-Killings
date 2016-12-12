
mo <- read_csv("movie_tidy1_df")

mo1 <- 
  mo %>%
  select(name, date, Director, Ratings, Entire.kill) %>%
  unique()

mo1 <-
  mo1 %>%
  group_by(date) 
  