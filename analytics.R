library(tidyverse)
library(ggthemes)
mo <- read_csv("movie_tidy1_df")

mo1 <- 
  mo %>%
  select(name, date, Director, Ratings, Entire.kill) %>%
  unique()

mo1 <-
  mo1 %>%
  group_by(date, Ratings) %>%
  mutate(`mean kills` = mean(Entire.kill))

c <- ggplot(mo1, aes(date, fill = Ratings))+ geom_histogram()
c + theme_bw() + scale_fill_brewer(type = "seq") + ggtitle("Movies produced by year")  + xlab("year")
mo2 <- filter(mo1, date < 2009 & Ratings == "PG" | Ratings == "R")
d <- ggplot(mo2, aes(date, kills, colour = Ratings)) + geom_point(alpha = 0.5) + geom_line() + xlab("year") + ylab("number of people killed") + ggtitle("Average people killed in the movie per year") 
d + theme_economist() + scale_colour_economist()
  
m <- ggplot(mo) + geom_point(aes(date, Entire.kill, colour = Ratings), alpha = 0.5) + facet_wrap(~Genre)  + xlab("year") + ylab("People died in total") 
m + theme_() + scale_color_economist()

mo2 <- 
  mo %>%
  select(name, date, Director, Ratings, Entire.kill) %>%
  unique() %>%
  group_by(Director) %>%
  mutate(Dkill = sum(Entire.kill)) 

Dlist <- mo2 %>% arrange(desc(Dkill))

Tkill <- mo2 %>%
  ungroup() %>% 
  arrange(desc(Entire.kill))

Tkill <- select(Tkill, name, date, Entire.kill)
Tkill[1:20,]
write_csv(Tkill, path = "Most killed")
