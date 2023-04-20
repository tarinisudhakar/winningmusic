library(tidyverse)
library(tidyr)
library(dplyr)

tracks <- read.csv("~/Documents/Coding/winningmusic/scrape/tracks.csv")

billboard_top_songs <- read.csv("~/Documents/Coding/winningmusic/scrape/billboard_top_songs.csv")

z = merge(billboard_top_songs, tracks, by.x = 'title', by.y = 'name', all.x = TRUE)

z[!duplicated(z$title), ]

x <- z %>% distinct(title, year, position, .keep_all = TRUE)
sum(is.na(x$id))

work <- x[complete.cases(x),]

write.csv(work, "~/Documents/Coding/winningmusic/work.csv", row.names=FALSE)


NAdata <- x[rowSums(is.na(x)) > 0,]
write.csv(NAdata, "~/Documents/Coding/winningmusic/NAdata.csv", row.names=FALSE)



###############################################################################
###############################################################################



tracks <- read.csv("tracks.csv")
topsongs <- read.csv("billboard_top_songs.csv")

#Big dataset: 1921 - 2020: Annie  

#Only take 1985- 2015
tracks <- tracks %>% separate(col = release_date, sep = "-", into = c("year", "month", "day")) %>% 
  filter(year >= 1985 & year < 2016) %>% 
  transform(year = as.numeric(year)) %>% select(-month, -day) %>% distinct(name, year, .keep_all = TRUE)

#Merge that with Billboard Top 100, 1985 - 2015
sample <- topsongs %>% full_join(tracks, by = c("title" = "name", "year" = "year")) #%>% distinct(title, year, .keep_all = TRUE)


#Put billboard_hits as a dummy variable (whether the track appeared on the Billboard Hot 100 chart (1=hit, 0=non-hit).)
sample <- sample %>% mutate(hit = ifelse(is.na(position), 0, 1))

write.csv(sample, "big_data.csv", row.names = FALSE)