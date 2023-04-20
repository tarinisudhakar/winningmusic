library(tidyverse)

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

