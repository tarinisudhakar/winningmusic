##Analysing trends in Billboard Top 100
library(tidyverse)
library(ClusterR) # new for kmeans++
library(foreach)
library(mosaic)


#Seeing an overall shift in music
data <- read.csv("~/Documents/Coding/winningmusic/data/out5.csv")

summary(data)

#Cleaning data
#Dropping uri, track href, analysis url
cs <- data[,-(30:34)]


data$danceability <- as.numeric(data$danceability)
data$energy <- as.numeric(data$energy)
data$key <- as.numeric(data$key)
data$loudness <- as.numeric(data$loudness)
data$difficult_words <- as.numeric(data$difficult_words)
data$num_dupes <- as.numeric(data$num_dupes)
data$num_words <- as.numeric(data$num_words)
data$num_lines <- as.numeric(data$num_lines)


# Center and scale the data
X = cs[,-(1:5)]
X = X[,-(13)]
X = scale(X, center=TRUE, scale=TRUE)
X = na.omit(X)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")


# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(X, 6, nstart=25)

ggplot(data) + geom_point(aes(x=speechiness, y=instrumentalness, fill=factor(clust1$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw(base_family="Helvetica")
