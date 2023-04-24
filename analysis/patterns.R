library(tidyverse)
library(ClusterR) # new for kmeans++
library(foreach)
library(mosaic)
library(lubridate)
library(fastDummies)
library(tibble)
library(ggcorrplot)
library(parallel)
library(rsample) 
library(caret)
library(tidyr)

#Changing data integer to numeric types
all_data$danceability <- as.numeric(all_data$danceability)
all_data$energy <- as.numeric(all_data$energy)
all_data$key <- as.numeric(all_data$key)
all_data$loudness <- as.numeric(all_data$loudness)
all_data$difficult_words <- as.numeric(all_data$difficult_words)
all_data$num_dupes <- as.numeric(all_data$num_dupes)
all_data$num_words <- as.numeric(all_data$num_words)
all_data$num_lines <- as.numeric(all_data$num_lines)
all_data$duration_ms <- as.numeric(all_data$duration_ms)
all_data$mode <- as.numeric(all_data$mode)
all_data$time_signature <- as.numeric(all_data$time_signature)

## 1960s Analysis
#keep analysis columns
cs <- all_data[,-(33)]
cs <- cs[,-(18:19)]
cs <- cs[,-(4:5)]
cs <- cs[,-(1:2)]
cs <- na.omit(cs) # must drop na observations
cs <- cs[cs$position<=10,] #Top 10 only
X <- cs[,-(1)] # Drop position

# Center and scale the data
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

#Correlation matrix
corr_matrix <- cor(X)
ggcorrplot(corr_matrix)

#Music by decade
billboard_60s = subset(all_data, year %in% c("1960", "1961", "1962", "1963", "1964", "1965", "1966", "1967", "1968", "1969"))

#keep analysis columns
cs_60s <- billboard_60s[,-(33)]
cs_60s <- cs_60s[,-(18:19)]
cs_60s <- cs_60s[,-(4:5)]
cs_60s <- cs_60s[,-(1:2)]
cs_60s <- na.omit(cs_60s) # must drop na observations
cs_60s <- cs_60s[cs_60s$position<=10,] #Top 10 only
X_60s <- cs_60s[,-(1)] # Drop position

# Center and scale the data
X_60s = scale(X_60s, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu_60s = attr(X_60s,"scaled:center")
sigma_60s = attr(X_60s,"scaled:scale")

#Num_words num_syllables num_lines are highly correlated. 
X_60s = X_60s[,-(12)]
X_60s = X_60s[,-(8)]

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X_60s = X_60s[,-(3:4)]
corr_matrix_60s <- cor(X_60s)

#Trying PCA
pc_sm_60s = prcomp(X_60s, rank=10, center = TRUE, scale=TRUE)

loadings_summary_60s = pc_sm_60s$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
pca1_60s=loadings_summary_60s %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
pca2_60s=loadings_summary_60s %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
pca3_60s=loadings_summary_60s %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

# Run k-means with 3 clusters and 25 starts
clust_60s = kmeans(X_60s, 3, nstart=25)
scores_60s = pc_sm_60s$x
Y_60s = as.data.frame(cbind(X_60s, scores_60s))
cluster_60s = ggplot(Y_60s) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust_60s$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()
# Examine specific songs
cluster1_songs_60s=which(clust_60s$cluster == 1) 
cluster2_songs_60s=which(clust_60s$cluster == 2) 
cluster3_songs_60s=which(clust_60s$cluster == 3)

loadings_summary_60s[c("Category","PC1","PC2","PC3")]

## 1970s Analysis
#Music by decade
billboard_70s = subset(all_data, year %in% c("1970", "1971", "1972", "1973", "1974", "1975", "1976", "1977", "1978", "1979"))

#keep analysis columns
cs_70s <- billboard_70s[,-(33)]
cs_70s <- cs_70s[,-(18:19)]
cs_70s <- cs_70s[,-(4:5)]
cs_70s <- cs_70s[,-(1:2)]
cs_70s <- na.omit(cs_70s) # must drop na observations
cs_70s <- cs_70s[cs_70s$position<=10,] #Top 10 only
X_70s <- cs_70s[,-(1)] # Drop position

# Center and scale the data
X_70s = scale(X_70s, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu_70s = attr(X_70s,"scaled:center")
sigma_70s = attr(X_70s,"scaled:scale")

#Num_words num_syllables num_lines are highly correlated. 
X_70s = X_70s[,-(12)]
X_70s = X_70s[,-(8)]

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X_70s = X_70s[,-(3:4)]
corr_matrix_70s <- cor(X_70s)

#Trying PCA
pc_sm_70s = prcomp(X_70s, rank=10, center = TRUE, scale=TRUE)

loadings_summary_70s = pc_sm_70s$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
pca1_70s=loadings_summary_70s %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
pca2_70s=loadings_summary_70s %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
pca3_70s=loadings_summary_70s %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

# Run k-means with 3 clusters and 25 starts
clust_70s = kmeans(X_70s, 3, nstart=25)
scores_70s = pc_sm_70s$x
Y_70s = as.data.frame(cbind(X_70s, scores_70s))
cluster_70s = ggplot(Y_70s) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust_70s$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()
# Examine specific songs
cluster1_songs_70s=which(clust_70s$cluster == 1) 
cluster2_songs_70s=which(clust_70s$cluster == 2) 
cluster3_songs_70s=which(clust_70s$cluster == 3)

loadings_summary_70s[c("Category","PC1","PC2","PC3")]

## 1980s Analysis
#Music by decade
billboard_80s = subset(all_data, year %in% c("1980", "1981", "1982", "1983", "1984", "1985", "1986", "1987", "1988", "1989"))

#keep analysis columns
cs_80s <- billboard_80s[,-(33)]
cs_80s <- cs_80s[,-(18:19)]
cs_80s <- cs_80s[,-(4:5)]
cs_80s <- cs_80s[,-(1:2)]
cs_80s <- na.omit(cs_80s) # must drop na observations
cs_80s <- cs_80s[cs_80s$position<=10,] #Top 10 only
X_80s <- cs_80s[,-(1)] # Drop position

# Center and scale the data
X_80s = scale(X_80s, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu_80s = attr(X_80s,"scaled:center")
sigma_80s = attr(X_80s,"scaled:scale")

#Num_words num_syllables num_lines are highly correlated. 
X_80s = X_80s[,-(12)]
X_80s = X_80s[,-(8)]

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X_80s = X_80s[,-(3:4)]
corr_matrix_80s <- cor(X_80s)
X_80s = X_80s[,-(21)] #No variance in time_signature
#Trying PCA
pc_sm_80s = prcomp(X_80s, rank=10, center = TRUE, scale=TRUE)

loadings_summary_80s = pc_sm_80s$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
pca1_80s=loadings_summary_80s %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
pca2_80s=loadings_summary_80s %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
pca3_80s=loadings_summary_80s %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

# Run k-means with 3 clusters and 25 starts
clust_80s = kmeans(X_80s, 3, nstart=25)
scores_80s = pc_sm_80s$x
Y_80s = as.data.frame(cbind(X_80s, scores_80s))
cluster_80s = ggplot(Y_80s) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust_80s$cluster)),
                                         size=3, col="#7f7f7f", shape=21) + theme_bw()
# Examine specific songs
cluster1_songs_80s=which(clust_80s$cluster == 1) 
cluster2_songs_80s=which(clust_80s$cluster == 2) 
cluster3_songs_80s=which(clust_80s$cluster == 3)

## 1990s Analysis
#Music by decade
billboard_90s = subset(all_data, year %in% c("1990", "1991", "1992", "1993", "1994", "1995", "1996", "1997", "1998", "1999"))

#keep analysis columns
cs_90s <- billboard_90s[,-(33)]
cs_90s <- cs_90s[,-(18:19)]
cs_90s <- cs_90s[,-(4:5)]
cs_90s <- cs_90s[,-(1:2)]
cs_90s <- na.omit(cs_90s) # must drop na observations
cs_90s <- cs_90s[cs_90s$position<=10,] #Top 10 only
X_90s <- cs_90s[,-(1)] # Drop position

# Center and scale the data
X_90s = scale(X_90s, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu_90s = attr(X_90s,"scaled:center")
sigma_90s = attr(X_90s,"scaled:scale")

#Num_words num_syllables num_lines are highly correlated. 
X_90s = X_90s[,-(12)]
X_90s = X_90s[,-(8)]

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X_90s = X_90s[,-(3:4)]
corr_matrix_90s <- cor(X_90s)

#Trying PCA
pc_sm_90s = prcomp(X_90s, rank=10, center = TRUE, scale=TRUE)

loadings_summary_90s = pc_sm_90s$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
pca1_90s=loadings_summary_90s %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
pca2_90s=loadings_summary_90s %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
pca3_90s=loadings_summary_90s %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

# Run k-means with 3 clusters and 25 starts
clust_90s = kmeans(X_90s, 3, nstart=25)
scores_90s = pc_sm_90s$x
Y_90s = as.data.frame(cbind(X_90s, scores_90s))
cluster_90s = ggplot(Y_90s) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust_90s$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()
# Examine specific songs
cluster1_songs_90s=which(clust_90s$cluster == 1) 
cluster2_songs_90s=which(clust_90s$cluster == 2) 
cluster3_songs_90s=which(clust_90s$cluster == 3)

loadings_summary_90s[c("Category","PC1","PC2","PC3")]

## 2000s Analysis
#Music by decade
billboard_00s = subset(all_data, year %in% c("2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", "2009"))

#keep analysis columns
cs_00s <- billboard_00s[,-(33)]
cs_00s <- cs_00s[,-(18:19)]
cs_00s <- cs_00s[,-(4:5)]
cs_00s <- cs_00s[,-(1:2)]
cs_00s <- na.omit(cs_00s) # must drop na observations
cs_00s <- cs_00s[cs_00s$position<=10,] #Top 10 only
X_00s <- cs_00s[,-(1)] # Drop position

# Center and scale the data
X_00s = scale(X_00s, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu_00s = attr(X_00s,"scaled:center")
sigma_00s = attr(X_00s,"scaled:scale")

#Num_words num_syllables num_lines are highly correlated. 
X_00s = X_00s[,-(12)]
X_00s = X_00s[,-(8)]

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X_00s = X_00s[,-(3:4)]
corr_matrix_00s <- cor(X_00s)

#Trying PCA
pc_sm_00s = prcomp(X_00s, rank=10, center = TRUE, scale=TRUE)

loadings_summary_00s = pc_sm_00s$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
pca1_00s=loadings_summary_00s %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
pca2_00s=loadings_summary_00s %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
pca3_00s=loadings_summary_00s %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

# Run k-means with 3 clusters and 25 starts
clust_00s = kmeans(X_00s, 3, nstart=25)
scores_00s = pc_sm_00s$x
Y_00s = as.data.frame(cbind(X_00s, scores_00s))
cluster_00s = ggplot(Y_00s) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust_00s$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()
# Examine specific songs
cluster1_songs_00s=which(clust_00s$cluster == 1) 
cluster2_songs_00s=which(clust_00s$cluster == 2) 
cluster3_songs_00s=which(clust_00s$cluster == 3)

loadings_summary_00s[c("Category","PC1","PC2","PC3")]
