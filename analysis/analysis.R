##Analysing trends in Billboard Top 100
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


#Seeing an overall shift in music
data <- read.csv("~/Documents/Coding/winningmusic/data/work.csv")
alltracks <- read.csv("~/Documents/Coding/winningmusic/scrape/tracks.csv")

#Cleaning data
#Dropping uri, track href, analysis url
#cs <- data[,-(30:34)]

#Changing data integer to numeric types
data$danceability <- as.numeric(data$danceability)
data$energy <- as.numeric(data$energy)
data$key <- as.numeric(data$key)
data$loudness <- as.numeric(data$loudness)
data$difficult_words <- as.numeric(data$difficult_words)
data$num_dupes <- as.numeric(data$num_dupes)
data$num_words <- as.numeric(data$num_words)
data$num_lines <- as.numeric(data$num_lines)
data$popularity <- as.numeric(data$popularity)
data$duration_ms <- as.numeric(data$duration_ms)
data$mode <- as.numeric(data$mode)
data$explicit <- as.numeric(data$explicit)
data$time_signature <- as.numeric(data$time_signature)

#Dropping extra artist, id columns 
cs <- data[,-(22:24)]
cs <- cs[,-(17:18)]

#Popularity density for Billboard - most songs are popular as expected
cs %>%
  ggplot(aes(x=popularity)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

#Popularity density for Billboard - most songs are less than 50 in popularity // 
#Need to correct for range of years

alltracks %>%
#  select(2015>=release_date>=1985)
  ggplot( aes(x=popularity)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

#Checking for correlation within Billboard data
# Center and scale the data
X = cs[,-(1:4)]
X = scale(X, center=TRUE, scale=TRUE)

# Extract the centers and scales from the rescaled data (which are named attributes)
mu = attr(X,"scaled:center")
sigma = attr(X,"scaled:scale")

#Correlation matrix - can see correlation among variables: 
#acousticness and energy are negatively correlated
#energy and loudness are positively correlated // matches with the reference
#not a clear correlation for popularity with other variables 
corr_matrix <- cor(X)
ggcorrplot(corr_matrix)


#Num_words num_syllables num_lines are highly correlated. 
#We should only use one of num_words or num_syllables or num_lines. I suggest num_words. 
#Trying again after dropping the other two. 
X = X[,-(8)]
X = X[,-(11)]
corr_matrix <- cor(X)
ggcorrplot(corr_matrix, sig.level=0.05, lab_size = 4.5, p.mat = NULL,
           insig = c("pch", "blank"), pch = 1, pch.col = "black", pch.cex =1,
           tl.cex = 14) +
  theme(axis.text.x = element_text(margin=margin(-2,0,0,0)),
  axis.text.y = element_text(margin=margin(0,-2,0,0)),
  panel.grid.minor = element_line(size=10)) + 
  geom_tile(fill="white") +
  geom_tile(height=0.8, width=0.8)

#Sentiment is also highly correlated with itself. Dropping compound_sentiment and neutral_sentiment. 
X = X[,-(3:4)]
corr_matrix <- cor(X)
ggcorrplot(corr_matrix)

#Trying PCA
pc_sm = prcomp(X, rank=10, center = TRUE, scale=TRUE)

# overall variation in 256 original features
summary(pc_sm)
plot(pc_sm)

loadings_summary = pc_sm$rotation %>%
  as.data.frame() %>%
  rownames_to_column('Category')

#PC1
loadings_summary %>%
  select(Category, PC1) %>%
  arrange(desc(PC1))

#PC2
loadings_summary %>%
  select(Category, PC2) %>%
  arrange(desc(PC2))

#PC3
loadings_summary %>%
  select(Category, PC3) %>%
  arrange(desc(PC3))

#PC4
loadings_summary %>%
  select(Category, PC4) %>%
  arrange(desc(PC4))

#PC5
loadings_summary %>%
  select(Category, PC5) %>%
  arrange(desc(PC5))


# Run k-means with 6 clusters and 25 starts
clust1 = kmeans(X, 5, nstart=25)

scores = pc_sm$x

Y <- as.data.frame(cbind(X, scores))

ggplot(Y) + geom_point(aes(x=PC2, y=PC4, fill=factor(clust1$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()

ggplot(Y) + geom_point(aes(x=PC1, y=PC5, fill=factor(clust1$cluster)),
                       size=3, col="#7f7f7f", shape=21) + theme_bw()

which(clust1$cluster == 1) #Pop but with more words?: Britney Spears, Celine Dion, Boyz II Men 
which(clust1$cluster == 2) #Pop: Taylor Swift and Britney Spears
which(clust1$cluster == 3) #Smashing Pumpkins, Cyndi Lauper
which(clust1$cluster == 4) #Rap: 50 Cent, Lil Wayne 
which(clust1$cluster == 5) #Seems more towards rock and instruments: it does contain Miley Cyrus - 7 things (a real banger ngl) lol 

#Predicting popularity using KNN
#Split data into training and test
cs_split <- initial_split(cs, prop. = 0.8)
cs_train <- training(cs_split)
cs_test <- testing(cs_split)

knn_model = knnreg(popularity ~ .,
                   data = cs_train, k = 5)
cat("In-sample RMSE for K = 5: ", modelr::rmse(knn_model, cs_train))
cat("Out-of-sample RMSE for K = 5: ", modelr::rmse(knn_model, cs_test))

knn_model = knnreg(popularity ~ .,
                   data = cs_train, k = 10)
cat("In-sample RMSE for K = 10: ", modelr::rmse(knn_model, cs_train))
cat("Out-of-sample RMSE for K = 10: ", modelr::rmse(knn_model, cs_test))

knn_model = knnreg(popularity ~ .,
                   data = cs_train, k = 20)
cat("In-sample RMSE for K = 20: ", modelr::rmse(knn_model, cs_train))
cat("Out-of-sample RMSE for K = 20: ", modelr::rmse(knn_model, cs_test))

knn_model = knnreg(popularity ~ .,
                   data = cs_train, k = 30)
cat("In-sample RMSE for K = 30: ", modelr::rmse(knn_model, cs_train))
cat("Out-of-sample RMSE for K = 30: ", modelr::rmse(knn_model, cs_test))


