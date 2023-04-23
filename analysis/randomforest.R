##Analysing trends in Billboard Top 100
library(tidyverse)
library(ClusterR) # new for kmeans++
library(foreach)
library(mosaic)
library(lubridate)
library(tibble)
library(ggcorrplot)
library(parallel)
library(rsample) 
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(doParallel)
library(caret)
library(pROC)
library(pdp)

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

loadings_summary %>%
  select(Category, PC1) %>%
  arrange(PC1)

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

#Code for random forests 
#Splitting into training and testing data
hits <- read.csv("~/Documents/Coding/winningmusic/hits.csv")
  
bb100_split <- initial_split(hits, prop. = 0.8)
bb100_train <- training(bb100_split)
bb100_test <- testing(bb100_split)


# let's fit a single tree
bb100.tree = rpart(hit ~ danceability + energy + key + loudness + mode + 
                     speechiness + acousticness + instrumentalness + liveness +
                     valence + tempo + duration_ms,
                   data=bb100_train, na.action = na.omit, control = rpart.control(cp = 0.00001))

cat("In-sample RMSE: ", modelr::rmse(bb100.tree, bb100_train))
cat("Out-of-sample RMSE: ", modelr::rmse(bb100.tree, bb100_test))

##Random forests for 10 trees
cl <- makeCluster(4)
registerDoParallel(cl)
ptm <- proc.time()
bb100.forest10 <- foreach(ntree=rep(10, 1), .combine=combine, .packages='randomForest') %dopar%
  randomForest(hit ~ danceability + energy + key + loudness + mode + 
                 speechiness + acousticness + instrumentalness + liveness +
                 valence + tempo + duration_ms,  data=bb100_train, 
               na.action = na.omit, importance = TRUE, ntree=ntree, keep.inbag = TRUE)
bb100.forest10
proc.time() - ptm
stopCluster(cl)

cat("In-sample RMSE: ", modelr::rmse(bb100.forest10, bb100_train))
cat("Out-of-sample RMSE: ", modelr::rmse(bb100.forest10, bb100_test))

#Confusion matrix for 10 trees
phat_train_bb10 = predict(bb100.forest10, bb100_train) 
yhat_train_bb10 = ifelse(phat_train_bb10 > 0.01, 1, 0) 
confusion_in10 = table(y = bb100_train$hit, yhat = yhat_train_bb10) 
confusion_in10 #0.4254017 in train set
cat("In-sample sensitivity: ", 1112/(1112+1502))

phat_test_bb10 = predict(bb100.forest10, bb100_test) 
yhat_test_bb10 = ifelse(phat_test_bb10 > 0.01, 1, 0) 
confusion_out10 = table(y = bb100_test$hit, yhat = yhat_test_bb10) 
confusion_out10 #0.03981265 in test set
cat("Out-of-sample sensitivity: ", 202/(652+202))

##Random forests for 20 trees
cl <- makeCluster(4)
registerDoParallel(cl)
ptm <- proc.time()
bb100.forest20 <- foreach(ntree=rep(10, 2), .combine=combine, .packages='randomForest') %dopar%
  randomForest(hit ~ danceability + energy + key + loudness + mode + 
                 speechiness + acousticness + instrumentalness + liveness +
                 valence + tempo + duration_ms,  data=bb100_train, 
               na.action = na.omit, importance = TRUE, ntree=ntree, keep.inbag = TRUE)
bb100.forest20
proc.time() - ptm
stopCluster(cl)

ensemble20 <- combine(bb100.forest20)

cat("In-sample RMSE: ", modelr::rmse(ensemble20, bb100_train))
cat("Out-of-sample RMSE: ", modelr::rmse(ensemble20, bb100_test))

#Confusion matrix for 20 trees
phat_train_bb20 = predict(ensemble20, bb100_train) 
yhat_train_bb20 = ifelse(phat_train_bb20 > 0.01, 1, 0) 
confusion_in20 = table(y = bb100_train$hit, yhat = yhat_train_bb20) 
confusion_in20 #0.4433818 in train set
cat("In-sample sensitivity: ", 1159/(1159+1455))

phat_test_bb20 = predict(ensemble20, bb100_test) 
yhat_test_bb20 = ifelse(phat_test_bb20 > 0.01, 1, 0) 
confusion_out20 = table(y = bb100_test$hit, yhat = yhat_test_bb20) 
confusion_out20 #0.04566745 in test set
cat("Out-of-sample sensitivity: ", 262/(262+592))

# now a random forest for 300 trees
bb100.forest = randomForest(hit ~ danceability + energy + key + loudness + mode + 
                              speechiness + acousticness + instrumentalness + liveness +
                              valence + tempo + duration_ms,
                            data=bb100_train, ntree=300, importance = TRUE, 
                            na.action = na.omit)
plot(bb100.forest)

# let's compare RMSE on the train set
cat("In-sample RMSE: ", sqrt(bb100.forest$mse[length(bb100.forest$mse)]))

# let's compare RMSE on the test set
cat("Out-of-sample RMSE: ", modelr::rmse(bb100.forest, bb100_test))

#Confusion matrix for 300 trees
phat_train_bb1 = predict(bb100.forest, bb100_train) 
yhat_train_bb1 = ifelse(phat_train_bb1 > 0.5, 1, 0) 
confusion_in300 = table(y = bb100_train$hit, yhat = yhat_train_bb1) 
confusion_in300 #51.75% in train set
cat("In-sample sensitivity: ", 1353/(1353+1261))

phat_test_bb1 = predict(bb100.forest, bb100_test) 
yhat_test_bb1 = ifelse(phat_test_bb2 > 0.5, 1, 0) 
confusion_out300 = table(y = bb100_test$hit, yhat = yhat_test_bb1) 
confusion_out300 #5.1% in test set
cat("Out-of-sample sensitivity: ", 44/(44+810))

#Partial dependence and variable dependence
vi = varImpPlot(bb100.forest, type=1)

#ROC for random forests
ROC_rf <- roc(bb100_test$hit, phat_test_bb10)
ROC_lr <- roc(bb100_test$hit, lr_prediction)
ROC_rf_auc <- auc(ROC_rf)
ROC_lr_auc <- auc(ROC_lr)
plot(ROC_rf, col = "green", main = "ROC For Random Forest (GREEN) vs Logistic Regression (RED)")
lines(ROC_lr, col = "red")

paste("Accuracy % of random forest: ", mean(bb100_test$hit == round(phat_test_bb10, digits = 0)))
paste("Accuracy % of logistic regression: ", mean(bb100_test$hit == round(lr_prediction, digits = 0)))
paste("Area under curve of random forest: ", ROC_rf_auc)
paste("Area under curve of logistic regression: ", ROC_lr_auc)