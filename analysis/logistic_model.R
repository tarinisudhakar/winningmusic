library(dplyr)
library(rsample)
library(modelr)
library(tidyr)
library(pROC)
library(ROCR)
library(caTools)

## CLASSIFICATION DIAGNOSIS
class_diag <- function(score, truth, model, test_set, positive, cutoff=.5){
  pred <- factor(score>cutoff,levels=c("TRUE","FALSE"))
  truth <- factor(truth==positive, levels=c("TRUE","FALSE"))
  tab<-table(truth, pred)
  acc=sum(diag(tab))/sum(tab)
  ppv=tab[1,1]/colSums(tab)[1]
  rmse_ = rmse(model, test_set)
  
  #CALCULATE EXACT AUC
  truth<-as.numeric(truth=="TRUE")
  ord<-order(score, decreasing=TRUE)
  score <- score[ord]; truth <- truth[ord]
  TPR=cumsum(truth)/max(1,sum(truth))
  FPR=cumsum(!truth)/max(1,sum(!truth))
  dup<-c(score[-1]>=score[-length(score)], FALSE)
  TPR<-c(0,TPR[!dup],1); FPR<-c(0,FPR[!dup],1)
  n <- length(TPR)
  auc<- sum( ((TPR[-1]+TPR[-n])/2) * (FPR[-1]-FPR[-n]) )
  round(data.frame(acc, ppv, rmse_, auc, row.names = "Metrics"),5)
}


data_hits <- read.csv("hits.csv") %>% select(-X) %>% drop_na(duration_ms)

data_hits %>% filter(is.na(duration_ms)) %>% summarise(n())

hits_split = initial_split(data_hits, prop=0.8)
hits_train = training(hits_split)
hits_test = testing(hits_split)

model1 = glm(hit ~ duration_ms + danceability + energy + key + loudness + speechiness + 
               acousticness + instrumentalness + liveness + valence + tempo + time_signature, 
             data = hits_train, family = "binomial")

# logistic regression model 1
prob_model1 = predict(model1, newdata = hits_test, type = "response")
# out-of-sample accuracy
class_diag(prob_model1, hits_test$hit, model1, hits_test, positive = 1)

ROCR_pred_test <- prediction(prob_model1,hits_test$hit)
cost_perf = performance(ROCR_pred_test, "cost") 
ROCR_pred_test@cutoffs[[1]][which.min(cost_perf@y.values[[1]])]




# FORWARD SELECTION PROCESS
lm0 = glm(hit ~ acousticness + instrumentalness + danceability + speechiness + 
            liveness + time_signature + valence + loudness + energy + 
            acousticness:speechiness + acousticness:danceability + liveness:valence + 
            valence:loudness + speechiness:loudness + acousticness:energy + 
            acousticness:loudness + valence:energy + loudness:energy, data=hits_train, family = "binomial")
model3 = step(lm0, direction = "forward",
              scope = ~(duration_ms + danceability + energy + key + loudness + 
                          speechiness + acousticness + instrumentalness + 
                          liveness + valence + tempo + time_signature)^2)
prob_model3 = predict(lm0, newdata = hits_test, type= "response")
class_diag(prob_model3, hits_test$hit, lm0, hits_test, positive = 1)


model4 = glm(hit ~ acousticness + instrumentalness + danceability + speechiness + 
               liveness + time_signature + valence + loudness + energy + 
               tempo + duration_ms + key + acousticness:speechiness + acousticness:danceability + 
               liveness:valence + valence:loudness + speechiness:loudness + 
               acousticness:energy + acousticness:loudness + valence:energy + 
               loudness:energy + acousticness:time_signature + instrumentalness:valence + 
               acousticness:instrumentalness + speechiness:liveness + danceability:time_signature + 
               danceability:valence + danceability:tempo + speechiness:tempo + 
               danceability:energy + danceability:loudness + speechiness:energy + 
               loudness:tempo + acousticness:liveness + danceability:speechiness + 
               acousticness:duration_ms + danceability:duration_ms + loudness:duration_ms + 
               instrumentalness:duration_ms + acousticness:valence + speechiness:duration_ms + 
               liveness:key + energy:key + instrumentalness:speechiness + 
               liveness:energy, 
             data=hits_train, family = "binomial")
prob_model4 = predict(model4, newdata = hits_test, type= "response")
# out-of-sample accuracy
class_diag(prob_model4, hits_test$hit, model4, hits_test, positive = 1)




###############################################################
## MODEL VALIDATION

prob_model1 = predict(model1, newdata = hits_test, type = "response")
roc_model1 <- roc(hits_test$hit, prob_model1)
ggroc(roc_model1) + labs(title ="ROC Curve")


## CROSS-VALIDATION
k = 5
folds <- rep(1:k, length.out = nrow(data_hits))
diags <- NULL
pred_hit <- c()
actual_hit <- c()

i = 1
for (i in 1:k) {
  train <- data_hits[folds != i, ]
  test <- data_hits[folds == i, ]
  truth <- test$hit
  
  fit <- glm(hit == 1 ~ duration_ms + danceability + energy + key + loudness + speechiness + 
               acousticness + instrumentalness + liveness + valence + tempo + time_signature, 
             data = train, family = "binomial")
  probs <- predict(fit, newdata = test, type = "response")
  #probs <- ifelse(probs > 0.5, 1, 0)
  diags <- rbind(diags, class_diag(probs, truth, fit, test, positive = 1))
  pred_hit <- append(pred_hit, sum(probs))
  actual_hit <- append(actual_hit, sum(truth))
}
cv_diags <- cbind(diags, pred_hit, actual_hit)
cv_diags
summarize_all(diags, mean)

