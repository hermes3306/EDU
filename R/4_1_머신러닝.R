
## 분류 문제 해결

# load packages
pkgs <- c("tidyverse", "caret", "doParallel", "pROC")
sapply(pkgs, require, character.only = T)


# load data
library(mlbench)
data(Sonar)
glimpse(Sonar)
table(Sonar$Class) %>% prop.table()


# missing values
sapply(Sonar, function(x) sum(is.na(x)))
Sonar %>% is.na() %>% sum()


# check duplicate
Sonar %>% duplicated() %>% sum()
Sonar %>% distinct() %>% count()


# train vs. test data
set.seed(1)
ind <- createDataPartition(Sonar$Class, p = 0.7, list = F) 
train <- Sonar[ ind, ]
test  <- Sonar[-ind, ]


# 데이터 비율 확인
table(Sonar$Class) %>% prop.table()
table(train$Class) %>% prop.table()
table(test$Class) %>% prop.table()

# my_list <- list(Sonar, train, test)
# my_list %>% 
#   map(~ table(.x[["Class"]])) %>% 
#   map(~ prop.table(.x)) %>% 
#   map_df(~ as.data.frame(.x))


# 10-fold cross-validation
ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     classProbs = T,
                     summaryFunction = twoClassSummary)


## single decision tree
modelLookup("rpart")
grid_rpt <- expand.grid(cp = c(0.001, 0.005, 0.01, 0.1))

sonar_rpt <- train(Class ~ ., 
                   data = train,
                   method = "rpart",
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   trControl = ctrl,
                   tuneGrid = grid_rpt)


## random Forest
detectCores()
registerDoParallel(detectCores())

modelLookup("rf")
grid_rf <- expand.grid(mtry = c(3, 4, 7, 14))
# 회귀 : p / 3
# 분류 : sqrt(p), sqrt(p) * 1/2, sqrt(p) * 2

sonar_rf <- train(Class ~ ., 
                  data = train,
                  method = "rf",
                  metric = "ROC",
                  preProcess = c("center", "scale"),
                  trControl = ctrl,
                  tuneGrid = grid_rf,
                  ntree = 500)


## gbm (gradient boosting)
modelLookup("gbm")
grid_gbm <- expand.grid(n.trees = 500,
                        interaction.depth  = c(5, 10, 30),
                        shrinkage   = c(0.01, 1),
                        n.minobsinnode = 30)

sonar_gbm <- train(Class ~ ., 
                   data = train,
                   method = "gbm",
                   metric = "ROC",
                   preProcess = c("center", "scale"),
                   trControl = ctrl,
                   tuneGrid = grid_gbm,
                   verbose = F)


## neural network (shallow neural network)
modelLookup("nnet")

nnet_Grid <- expand.grid(size = c(3, 5, 10, 15),
                         decay = c(0.01, 0.1),
                         bag = F)

maxSize <- max(nnet_Grid$size)

numWts <- 5 * (maxSize * (ncol(train) + 1) + 5 + 1)

snn <- train(Class ~ ., 
             data = train,
             method     = "avNNet",
             metric     = "ROC",
             trControl  = ctrl,
             preProcess = c("center", "scale"),
             tuneGrid   = nnet_Grid,
             maxit      = 500,
             linout     = F,
             trace      = F,
             MaxNWts    = numWts)


## compare models
results <- resamples(
  list(rp  = sonar_rpt,
  rf  = sonar_rf,
  gbm = sonar_gbm,
  snn = snn))

summary(results)


## predict test data
preds <- predict(sonar_rf, test)
confusionMatrix(preds, test$Class, positive = "M")


## Graphs
ggplot(sonar_rf)
plot(varImp(sonar_rf))

pred_roc <- predict(sonar_rf, test, type = "prob")
roc_curve <- roc(test$Class, pred_roc$M)
plot(roc_curve) 
