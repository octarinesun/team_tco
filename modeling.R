setwd("~/R Projects/kaggle-competitions/home-credit-default-risk/team_tco")
library(caret)
library(doParallel)
library(pROC)
library(dplyr)
library(magrittr)

df <- readRDS(file = "dat/df_processed.rds")
#df <- readRDS(file = "dat/df_test_processed.rds")

#keepFeatures <- selectFeatures(df)
#saveRDS(keepFeatures, file = "models/keepFeatures.p4.rds")
keepFeatures <- readRDS(file = "models/keepFeatures.p4.rds")
factors <- df[,keepFeatures$factors]
factors <- lapply(factors[,-factors$SK_ID_CURR], as.factor) %>% as.data.frame
factors$SK_ID_CURR <- as.numeric(as.character(factors$SK_ID_CURR))
numerics <- df[,keepFeatures$numerics]

# Select these features in the training set.
df <- left_join(numerics, factors, by = "SK_ID_CURR")

rm(numerics, factors)

# Split into testing and training sets; do pre-processing on training set
set.seed(232)
inTrain <- createDataPartition(y = df$TARGET, p = 0.5, list = FALSE)
training <- df[inTrain,]
testing <- df[-inTrain,]
rm(df)

## Downsample the negative class so that the split is 80/20 instead of 92/8
negInd <- which(training$TARGET == 0)
posInd <- which(training$TARGET == 1)
targetNum <- 4 * length(posInd)
set.seed(232)
negInd <- sample(negInd, size = targetNum)
training <- training[c(negInd, posInd),]

preProc1 <- preProcess(training[,-c(1:2)], 
                            method = c("medianImpute"), 
                            verbose = TRUE)

trainingProcessed <- predict(preProc1, training[,-c(1:2)])

preProc2 <- preProcess(trainingProcessed, 
                            method = c("center", "scale"), 
                            verbose = TRUE)

trainingProcessed <- predict(preProc2, trainingProcessed)
trainingProcessed <- cbind(training[,1:2], trainingProcessed)

## IN ORDER TO DO XGB, MUST CONVERT SOME ALL FACTOR COLUMNS TO NUMERICS. THAT MEANS SOME NEED TO BE ONE-HOT ENCODED.
factors <- select(trainingProcessed, which(sapply(trainingProcessed, is.factor)))
factorLevs <- apply(factors, 2, table)

# Change 0 and 1 to Yes and No, respectively
trainingProcessed[,2] <- as.factor(trainingProcessed[,2])
levels(trainingProcessed[,2]) <- c("Yes", "No")
#saveRDS(trainingProcessed, "dat/trainingProcessed3.RDS")

# Process the testing data in the same way
# Keep features
# preProc1 - impute
# preProc2 - center/scale
factorsTest <- testing[,keepFeatures$factors]
factorsTest <- lapply(factorsTest, as.factor) %>% as.data.frame
factorsTest$SK_ID_CURR <- as.numeric(as.character(factorsTest$SK_ID_CURR))
numericsTest <- testing[,keepFeatures$numerics]
testing <- left_join(numericsTest, factorsTest, by = "SK_ID_CURR")
rm(factorsTest, numericsTest)

preProcTest1 <- predict(preProc1, newdata = testing[,-c(1:2)])
preProcTest2 <- predict(preProc2, newdata = preProcTest1)
testingProcessed <- cbind(testing[,1:2], preProcTest2)
testingProcessed[,2] <- as.factor(testingProcessed[,2])
levels(testingProcessed[,2]) <- c("Yes", "No")
rm(preProcTest1, preProcTest2)


###### Modeling ######

cl <- makePSOCKcluster(2)
registerDoParallel(cl)

ctrl <- trainControl(method = "repeatedcv", 
                     repeats = 5, 
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE,
                     allowParallel = TRUE,
                     trim = TRUE,
                     returnData = FALSE)

t.xgb1 <- system.time(
      mdl.xgb1 <- train(x = trainingProcessed[,3:length(trainingProcessed)],
                        y = trainingProcessed[,2], 
                        method = "xgbTree", 
                        metric = "ROC",
                        trControl = ctrl)
)
saveRDS(mdl.xgb1, file = "models/mdl.xgb1.rds")

stopCluster(cl)

# Use the model on the processed testing data

test.pred <- predict(mdl.xgb1, testingProcessed)
levels(test.pred) <- c("Yes","No")
confusionMatrix(test.pred, testingProcessed[,2])

test.pred.prob <- predict(mdl.xgb1, testingProcessed, type = "prob")
result.roc <- roc(testingProcessed[,2], test.pred.prob$No)
plot(result.roc, print.thres="best", 
     print.thres.best.method="closest.topleft",
     print.auc = TRUE)

