library(gbm)
library(caret)

# Use the model on the test data for submission
testing <- readRDS(file = "dat/df_test_processed.rds")
keepFeaturesValidation <- vector("list")
keepFeaturesValidation$numerics <- keepFeatures$numerics[keepFeatures$numerics != "TARGET"]
keepFeaturesValidation$factors <- keepFeatures$factors[keepFeatures$factors != "TARGET"]
factorsTest <- testing[,keepFeaturesValidation$factors]
factorsTest <- lapply(factorsTest, as.factor) %>% as.data.frame
factorsTest$SK_ID_CURR <- as.numeric(as.character(factorsTest$SK_ID_CURR))
numericsTest <- testing[,keepFeaturesValidation$numerics]
testing <- left_join(numericsTest, factorsTest, by = "SK_ID_CURR")
rm(factorsTest, numericsTest)

preProcTest1 <- predict(preProc1, newdata = testing[,-1])
preProcTest2 <- predict(preProc2, newdata = preProcTest1)
testingProcessed <- cbind(SK_ID_CURR = testing[,1], preProcTest2)
rm(preProcTest1, preProcTest2)

test.pred.prob <- predict(mdl.gbm7, testingProcessed, type = "prob")
submission <- data.frame("SK_ID_CURR" = as.integer(testing[,1]),
                         "TARGET" = test.pred.prob$No)
write.csv(submission, "submits/mdl.gbm7.8-10-18.csv", 
          row.names = FALSE, quote = FALSE)
sink(file = 'models/varImp.mdl.gbm7.txt')
varImp(mdl.gbm7)
sink()
