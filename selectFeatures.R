## Order of processing:
# Correlation
# Imputation
# NZV
# Center/Scale/PCA

# Function requirements: takes the original dataset and does correlation, imputation, and nzv. Returns a vector of features to keep.

# df <- training
# impute <- "medianImpute"
# corrCutoff <- 0.75

selectFeatures <- function(df, impute = "medianImpute", corrCutoff = 0.75) {
      
      require(caret)
      require(doParallel)
      require(dplyr)

      numerics <- select(df, which(sapply(df, is.numeric)))
      factors <- select(df, SK_ID_CURR, which(sapply(df, is.factor)))
      
      # There are some categorical features that were converted to numeric dummies during the processing stage, or that are binary. Cast them to factors. 
      catFeats <- grep("NAME|PRODUCT|STATUS|CODE_REJECT|CHANNEL_TYPE|FLAG|NFLAG|^FLAG_|^LIVE_|^REG_", 
                       names(numerics))
      
      catFeatDF <- numerics[,catFeats]
      
      # Cast
      catFeatDF <- lapply(catFeatDF, as.factor) %>% as.data.frame
      factors <- as.data.frame(c(catFeatDF, factors))
      numerics <- numerics[,-catFeats]

      ## Change missing NA values to 0
      # for(i in catFeats) {
      #       theCol <- numerics[,i]
      #       theCol[is.na(theCol)] <- 0
      #       numerics[,i] <- theCol
      # }
      # rm(i, theCol)
      
      # Remove correlated features
      correlationMatrix <- cor(numerics[,-c(1:2)], use = "na.or.complete")
      
      # Set NA values to 0
      correlationMatrix[is.na(correlationMatrix)] <- 0
      
      # find attributes that are highly correlated
      highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = corrCutoff, 
                                          names = FALSE)
      
      # Drop highly correlated columns (add 2)
      numerics <- numerics[,-(highlyCorrelated + 2)]
      
      # Imputation (default is medianImpute)
      preProc1 <- preProcess(numerics, 
                                  method = c(impute), 
                                  verbose = TRUE)
      
      numerics <- predict(preProc1, newdata = numerics)
      
      # Remove ZV or NZV features
      cl <- makePSOCKcluster(2)
      registerDoParallel(cl)
      nzvOut <- nearZeroVar(x = numerics[,-c(1:2)], 
                            saveMetrics = TRUE, 
                            names = TRUE, 
                            foreach = TRUE)
      stopCluster(cl)
      
      # nonNZVNumerFeats <- c("SK_ID_CURR", 
      #                       "TARGET", 
      #                       rownames(nzvOut[nzvOut$nzv == FALSE,]))
      
      # Remove zero variance features instead
      nonNZVNumerFeats <- c("SK_ID_CURR", 
                            "TARGET", 
                            rownames(nzvOut[nzvOut$zeroVar == FALSE,]))
      
      numerics <- select(numerics, nonNZVNumerFeats)
      
      # There are two factor columns with lots of NA values: NAME_CONTRACT_STATUS_CC (83%) AND NAME_CONTRACT_STATUS_POS (78%)
      # STATUS"X"BUREAU ALSO HAS A LOT OF NAS (70%)
      # FLAG_MOBIL is mostly 1's
      
      factors <- select(factors, -c(NAME_CONTRACT_STATUS_CC, NAME_CONTRACT_STATUS_POS, STATUS0_BUREAU, STATUS1_BUREAU, STATUS2_BUREAU, STATUS3_BUREAU, STATUS4_BUREAU, STATUS5_BUREAU, STATUSC_BUREAU, STATUSX_BUREAU, FLAG_MOBIL))
      
      # Rejoin data frame
      #df <- left_join(numerics, factors, by = "SK_ID_CURR")
      
      keepFeatures <- list(factors = names(factors), numerics = names(numerics))
      keepFeatures
}