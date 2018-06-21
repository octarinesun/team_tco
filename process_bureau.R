library(dplyr)

## Read data
application_train <- readRDS("dat/application_train.RDS")
bureau <- readRDS("dat/bureau.RDS")
bureau_balance <- readRDS("dat/bureau_balance.RDS")

### The bureau_balance dataframe has multiple entries for each SK_ID_BUREAU indicating the STATUS of the credit bureau loan during each month prior to the loan application. Larger numbers indicate more recent data (MONTHS_BALANCE). Let's only keep the most recent entry for each SK_ID_BUREAU.

bureau_balance <- group_by(bureau_balance, SK_ID_BUREAU) %>% 
      filter(MONTHS_BALANCE == max(MONTHS_BALANCE))

### Bureau dataframe: drop the columns that are probably not useful
# Get min, max, and mean for each remaining column
bureau <- left_join(bureau, bureau_balance, by = "SK_ID_BUREAU") %>% 
      select(-c(CREDIT_ACTIVE, CREDIT_CURRENCY, DAYS_CREDIT_UPDATE, 
                CREDIT_TYPE)) %>%
      group_by(SK_ID_CURR)

# Do one-hot encoding for factor variables
# Figure out a way to loop through and determine if it's a factor variable, and automatically build the one-hot matrix. For now there's only one factor variable, so this will be okay, but it needs to be automated. (Try dummies package?)
mm <- tbl_df(model.matrix(~ SK_ID_CURR + STATUS-1, data = bureau))
mm$SK_ID_CURR <- as.integer(mm$SK_ID_CURR)
mm <- group_by(mm, SK_ID_CURR) %>%
      summarize_all(sum)

newFeatures <- distinct(bureau, SK_ID_CURR)
newFeatures <- left_join(newFeatures, mm)

for (i in 3:length(bureau)) {
      colName <- names(bureau)[i]
      if(class(as.data.frame(bureau)[,i]) != "factor") {
            colNameMax <- paste(colName, "_MAX", sep="")
            colNameMin <- paste(colName, "_MIN", sep="")
            colNameMean <- paste(colName, "_MEAN", sep="")
            colName <- as.name(colName)
            tempdf <- summarize(bureau, !!colNameMax := max(!!colName),
                                !!colNameMin := min(!!colName),
                                !!colNameMean := mean(!!colName))
            
            newFeatures <- left_join(newFeatures, tempdf)
      }
}

# Join the new features to the training set
application_train <- left_join(application_train, newFeatures, by = "SK_ID_CURR")