library(dplyr)
library(data.table)

## Read data
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

newFeatures <- bureau %>% 
  distinct(SK_ID_CURR) %>%
  left_join(mm, by="SK_ID_CURR")

valid_col = function(x, y){
  return(!is.factor(x) & names(x[,1]) != y)
}

for (i in 3:length(bureau)) {
      colName <- names(bureau)[i]
      if(class(as.data.frame(bureau)[,i]) != "factor") {
            colNameMax <- paste(colName, "_MAX", sep="")
            colNameMin <- paste(colName, "_MIN", sep="")
            colNameMean <- paste(colName, "_MEAN", sep="")
            colName <- as.name(colName)
            tempdf <- summarize(as.data.table(bureau), !!colNameMax := max(!!colName),
                                !!colNameMin := min(!!colName),
                                !!colNameMean := mean(!!colName))
            
            newFeatures <- left_join(newFeatures, tempdf)
      }
}

# Is this what we're looking for? summary?
bureau %>% ungroup() %>% summarise_if(is.numeric, funs(max, min, mean), na.rm=T)

# Join the new features to the training set
df <- left_join(df, newFeatures, by = "SK_ID_CURR")

rm(newFeatures, bureau, bureau_balance, mm, tempdf, colName, colNameMax, colNameMean, colNameMin, i)