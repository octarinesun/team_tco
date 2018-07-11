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

# Do one-hot encoding for factor variable STATUS
mm <- tbl_df(model.matrix(~ SK_ID_CURR + STATUS-1, data = bureau))
mm$SK_ID_CURR <- as.integer(mm$SK_ID_CURR)
mm <- group_by(mm, SK_ID_CURR) %>%
      summarize_all(sum)

newFeatures <- data.frame(SK_ID_CURR = unique(bureau$SK_ID_CURR))
newFeatures <- left_join(newFeatures, mm) %>% as.data.table()

bureau$SK_ID_BUREAU <- as.factor(bureau$SK_ID_BUREAU)
bureau <- bureau %>% summarize_if(is.numeric, funs(max, min, mean), na.rm=T)
# Rename columns to denote that they came from the "bureau" dataframe (some column names are the same in different files)
colNames <- names(bureau) %>% 
      .[2:length(.)] %>%
      sapply(function(name) paste0("BUREAU_", name))
names(bureau) <- c("SK_ID_CURR", colNames)

# Join the new features to the training set
df <- left_join(df, newFeatures, by = "SK_ID_CURR")

rm(newFeatures, bureau, bureau_balance, mm, colNames)
