library(dplyr)

## Read data
#application_train <- readRDS("dat/application_train.RDS")
pos_cash <- readRDS("dat/pos_cash.RDS")

get_recent_cash_data = function(df) {
      df <- filter(df, MONTHS_BALANCE == max(MONTHS_BALANCE, 
                                             na.rm = TRUE)) %>%
            mutate(cnt_installment_remainder = 
                         CNT_INSTALMENT_FUTURE + MONTHS_BALANCE) %>%
            rename(SK_DPD_POS = SK_DPD, SK_DPD_DEF_POS = SK_DPD_DEF,
                   NAME_CONTRACT_STATUS_POS = NAME_CONTRACT_STATUS)
}

# FEATURE ENGINEERING
# Somehow figure out how well someone is paying off their previous loan
# Key:
# Single Hash: original features
## Double Hash: engineered feature, summary entry
### Triple Hash: engineered feature for each row

# SK_ID_PREV 
# SK_ID_CURR
# MONTHS_BALANCE: most recent
# CNT_INSTALMENT: drop
# CNT_INSTALMENT_FUTURE: drop after mutation
## cnt_installment_remainder = CNT_INSTALMENT_FUTURE + MONTHS_BALANCE
# NAME_CONTRACT_STATUS: recent
# SK_DPD: sum and mean and recent
# SK_DPD_DEF: sum and mean and recent

pos_cash_recent <- get_recent_cash_data(pos_cash) %>%
      select(-c(CNT_INSTALMENT, CNT_INSTALMENT_FUTURE, MONTHS_BALANCE))

pos_cash_summary <- pos_cash %>% 
      group_by(SK_ID_CURR, SK_ID_PREV) %>% 
      #filter(SK_ID_CURR == 126868) %>% # Testing one loan
      summarize(SK_DPD_POS_SUM = sum(SK_DPD, na.rm = TRUE),
                SK_DPD_POS_MEAN = mean(SK_DPD, na.rm = TRUE),
                SK_DPD_DEF_POS_SUM = sum(SK_DPD_DEF, na.rm = TRUE),
                SK_DPD_DEF_POS_MEAN = mean(SK_DPD_DEF, na.rm = TRUE))

# Join the datasets
pos_cash_summary <- left_join(pos_cash_recent, pos_cash_summary, 
                                by = c("SK_ID_PREV", "SK_ID_CURR"))

# There are still some SK_ID_CURRs that have multiple SK_ID_PREVs. Collapse to 1:1
# Wasn't sure how to make NAME_CONTRACT_STATUS into a single value
pos_mult <- pos_cash_summary %>%
      group_by(SK_ID_CURR) %>%
      filter(n() > 1) %>%
      summarize(SK_DPD_POS_SUM = sum(SK_DPD_POS_SUM),
                SK_DPD_POS_MEAN = mean(SK_DPD_POS_MEAN),
                SK_DPD_DEF_POS_SUM = sum(SK_DPD_DEF_POS_SUM),
                SK_DPD_DEF_POS_MEAN = mean(SK_DPD_DEF_POS_MEAN),
                SK_DPD_POS = sum(SK_DPD_POS),
                SK_DPD_DEF_POS = sum(SK_DPD_DEF_POS),
                cnt_installment_remainder = sum(cnt_installment_remainder),
                NAME_CONTRACT_STATUS_POS = "Unknown")

# Drop SK_ID_PREV column, then filter out the SK_ID_CURRs with multiple entries;
# Join to consolidated table (pos_mult)
pos_cash_summary <- pos_cash_summary %>% 
      select(-SK_ID_PREV) %>%
      filter(!(SK_ID_CURR %in% pos_mult$SK_ID_CURR)) %>%
      union(pos_mult)

# Join to training set
df <- left_join(df, pos_cash_summary, by = "SK_ID_CURR")
df$NAME_CONTRACT_STATUS_POS <- as.factor(df$NAME_CONTRACT_STATUS_POS)

rm(pos_cash, pos_cash_recent, pos_cash_summary, pos_mult)
