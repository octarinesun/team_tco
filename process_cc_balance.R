library(dplyr)

## Read data
cc_balance <- readRDS("dat/cc_balance.RDS")
application_train <- readRDS("dat/application_train.RDS")

get_recent_cc_data = function(df) {
      df <- filter(df, MONTHS_BALANCE == max(MONTHS_BALANCE, 
                                             na.rm = TRUE)) %>%
            select(SK_ID_CURR, SK_ID_PREV, MONTHS_BALANCE, 
                   AMT_BALANCE, AMT_CREDIT_LIMIT_ACTUAL,
                   AMT_DRAWINGS_ATM_CURRENT, AMT_DRAWINGS_CURRENT, 
                   AMT_DRAWINGS_OTHER_CURRENT, AMT_DRAWINGS_POS_CURRENT,
                   AMT_RECEIVABLE_PRINCIPAL, AMT_TOTAL_RECEIVABLE, 
                   CNT_DRAWINGS_ATM_CURRENT, CNT_DRAWINGS_CURRENT, 
                   CNT_DRAWINGS_OTHER_CURRENT, CNT_DRAWINGS_POS_CURRENT, 
                   NAME_CONTRACT_STATUS, SK_DPD, SK_DPD_DEF) %>%
            rename(SK_DPD_CC = SK_DPD, SK_DPD_DEF_CC = SK_DPD_DEF)
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
# AMT_BALANCE: most recent
## debt_time_corr: calculated from MONTHS_BALANCE and AMT_BALANCE (unused)
# AMT_CREDIT_LIMIT_ACTUAL: most recent and max
# AMT_DRAWINGS_ATM_CURRENT: most recent
# AMT_DRAWINGS_CURRENT: most recent
# AMT_DRAWINGS_OTHER_CURRENT: most recent
# AMT_DRAWINGS_POS_CURRENT: most recent
# AMT_INST_MIN_REGULARITY
# AMT_PAYMENT_CURRENT: drop (not as useful as AMT_PAYMENT_TOTAL_CURRENT I think)
# AMT_PAYMENT_TOTAL_CURRENT
### amt_above_min_payment: AMT_PAYMENT_TOTAL_CURRENT - AMT_INST_MIN_REGULARITY
## amt_above_min_payment_mean: mean(amt_above_min_payment)
# AMT_RECEIVABLE_PRINCIPAL: most recent
# AMT_RECIVABLE: drop (not as useful as AMT_TOTAL_RECEIVABLE I think)
# AMT_TOTAL_RECEIVABLE: most recent
### interest: AMT_TOTAL_RECEIVABLE - AMT_RECEIVABLE_PRINCIPAL
## interest_total: sum(interest)
## interest_mean: mean(interest)
# CNT_DRAWINGS_ATM_CURRENT: most recent
# CNT_DRAWINGS_CURRENT: most recent
# CNT_DRAWINGS_OTHER_CURRENT: most recent
# CNT_DRAWINGS_POS_CURRENT: most recent
# CNT_INSTALMENT_MATURE_CUM: drop
# NAME_CONTRACT_STATUS: most recent
# SK_DPD: sum and mean and recent
# SK_DPD_DEF: sum and mean and recent

cc_balance <- select(cc_balance, -c(AMT_RECIVABLE, AMT_PAYMENT_CURRENT)) %>%
      mutate(amt_above_min_payment = 
                   AMT_PAYMENT_TOTAL_CURRENT - AMT_INST_MIN_REGULARITY, 
             interest = AMT_TOTAL_RECEIVABLE - AMT_RECEIVABLE_PRINCIPAL)

cc_balance_recent <- get_recent_cc_data(cc_balance)

cc_balance_summary <- cc_balance %>% 
      group_by(SK_ID_CURR, SK_ID_PREV) %>% 
      #filter(SK_ID_CURR == 126868) %>% # Testing one loan
      summarize(AMT_CREDIT_LIMIT_ACTUAL_MAX = max(AMT_CREDIT_LIMIT_ACTUAL, 
                                                  na.rm = TRUE),
                amt_above_min_payment_mean = mean(amt_above_min_payment, 
                                                  na.rm = TRUE),
                interest_total = sum(interest, na.rm = TRUE),
                interest_mean = mean(interest, na.rm = TRUE), 
                SK_DPD_CC_SUM = sum(SK_DPD, na.rm = TRUE),
                SK_DPD_CC_MEAN = mean(SK_DPD, na.rm = TRUE),
                SK_DPD_DEF_CC_SUM = sum(SK_DPD_DEF, na.rm = TRUE),
                SK_DPD_DEF_CC_MEAN = mean(SK_DPD_DEF, na.rm = TRUE))

# This block takes forever to run...maybe not the best way to do it
# cc_balance_correlations <- cc_balance %>%
#       group_by(SK_ID_CURR, SK_ID_PREV) %>%
#       summarize(debt_time_corr = debt_time_corr(.))

# Join the datasets
cc_balance_summary <- left_join(cc_balance_recent, cc_balance_summary,  
                                by = c("SK_ID_PREV", "SK_ID_CURR"))


# There are still some SK_ID_CURRs that have multiple SK_ID_PREVs. Collapse to 1:1

# Wasn't sure how to make NAME_CONTRACT_STATUS into a single value
cc_mult <- cc_balance_summary %>%
      group_by(SK_ID_CURR) %>%
      filter(n() > 1) %>%
      summarize(AMT_CREDIT_LIMIT_ACTUAL_MAX = sum(AMT_CREDIT_LIMIT_ACTUAL_MAX),
                amt_above_min_payment_mean = mean(amt_above_min_payment_mean),
                interest_total = sum(interest_total),
                interest_mean = mean(interest_mean),
                SK_DPD_CC_SUM = sum(SK_DPD_CC_SUM),
                SK_DPD_CC_MEAN = mean(SK_DPD_CC_MEAN),
                SK_DPD_DEF_CC_SUM = sum(SK_DPD_DEF_CC_SUM),
                SK_DPD_DEF_CC_MEAN = mean(SK_DPD_DEF_CC_MEAN),
                MONTHS_BALANCE = max(MONTHS_BALANCE),
                AMT_BALANCE = sum(AMT_BALANCE),
                AMT_CREDIT_LIMIT_ACTUAL = sum(AMT_CREDIT_LIMIT_ACTUAL),
                AMT_DRAWINGS_ATM_CURRENT = sum(AMT_DRAWINGS_ATM_CURRENT),
                AMT_DRAWINGS_CURRENT = sum(AMT_DRAWINGS_CURRENT),
                AMT_DRAWINGS_OTHER_CURRENT = sum(AMT_DRAWINGS_OTHER_CURRENT),
                AMT_DRAWINGS_POS_CURRENT = sum(AMT_DRAWINGS_POS_CURRENT),
                AMT_RECEIVABLE_PRINCIPAL = sum(AMT_RECEIVABLE_PRINCIPAL),
                AMT_TOTAL_RECEIVABLE = sum(AMT_TOTAL_RECEIVABLE),
                CNT_DRAWINGS_ATM_CURRENT = sum(CNT_DRAWINGS_ATM_CURRENT),
                CNT_DRAWINGS_CURRENT = sum(CNT_DRAWINGS_CURRENT),
                CNT_DRAWINGS_OTHER_CURRENT = sum(CNT_DRAWINGS_OTHER_CURRENT),
                CNT_DRAWINGS_POS_CURRENT = sum(CNT_DRAWINGS_POS_CURRENT),
                SK_DPD = sum(SK_DPD),
                SK_DPD_DEF = sum(SK_DPD_DEF),
                NAME_CONTRACT_STATUS = "Unknown")

# Drop SK_ID_PREV column, then filter out the SK_ID_CURRs with multiple entries;
# Join to consolidated table (cc_mult)
cc_balance_summary <- cc_balance_summary %>% 
      select(-SK_ID_PREV) %>%
      filter(!(SK_ID_CURR %in% cc_mult$SK_ID_CURR)) %>%
      union(cc_mult)

# Join to training set
application_train <- left_join(application_train, cc_balance_summary, by = "SK_ID_CURR")