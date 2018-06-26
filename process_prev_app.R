library(dplyr)
library(dummies)

## Read data
#application_train <- readRDS("dat/application_train.RDS")
prev_app <- readRDS("dat/prev_app.RDS")

# Check to see which features have potential
# prev_app_targets <- application_train[,1:2]
# prev_app <- inner_join(prev_app_targets, prev_app, by = "SK_ID_CURR")
# auto_explorer(prev_app, target = "TARGET", features_start = 4, file = "fig/prev_app_plots.pdf")
# rm(prev_app_targets)

prev_app <- select(prev_app, -c(AMT_APPLICATION, AMT_DOWN_PAYMENT, AMT_GOODS_PRICE, WEEKDAY_APPR_PROCESS_START, FLAG_LAST_APPL_PER_CONTRACT, NFLAG_LAST_APPL_IN_DAY, RATE_DOWN_PAYMENT, RATE_INTEREST_PRIMARY, RATE_INTEREST_PRIVILEGED, NAME_PAYMENT_TYPE, NAME_TYPE_SUITE, NAME_CLIENT_TYPE, NAME_PORTFOLIO, CNT_PAYMENT, DAYS_FIRST_DRAWING, DAYS_FIRST_DUE, DAYS_LAST_DUE_1ST_VERSION, DAYS_LAST_DUE, DAYS_DECISION, SELLERPLACE_AREA, DAYS_TERMINATION, NFLAG_INSURED_ON_APPROVAL))

# Separate factor and non-factor columns
factorCols <- prev_app[,c(1,2,which(sapply(prev_app, class) == "factor"))]
prev_app <- prev_app[,!(sapply(prev_app, class) == "factor")]

# One-hot encoding; remove spaces from column names
factorCols <- dummy.data.frame(data = factorCols, dummy.classes = "factor", sep = "_")
names(factorCols) <- lapply(names(factorCols), 
                            function(x) gsub(" |/|:|\\(|-|)|\\+", "_", x))

# Collapse for each loan to get the count in each category
factorCols <- factorCols %>%
      group_by(SK_ID_CURR) %>%
      summarize_all(sum)

prev_app_summary <- prev_app %>%
      group_by(SK_ID_CURR) %>%
      summarize(AMT_ANNUITY_PREV_SUM = sum(AMT_ANNUITY),
                AMT_ANNUITY_PREV_MEAN = mean(AMT_ANNUITY),
                AMT_ANNUITY_PREV_MAX = max(AMT_ANNUITY),
                AMT_ANNUITY_PREV_MIN = min(AMT_ANNUITY),
                AMT_CREDIT_PREV_SUM = sum(AMT_CREDIT),
                AMT_CREDIT_PREV_MEAN = mean(AMT_CREDIT),
                AMT_CREDIT_PREV_MAX = max(AMT_CREDIT),
                AMT_CREDIT_PREV_MIN = min(AMT_CREDIT),
                HOUR_APPR_PROCESS_START_PREV_MIN = min(HOUR_APPR_PROCESS_START),
                HOUR_APPR_PROCESS_START_PREV_MAX = max(HOUR_APPR_PROCESS_START))

prev_app_summary <- left_join(prev_app_summary, factorCols, by = "SK_ID_CURR")

# Join to dataset
df <- left_join(df, prev_app_summary, by = "SK_ID_CURR")

rm(prev_app_summary, factorCols, prev_app)

# Looks like the general trend is that the more loans a particular person has, the less likely they are to repay the loan