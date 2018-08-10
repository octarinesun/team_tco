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
      select(c(SK_ID_CURR, AMT_ANNUITY, AMT_CREDIT, HOUR_APPR_PROCESS_START)) %>%
      rename(AMT_ANNUITY_PREV = AMT_ANNUITY, 
             AMT_CREDIT_PREV = AMT_CREDIT, 
             HOUR_APPR_PROCESS_START_PREV = HOUR_APPR_PROCESS_START) %>%
      group_by(SK_ID_CURR) %>%
      summarize_if(is.numeric, funs(min, max, sum, mean))

# The summarize_if function will cause -Inf and Inf values when a particular SK_ID_CURR has only NA values for a feature. So, find all of those and set them equal to NA.
for (i in 1:length(prev_app_summary)) {
      theCol <- prev_app_summary[i]
      infs <- which(theCol == -Inf | theCol == Inf)
      prev_app_summary[infs, i] <- NA
}
rm(theCol, infs)

factorCols <- select(factorCols, -SK_ID_PREV)
prev_app_summary <- left_join(prev_app_summary, factorCols, by = "SK_ID_CURR")

# Join to dataset
df <- left_join(df, prev_app_summary, by = "SK_ID_CURR")

rm(prev_app_summary, factorCols, prev_app)

# Looks like the general trend is that the more loans a particular person has, the less likely they are to repay the loan