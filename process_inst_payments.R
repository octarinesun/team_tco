library(dplyr)

## Read data
#application_train <- readRDS("dat/application_train.RDS")
inst_pay <- readRDS("dat/inst_pay.RDS")

# FEATURE ENGINEERING
# Somehow figure out how well someone is paying off their previous loan
# Key:
# Single Hash: original features
## Double Hash: engineered feature, summary entry
### Triple Hash: engineered feature for each row
# SK_ID_PREV 
# SK_ID_CURR
# NUM_INSTALMENT_VERSION
# NUM_INSTALMENT_NUMBER
# DAYS_INSTALMENT
# DAYS_ENTRY_PAYMENT
# days_behind_schedule = DAYS_ENTRY_PAYMENT - DAYS_INSTALMENT
# AMT_INSTALMENT
# AMT_PAYMENT
# amt_overpaid = AMT_PAYMENT - AMT_INSTALMENT
# frac_pay_avg = AMT_PAYMENT / AMT_INSTALMENT
# frac_overpaid = amt_overpaid / AMT_INSTALMENT

# There are some entries where people have paid multiple times on a particular installment; it should be represented by a single payment for our purposes

inst_pay <- inst_pay %>%
      group_by(SK_ID_PREV, SK_ID_CURR, NUM_INSTALMENT_NUMBER,
               NUM_INSTALMENT_VERSION, DAYS_INSTALMENT) %>%
      summarize(DAYS_ENTRY_PAYMENT = mean(DAYS_ENTRY_PAYMENT),
                AMT_INSTALMENT = max(AMT_INSTALMENT),
                AMT_PAYMENT = sum(AMT_PAYMENT))

dbs <- inst_pay$DAYS_ENTRY_PAYMENT - inst_pay$DAYS_INSTALMENT
ao <- round(inst_pay$AMT_PAYMENT - inst_pay$AMT_INSTALMENT, digits = 3)

# Some AMT_INSTALMENT entries are 0, which will present problems when computing fractions; change 0's to NA's to avoid this.

AMT_INSTALMENT_NA <- sapply(inst_pay$AMT_INSTALMENT, function(x) { 
      if (x == 0) { 
            x <- NA 
      } 
      x 
})

fo <- ao/AMT_INSTALMENT_NA
fpa <- inst_pay$AMT_PAYMENT/AMT_INSTALMENT_NA
inst_pay <- ungroup(inst_pay) %>%
      mutate(days_behind_schedule = dbs, amt_overpaid = ao, frac_pay_avg = fpa,
             frac_overpaid = fo)

rm(ao, dbs, fpa, fo, AMT_INSTALMENT_NA)

# Now collapse to 1 entry per SK_ID_CURR

inst_pay_summary <- inst_pay %>% 
      select(c(SK_ID_CURR, days_behind_schedule, amt_overpaid, frac_pay_avg, 
               frac_overpaid)) %>%
      group_by(SK_ID_CURR) %>% 
      summarize_if(is.numeric, funs(mean, max, min), na.rm = T)

# Join to training set
df <- left_join(df, inst_pay_summary, by = "SK_ID_CURR")
rm(inst_pay, inst_pay_summary)
