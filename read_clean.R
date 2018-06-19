setwd("~/R Projects/kaggle-competitions/home-credit-default-risk/team_tco")
# setwd("~/kaggle_home_default/team_tco/")

library(dplyr)
library(lazyeval)
library(ggplot2)
library(magrittr)
library(gridExtra)


### Read Data
load_kaggle_data = function(){
application_test <-  read.table(file="dat/application_test.csv", sep = ",", header=T)
saveRDS(application_test, file = "dat/application_test.RDS")
application_train <- read.table(file="dat/application_train.csv", sep = ",", header=T)
saveRDS(application_train, file = "dat/application_train.RDS")
bureau <- read.table(file="dat/bureau.csv", sep = ",", header=T)
saveRDS(bureau, file = "dat/bureau.RDS")
bureau_balance <- read.table(file="dat/bureau_balance.csv", sep=",", header=T)
saveRDS(bureau_balance, file = "dat/bureau_balance.RDS")
cc_balance <- read.table(file="dat/credit_card_balance.csv", sep=",", header=T)
saveRDS(cc_balance, file = "dat/cc_balance.RDS")
prev_app <- read.table(file="dat/previous_application.csv", sep=",", header=T)
saveRDS(prev_app, file = "dat/prev_app.RDS")
inst_pay <- read.table(file="dat/installments_payments.csv", sep=",", header=T)
saveRDS(inst_pay, file = "dat/inst_pay.RDS")
pos_cash <- read.table(file="dat/POS_CASH_balance.csv", sep=",", header=T)
saveRDS(pos_cash, file = "dat/pos_cash.RDS")
}

# Read from RDS files
application_test <- readRDS("dat/application_test.RDS")
application_train <- readRDS("dat/application_train.RDS")
bureau <- readRDS("dat/bureau.RDS")
bureau_balance <- readRDS("dat/bureau_balance.RDS")
cc_balance <- readRDS("dat/cc_balance.RDS")
prev_app <- readRDS("dat/prev_app.RDS")
inst_pay <- readRDS("dat/inst_pay.RDS")
pos_cash <- readRDS("dat/pos_cash.RDS")


# Functions
target = "TARGET"
df = application_train
auto_explorer = function(df,target){
  explore_df <-  df
  targetCol <-  which(names(df) == target)
  plot_df = NULL
  for(i in c(1:length(explore_df))){
    thisColNm = names(explore_df)[i]
    if(class(df[,i]) == "factor" | n_distinct(explore_df[,i])<10){
      p <- explore_df %>% group_by_(thisColNm) %>%
        summarise_(sum = interp(~ sum(x), x = as.name(target)),
                   n_entries =~n()) %>%
        mutate(frac = sum/n_entries) %>%
        ggplot(aes_string(x=thisColNm,y="frac")) + geom_bar(stat = "identity")
      
      plot_df[[i]] <- p
    } else{
      p <- explore_df %>% 
        ggplot(aes_string(x=thisColNm)) + 
            geom_density(aes_string(color = shQuote(target))) +
            xlim(min(df[,i], na.rm=T),quantile(df[,i],0.99, na.rm=T)) +
            theme(legend.position = "none")
      
      plot_df[[i]] <- p
      }
  }
  return(plot_df)
}

plots <- auto_explorer(application_train, target = "TARGET")

pdf("fig/plots.pdf")
for(p in plots){
  print(p)
}
dev.off()


# application_train %>% #mutate(TARGET = as.factor(TARGET)) %>%
# ggplot(aes(x=AMT_INCOME_TOTAL, color = TARGET)) + 
#   geom_density()+
#   xlim(min(application_train$AMT_INCOME_TOTAL, na.rm=T),quantile(application_train$AMT_INCOME_TOTAL, 0.99, na.rm=T))

# load_kagge_data()

#head(application_train)
#df = application_train

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
# application_train <- left_join(application_train, newFeatures, by = "SK_ID_CURR")

make_df = function(df){
  df %>% head() %>%
            left_join(newFeatures, by = "SK_ID_CURR")
    left_join(bureau, by = "SK_ID_CURR") %>% View()
    left_join(bureau_balance, by = "SK_ID_BUREAU") %>%
    left_join(cc_balance, by = "SK_ID_CURR")
    
}
