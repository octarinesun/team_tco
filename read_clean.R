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
auto_explorer = function(df,target, features_start = 3, file = "fig/plots.pdf"){
  try(dev.off())
  explore_df <-  df
  targetCol <-  which(names(df) == target)
  explore_df[,targetCol] %<>% as.factor()
  event_rate = sum(explore_df[,targetCol] == 1, na.rm=T)/nrow(explore_df)
  plot_df = NULL
  for(i in c(features_start:length(explore_df))){
    thisColNm = names(explore_df)[i]
    # check if factor or integer
    if(class(explore_df[,i]) == "factor" | (is.numeric(explore_df[,i]) & all(explore_df[,i]%%1==0, na.rm=T) & n_distinct(explore_df[,i])<30)){ 
      p <- explore_df %>% group_by_(thisColNm) %>%
        summarise_(sum = interp(~ sum(x==1), x = as.name(target)),
                   n_entries =~n()) %>%
        mutate(frac = sum/n_entries) %>%
        ggplot(aes_string(x=thisColNm,y="frac")) + geom_bar(stat = "identity")+
        geom_hline(aes(yintercept=event_rate))+
        theme_bw()
      
      plot_df[[i]] <- p
    } else{
      p <- explore_df %>% 
        ggplot(aes_string(x=thisColNm)) + geom_density(aes_string(color = target)) +
        xlim(quantile(explore_df[,i],0.01, na.rm=T),quantile(explore_df[,i],0.99, na.rm=T)) +
        theme(legend.position="none") +
        theme_bw()
      
      plot_df[[i]] <- p
      }
  }
  pdf("fig/plots.pdf")
  for(p in plot_df){
    print(p)
  }
  dev.off()
}

auto_explorer(application_train, target = "TARGET")


# application_train %>% #mutate(TARGET = as.factor(TARGET)) %>%
# ggplot(aes(x=AMT_INCOME_TOTAL, color = TARGET)) + 
#   geom_density()+
#   xlim(min(application_train$AMT_INCOME_TOTAL, na.rm=T),quantile(application_train$AMT_INCOME_TOTAL, 0.99, na.rm=T))

# load_kagge_data()

#head(application_train)
#df = application_train

make_df = function(df){
  df %>% head() %>%
            left_join(newFeatures, by = "SK_ID_CURR")
    left_join(bureau, by = "SK_ID_CURR") %>% View()
    left_join(bureau_balance, by = "SK_ID_BUREAU") %>%
    left_join(cc_balance, by = "SK_ID_CURR")
    
}

# Load/clean installments

system.time({
installment_df <- inst_pay %>% 
  # Takes a fraction of the installments just to test aggregates
  head(nrow(inst_pay)/20) %>%
  group_by(SK_ID_CURR, SK_ID_PREV, NUM_INSTALMENT_VERSION, NUM_INSTALMENT_NUMBER) %>%
  summarise(n_payments = n(),
            frac_pay_avg = mean(AMT_PAYMENT/AMT_INSTALMENT))
})
