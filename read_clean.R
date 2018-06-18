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
}


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
        ggplot(aes_string(x=thisColNm)) + geom_density(aes_string(color = shQuote(target))) +
        xlim(min(df[,i], na.rm=T),quantile(df[,i],0.99, na.rm=T))
      
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


application_train %>% #mutate(TARGET = as.factor(TARGET)) %>%
ggplot(aes(x=AMT_INCOME_TOTAL, color = TARGET)) + 
  geom_density()+
  xlim(min(application_train$AMT_INCOME_TOTAL, na.rm=T),quantile(application_train$AMT_INCOME_TOTAL, 0.99, na.rm=T))

# load_kagge_data()

head(application_train)
df = application_train

make_df = function(df){
  df %>% head() %>%
    left_join(bureau, by = "SK_ID_CURR") %>% View()
    left_join(bureau_balance, by = "SK_ID_BUREAU") %>%
    left_join(cc_balance, by = "SK_ID_CURR")
    
}
