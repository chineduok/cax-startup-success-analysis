library(dplyr)
library(tidyverse)
library(broom)

cnt_df <- read.csv('cnt_var_processed.csv')
char_df <- read.csv('char_df_processed.csv')

# t-test for checking difference in mean
t.test(Team.size.all.employees~Dependent.Company.Status, data=cnt_df)

#select variables that have the two samples means almost equal for t-test and variabled seletion
# efficiently selecting relevant variables from t-test results

var_seleection <- cnt_df%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  group_by(var)%>%
  do(tidy(t.test(value~ Dependent.Company.Status, data=.)))

as.data.frame(var_seleection)
var_less_0.05<- var_seleection%>%filter(p.value <= 0.05)%>%select(statistic,p.value)
var_above_0.05<- var_seleection%>%filter(p.value >0.05)

### Select Variables with p-value below 0.05
new_cnt_var<-var_less_0.05$var

cnt_df_new<-cnt_df[,new_cnt_var]


# tabulating data for chi-sq test
tab<- table(char_df$Dependent.Company.Status,char_df$Local.or.global.player)
tab
# chi-sq test
tidy(chisq.test(tab))

# apply simultaneously to select variables

chisq<-as.data.frame(lapply(char_df[,-1], function(x) chisq.test(table(x,char_df$Dependent.Company.Status), 
                                                                 simulate.p.value = TRUE)$p.value))

row.names(chisq)<- "p.values"
chisq_t <-as.data.frame(t(chisq))
str(chisq_t)
pv.less.0.05 <- t(chisq_t%>%filter(p.values <0.05))

char.cols.sel <- colnames(pv.less.0.05)
char.cols.sel

new.char_df <-char_df[,c(char.cols.sel,"Dependent.Company.Status")]

# combine the new continuous variables dataframe with the categorical variables dataframe

final_df <-cbind(cnt_df_new,new.char_df)

colnames(final_df)
write.csv(final_df,"startup_final_data.csv",row.names=F)
