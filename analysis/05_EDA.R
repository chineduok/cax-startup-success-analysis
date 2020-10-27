library(dplyr)
library(tidyverse)
library(ggplot2)
library(broom)

char_df <-read.csv('char_df_processed.csv')
cnt_df <- read.csv('cnt_var_processed.csv')

# drop some variables that will not be used
char_df <- char_df%>%select(-Company_Name,-Short.Description.of.company.profile,-Est..Founding.Date,-Last.Funding.Date,
                            -Industry.of.company,-Investors,-Specialization.of.highest.education,
                            -Time.to.maturity.of.technology..in.years.)
# boxplot of employee count
boxplot(cnt_df$Employee.Count, main="box plot of employee count", 
        ylab="Employee count")

# histogram with black outline, white fill and median line

ggplot(cnt_df, aes(x=Employee.Count))+
  geom_histogram(binwidth=5, colour="black", fill="white")+
  geom_vline(aes(xintercept=median(Employee.Count, na.rm=T)),   
             color="red", linetype="dashed", size=1)+
  ggtitle("Histogram of Employee count")+
  xlab("Employee Count") + 
  ylab("Frequency")+
  theme_light()
colnames(cnt_df)

# Plot univariate distributions of independent variables using facet wrap
names(cnt_df)
cnt_df%>%
  select(c(1:6))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")

cnt_df%>%
  select(c(7:12))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")
cnt_df%>%
  select(c(13:18))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")
  
cnt_df%>%
  select(c(19:24))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")

cnt_df%>%
  select(c(25:30))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")

cnt_df%>%
  select(c(31:36))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")
cnt_df%>%
  select(c(37:41))%>%
  gather()%>%
  ggplot(aes(value,fill=value))+
  facet_wrap(~key,scales = 'free') +
  geom_histogram()+
  geom_vline(aes(xintercept=median(value, na.rm=T)),   
             color="red", linetype="dashed")
#colnames(cnt_df[c(37:41)])
# box plot to see differnce in mean of team size w.r.t two categories of dependent
ggplot(cnt_df, aes(x=Dependent.Company.Status,y=Team.size.all.employees, 
                   fill=Dependent.Company.Status)) + 
  geom_boxplot()

# Creating boxplots of continuous variables using facet wrap
cnt_df%>%
  select(c(1:6,42))%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  ggplot(aes(x = value,y = Dependent.Company.Status, 
             fill=Dependent.Company.Status))+
  geom_boxplot()+
  facet_wrap(~var,scales = 'free') +
  theme_bw()

cnt_df%>%
  select(c(7:12,42))%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  ggplot(aes(x = value,y = Dependent.Company.Status, 
             fill=Dependent.Company.Status))+
  geom_boxplot()+
  facet_wrap(~var,scales = 'free') +
  theme_bw()

cnt_df%>%
  select(c(13:18,42))%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  ggplot(aes(x = value,y = Dependent.Company.Status, 
             fill=Dependent.Company.Status))+
  geom_boxplot()+
  facet_wrap(~var,scales = 'free') +
  theme_bw()

cnt_df%>%
  select(c(19:24,42))%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  ggplot(aes(x = value,y = Dependent.Company.Status, 
             fill=Dependent.Company.Status))+
  geom_boxplot()+
  facet_wrap(~var,scales = 'free') +
  theme_bw()

cnt_df%>%
  select(c(25:30,42))%>%
  gather(-Dependent.Company.Status, key = "var",value = "value")%>%
  ggplot(aes(x = value,y = Dependent.Company.Status, 
             fill=Dependent.Company.Status))+
  geom_boxplot()+
  facet_wrap(~var,scales = 'free') +
  theme_bw()

# data preparation for bar chart
avg_emp<-aggregate(as.numeric(cnt_df$Team.size.all.employees), 
                   by=list(as.factor(cnt_df$Dependent.Company.Status)), 
                   FUN=mean, na.rm=TRUE)
colnames(avg_emp)<-c("company.status","Avg.Employee.size")

# bar chart to check for difference in mean
ggplot(avg_emp, aes(x = company.status, y = Avg.Employee.size, fill=company.status)) +
  geom_bar(stat = "identity")


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

write.csv(final_df,"startup_final_data.csv",row.names=F)
