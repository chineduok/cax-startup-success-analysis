
library(tidyverse)
library(ggplot2)
library(broom)


# drop some variables that will not be used
cnt_df$Dependent.Company.Status <- new_startup$Dependent.Company.Status
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



