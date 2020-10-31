library(dplyr)
library(tidyverse)

# Separate data frame for numeric variables, use the select_if()
new_startup <-read.csv('na_processed_data.csv')
cnt_df <- select_if(new_startup,is.numeric)
str(cnt_df)

# # separate data frame for character variables
# cnt_var<-colnames(cnt_df)
# var <- colnames(new_startup) %in% cnt_var 
# char_df <- new_startup[!var]
# 
# str(char_df)

# checking distribution of continuous variable for outlier detection and missing values
summary(cnt_df)
summary(cnt_df$Team.size.all.employees)
quantile(cnt_df$Team.size.all.employees, probs = seq(0, 1, by= 0.05),na.rm=T)

# further exploration to determine cutoff for capping
quantile(cnt_df$Team.size.all.employees, probs = seq(.9, 1, by= 0.01),na.rm=T)

quants <- seq(0,1,.1)

#ussing apply function to get the quantiles of the variables in a dataframe
qunt_df <- t(apply( cnt_df[2:36] , 2 , quantile , probs = quants , na.rm = TRUE ))
qunt_df <- as.data.frame(qunt_df[c(3:4,11,15,16,20,22:24),])

quantile(cnt_df$Percent_skill_Engineering, probs = seq(0.9, 1, by= 0.01),na.rm=T)


# capping some outlier values  
cnt_df$Team.size.all.employees[cnt_df$Team.size.all.employees>106.4]<-106
cnt_df$Internet.Activity.Score[cnt_df$Internet.Activity.Score<0]<-0
cnt_df$Internet.Activity.Score[cnt_df$Internet.Activity.Score>100]<-100
cnt_df$Employee.Count[cnt_df$Employee.Count>211.8] <-211
cnt_df$Skills.score[cnt_df$Skills.score>53.45]<-53
cnt_df$Number.of.Recognitions.for.Founders.and.Co.founders[cnt_df$Number.of.Recognitions.for.Founders.and.Co.founders>
                                                             107.5]<-107
cnt_df$Employees.per.year.of.company.existence[
  cnt_df$Employees.per.year.of.company.existence>76.14
]<- 76
cnt_df$Time.to.1st.investment..in.months.[cnt_df$Time.to.1st.investment..in.months.>48]<-48
cnt_df$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment[ cnt_df$Avg.time.to.investment...average.across.all.rounds..measured.from.previous.investment>
                                                                                                48] <- 48

cnt_df$Percent_skill_Entrepreneurship[cnt_df$Percent_skill_Entrepreneurship>30.4]<-30
cnt_df$Percent_skill_Engineering[cnt_df$Percent_skill_Engineering>76]<-75

# add back target variable to continuous data 
cnt_df$Dependent.Company.Status <- new_startup$Dependent.Company.Status

# Missing value treatemnt using median
cnt_df$Team.size.all.employees[is.na(cnt_df$Team.size.all.employees)]<-median(cnt_df$Team.size.all.employees,na.rm=T)
any(is.na(cnt_df$Team.size.all.employees))

# Using median for NA values
impute.med <- function(x) {
  z <- median(x, na.rm = TRUE)
  x[is.na(x)] <- z
  return(x)
}

# Applying median value to missing values in dataframe
nums <-c(1:36)
cnt_df[nums] <- sapply(cnt_df[nums], function(x){
  if(any(is.na(x))){
    impute.med(x)
  } else {
    x
  }
}
)

head(cnt_df)
glimpse(cnt_df)


save(cnt_df,file="rda/cnt_var_processed.rda")
