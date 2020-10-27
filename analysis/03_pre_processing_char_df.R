library(dplyr)
library(tidyverse)

# Separate data frame for numeric variables, use the select_if()
new_startup <-read.csv('na_processed_data.csv')
cnt_df <- select_if(new_startup,is.numeric)


# separate data frame for character variables
cnt_var<-colnames(cnt_df)
var <- colnames(new_startup) %in% cnt_var 
char_df <- new_startup[!var]

# checking distribution of categorical variable for missing values
table(char_df$Local.or.global.player,useNA="always")
table(char_df$Gartner.hype.cycle.stage, useNA = "always")

# convert a variable to uppercase
char_df$Local.or.global.player<-toupper(char_df$Local.or.global.player)

cols <- c(2:3,6:72)
for(i in cols)
{
  char_df[,i]<-toupper(char_df[,i])
}

# trimming white-spaces
char_df$Local.or.global.player<-trimws(char_df$Local.or.global.player)

# converting character variables to factors
char_df$Local.or.global.player<- as.factor(char_df$Local.or.global.player)
char_df$Degree.from.a.Tier.1.or.Tier.2.university.<-as.factor(char_df$Degree.from.a.Tier.1.or.Tier.2.university.)
char_df$Number.of..of.Partners.of.company<-as.factor(char_df$Number.of..of.Partners.of.company)
char_df$Cloud.or.platform.based.serive.product.<-as.factor(char_df$Cloud.or.platform.based.serive.product.)
char_df$Average.size.of.companies.worked.for.in.the.past <- as.factor(char_df$Average.size.of.companies.worked.for.in.the.past)

colnames(char_df)
# using for loop to convert common variable vlaues as factors
yes_no <- c(7,12,14,16:19,21,23,25,29,31:40,45,49:50,60:61,63:70)
for(i in yes_no)
{
  char_df[,i]<-as.factor(char_df[,i])
}
hi_lo <- c(13,43,44,46,52,56:59,62)
for(i in hi_lo)
{
  char_df[,i]<-as.factor(char_df[,i])
}

other_factors <- c(10,11,20,22,24,28,41,42,47)
for(i in other_factors)
{
  char_df[,i]<-as.factor(char_df[,i])
}


# Create additional features like counting number of investors for company
char_df$Investor.count<-length(strsplit(char_df$Investors, "|",fixed=T))
for (i in (1:length(char_df$Investors)))
{
  if(is.na(char_df$Investors[i])==T){
    char_df$Investor.count[i]<- NA}
  else{
    lst<-strsplit(char_df$Investors[i], "|", fixed=T)
    char_df$Investor.count[i]<-length(lst[[1]])
  } }

# Create additional features like counting number of industries a startup belongs to
char_df$Industry.count<-length(strsplit(char_df$Industry.of.company, "|",fixed=T))
for (i in (1:length(char_df$Industry.of.company)))
{
  if(is.na(char_df$Industry.of.company[i])==T){
    char_df$Industry.count[i]<- NA}
  else{
    lst<-strsplit(char_df$Industry.of.company[i], "|", fixed=T)
    char_df$Industry.count[i]<-length(lst[[1]])
  } }

char_df$Investor.count<- as.factor(char_df$Investor.count)
char_df$Dependent.Company.Status <- as.factor(char_df$Dependent.Company.Status)
char_df$Focus.functions.of.company <- as.factor(char_df$Focus.functions.of.company)
char_df$Industry.count <-as.factor(char_df$Industry.count)


# function to calculate mode
Mode <- function(x) {
  u <- unique(na.omit(x))  # took me a while had to include this as I had NAs as highest frequency
  u[which.max(tabulate(match(x, u)))]
}
# Filling missing values with mode
char_df$Local.or.global.player[is.na(char_df$Local.or.global.player)]<-Mode(char_df$Local.or.global.player)
table(char_df$Local.or.global.player)
char_df$Gartner.hype.cycle.stage[is.na(char_df$Gartner.hype.cycle.stage)] <- Mode(char_df$Gartner.hype.cycle.stage)
table(char_df$Gartner.hype.cycle.stage)


#applying mode of each variable to their missing values

for(char in 1:ncol(char_df)) {
  char_df[is.na(char_df[,char]),char] <-Mode(char_df[,char])
}



write.csv(char_df,"char_df_processed.csv",row.names = F)
