library(dplyr)

startup<- read.csv("data/CAX_Startup_Data.csv", header=TRUE,as.is=T)

# replacing 'No Info' and 'blanks' with NA

unique(startup$Survival.through.recession..based.on.existence.of.the.company.through.recession.times)
startup[startup=="No Info"]<- NA
startup[startup=="Not Applicable"]<-NA
startup[startup==""]<- NA
str(startup)

# checking for number of rows with missing values
nrow(startup[!complete.cases(startup),])

# converting column as date.....
startup$Est..Founding.Date <- as.Date(startup$Est..Founding.Date, "%m/%d/%Y")
startup$Last.Funding.Date <- as.Date(startup$Last.Funding.Date, "%m/%d/%Y")

# display column header of data
colnames(startup) 

# noting columns that needs to be converted to numeric
col<- c(3:5,10,11,18:23,25,61,66,72,74,88,92,94:96,98,99,102:116)

# using for loop to convert column as numeric
for(i in col)
{
  startup[,i]<-as.numeric(startup[,i])
}

# Percent missing value for each variable
mis_val<-sapply(startup, function(x) sum(is.na(x)))
percent_mis<-as.data.frame(round((mis_val/nrow(startup))*100,1))

name<-row.names(percent_mis)
pcnt_mis_var<-cbind(name,percent_mis)
row.names(pcnt_mis_var)<-NULL
colnames(pcnt_mis_var)<-c("variable","Percent.Missing")


# keeping only variables with less than 40% missing
removed_variable <- pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing<=40)]
new_var<-as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing<=40)])
new_startup<-startup[new_var]

ncol(startup) # 116
ncol(new_startup)#113


# separate data frame for more than 40% missing
other_var<-as.character(pcnt_mis_var$variable[which(pcnt_mis_var$Percent.Missing>40)])
other_data<-startup[other_var]

# Percent missing value for observations
#new_startup <- new_startup[which(rowMeans(!is.na(new_startup))>.4), ]
new_startup$survived.recessions <- other_data$Survival.through.recession..based.on.existence.of.the.company.through.recession.times

#Impute missing values for Survived.recessions based on year founding, and Dependent.Company.Status
new_startup%>%select(Dependent.Company.Status,survived.recessions,Age.of.company.in.years,year.of.founding,Gartner.hype.cycle.stage)%>%
  filter(Dependent.Company.Status=='Failed'& year.of.founding<2008)

for (i in 1:length(new_startup$survived.recessions))
{
  if(is.na(new_startup$survived.recessions[i]) & is.na(new_startup$year.of.founding[i])){
    is.na(new_startup$survived.recessions[i])}
  else if(is.na(new_startup$survived.recessions[i]) & new_startup$year.of.founding[i] <= 2008 &
             new_startup$Dependent.Company.Status[i]=='Failed') {
    new_startup$survived.recessions[i]<- 'No'}
  else if(is.na(new_startup$survived.recessions[i]) & new_startup$year.of.founding[i] >2008){
    new_startup$survived.recessions[i]<- 'Post-Recession'}
  else {new_startup$survived.recessions[i]}
  } 


#Recalculating age of company

for (i in 1:length(new_startup$Age.of.company.in.years))
{
  if(is.na(new_startup$Age.of.company.in.years[i]) & is.na(new_startup$year.of.founding[i])){
    new_startup$Age.of.company.in.years[i]}
  else if(new_startup$year.of.founding[i] < 2001 & new_startup$survived.recessions[i]== 'No' &
          !is.na(new_startup$Time.to.1st.investment..in.months.[i])) {
    new_startup$Age.of.company.in.years[i]<- ceiling((new_startup$Time.to.1st.investment..in.months.[i])/12)}
  else if(new_startup$year.of.founding[i] >= 2001 & new_startup$survived.recessions[i]== 'No' &
          !is.na(new_startup$Time.to.1st.investment..in.months.[i])) {
    new_startup$Age.of.company.in.years[i]<- ceiling((new_startup$Time.to.1st.investment..in.months.[i])/12)}
  else {new_startup$Age.of.company.in.years[i]}
} 
# writing new data as csv file

write.csv(new_startup,"na_processed_data.csv",row.names=F)

