library(dplyr)
startup<- read.csv(file="CAX_Startup_Data.csv", header=TRUE,as.is=T)

# replacing 'No Info' and 'blanks' with NA
startup[startup=="No Info"]<- NA
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
new_startup <- new_startup[which(rowMeans(!is.na(new_startup))>.4), ]

# writing new data as csv file
write.csv(new_startup,"na_processed_data.csv",row.names=F)

