library(tidyverse)
library(caret)
library(fpp2)

#Importing data
filename="Energy-production-by-source-1900-2014.csv"
fullpath=file.path(getwd(), filename)

data1=read_csv(fullpath)
data1=as.data.frame(data1)

#Inspecting data
class(data1)
str(data1)
head(data1)

#Before proceeding it is better to change the names of the columns
colnames(data1)=c("Entity","Year","Oil","Renewables")

#Also we see that the columns Oil and Renewables are of type character
#So first we should make them numeric
#All missing values will be replaced automatically by NA
data1=data1%>%mutate(Oil=as.numeric(Oil),Renewables=as.numeric(Renewables))


#By looking at the csv file we see that the entity column contain several regions
#So we will convert it to a factor and see how many regions we have

data1=data1%>%mutate(Entity=as.factor(Entity))
str(data1) #we see that we have 8 different regions
levels(data1$Entity)

#Now we will separate each region into a single data frame
#This will help for the time series analysis
regions=levels(data1$Entity)
data.list=rep(NA,length(regions))%>%as.list()
for (i in 1:length(regions)) {
data.list[[i]]=data1%>%filter(Entity==regions[i])  
}
data.list
#Now we have the data in 2 forms: a data frame and a list of 8 data frames 
#We can access each variable as follows:
data.list[[1]]$Renewables

#Saving the dataset and the data list for future use
saveRDS(data.list, file = "datalist.RDS")
saveRDS(data1, file = "data1.RDS")
