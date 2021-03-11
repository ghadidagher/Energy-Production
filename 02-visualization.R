library(tidyverse)
library(caret)
library(fpp2)
library(gridExtra)
library(gtable)
library(ggthemes)

#Loading data 
data.list=readRDS("datalist.RDS")
data1=readRDS("data1.RDS")

#Plot 1: Oil production by region
p1=data1%>%select(Entity,Year,Oil)%>%group_by(Entity)%>%na.omit()%>%
  ggplot(aes(x=Year,y=Oil,col=Entity))+geom_line(size=1)+
  ggtitle("Oil Production by Region")+ylab("Oil Production in Mtoe")
p1

#Plot 2: Renewable energy production by region
p2=data1%>%select(Entity,Year,Renewables)%>%group_by(Entity)%>%na.omit()%>%
  ggplot(aes(x=Year,y=Renewables,col=Entity))+geom_line(size=1)+
  ggtitle("Renewable Energy Production by Region")+
  ylab("Renewable Energy Production in Mtoe")+
  scale_x_continuous(limits = c(1960,2018))
p2

#Comparing oil production vs renewable energy for each region
#Plot 3: Africa
p3=data1%>%filter(Entity=="Africa")%>%
  gather(Type, Production,"Oil":"Renewables")%>%
  group_by(Type)%>%
  ggplot(aes(x=Year,y=Production,col=Type))+
  geom_line(size=1)+
  ggtitle("Oil vs Renewable Energy In Africa")+ylab("Production in Mtoe")
p3
#The gather function was used to make the Type a category so we can have a 
#legend in the plot

#Instead of repeating the same thing for all regions, it is better to create 
#a for loop
regions=levels(data1$Entity)
p=rep(NA,length(regions))%>%as.list()
for (i in 1:length(regions)) {
  p[[i]]=data1%>%filter(Entity==regions[i])%>%
    gather(Type, Production,"Oil":"Renewables")%>%
    group_by(Type)%>%
    ggplot(aes(x=Year,y=Production,col=Type))+
    geom_line(size=1)+
    ggtitle(paste("Oil vs Renewable Energy In",regions[i],sep=" "))+
    ylab("Production in Mtoe")
  
  print(p[[i]])
  
}
#the print function is needed inside the for loop

#we see that the renewable energy production is very small thus we will 
#model and forecast oil production only

#finally we save the plots
saveRDS(p, file = "plotlist.RDS")