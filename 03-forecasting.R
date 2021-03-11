library(tidyverse)
library(caret)
library(fpp2)

#Loading data 
data.list=readRDS("datalist.RDS")
data1=readRDS("data1.RDS")
plots=readRDS("plotlist.RDS")

#The first thing we need to do is to transform all oil data (Oil column)
#to an object of class ts
#Then we separate each ts object into a train data and a test data
#Then test data will contain the last 20 years

firstyear=data1$Year[1]
lastyear=data1$Year[length(data1$Year)/8]

oil.ts=rep(NA,length(data.list))%>%as.list()
train.ts.1=rep(NA,length(data.list))%>%as.list()
test.ts.1=rep(NA,length(data.list))%>%as.list()
train.ts.2=rep(NA,length(data.list))%>%as.list()
test.ts.2=rep(NA,length(data.list))%>%as.list()

for (i in 1:length(data.list)) {
  oil.ts[[i]]=ts(data.list[[i]]$Oil, start=firstyear, end=lastyear, frequency=1)
  train.ts.1[[i]]=window(oil.ts[[i]],end=2004)
  test.ts.1[[i]]=window(oil.ts[[i]],start=2005)
  train.ts.2[[i]]=window(oil.ts[[i]],end=1994)
  test.ts.2[[i]]=window(oil.ts[[i]],start=1995)
  
}

class(oil.ts[[1]]) #this proves that we succeeded in our task
test.ts.1[[1]]

#saving the ts objects 
saveRDS(oil.ts, file = "oil-ts-data.RDS")
saveRDS(train.ts.1, file = "train-ts-data-1.RDS")
saveRDS(test.ts.1, file = "test-ts-data-1.RDS")
saveRDS(train.ts.2, file = "train-ts-data-2.RDS")
saveRDS(test.ts.2, file = "test-ts-data-2.RDS")

#Now for each train data we will forecast 3 benchmarks methods and evaluate 
#their test error 
#If our later models don't beat these simple forecasts then the complex methods
#are useless here
#The 3 benchmarks are the naive, the average and the drift methods
#also the automatic ets and arima are added
#further possible optimization of ets and arima will be done later only on world data

#Each element of the lists will contain the forecast object of the corresponding
#ts data
h1=10

mean.forecast.1=rep(NA,length(data.list))%>%as.list()
naive.forecast.1=rep(NA,length(data.list))%>%as.list()
drift.forecast.1=rep(NA,length(data.list))%>%as.list()
arima.forecast.1=rep(NA,length(data.list))%>%as.list()
ets.forecast.1=rep(NA,length(data.list))%>%as.list()


for (i in 1:length(data.list)) {
  mean.forecast.1[[i]]=meanf(train.ts.1[[i]], h=h1)
  naive.forecast.1[[i]]=rwf(train.ts.1[[i]], h=h1)
  drift.forecast.1[[i]]=rwf(train.ts.1[[i]], h=h1,drift = TRUE)
  ets.forecast.1[[i]]=ets(train.ts.1[[i]],restrict = FALSE)
  arima.forecast.1[[i]]=auto.arima(train.ts.1[[i]],seasonal = FALSE,stepwise = FALSE,
                                 approximation = FALSE)
}

#saving these forecasts for later use
saveRDS(mean.forecast.1, file = "mean-forecast-1.RDS")
saveRDS(naive.forecast.1, file = "naive-forecast-1.RDS")
saveRDS(drift.forecast.1, file = "drift-forecast-1.RDS")
saveRDS(ets.forecast.1, file = "ets-forecast-1.RDS")
saveRDS(arima.forecast.1, file = "arima-forecast-1.RDS")

#Plotting all models for all data
regions=levels(data1$Entity)
plots.forecast.1=rep(NA,length(data.list))%>%as.list()

for (i in 1:length(data.list)) {
  
   plots.forecast.1[[i]]=oil.ts[[i]]%>%autoplot(size=1)+
      autolayer(mean.forecast.1[[i]],series="Mean", PI=FALSE,size=1)+
      autolayer(naive.forecast.1[[i]],series="Naive", PI=FALSE,size=1)+
      autolayer(drift.forecast.1[[i]],series="Drift", PI=FALSE,size=1)+
      autolayer(ets.forecast.1[[i]]%>%forecast(h=h1,PI=FALSE),series="ETS",size=1)+
      autolayer(arima.forecast.1[[i]]%>%forecast(h=h1),series="Arima",PI=FALSE,size=1)+
      ggtitle(paste("Oil Production and Forecasts in",regions[i],sep=" "))+
      ylab("Production")
   print(plots.forecast.1[[i]])
  
}
saveRDS(plots.forecast.1, file = "plot-forecast-1.RDS")
#Note 1: prediction intervals (PI) are removed to make the graph clearer
#Data varies a lot that's why models perform differently when h=10 vs h=20
#Comparing the test RMSE of all models to see which one performs better
model.name=c("mean","naive","drift","ets","arima")
acc=rep(NA,5)

for (i in 1:length(data.list)) {
  acc1=mean.forecast.1[[i]]%>%accuracy(test.ts.1[[i]])
  acc2=naive.forecast.1[[i]]%>%accuracy(test.ts.1[[i]])
  acc3=drift.forecast.1[[i]]%>%accuracy(test.ts.1[[i]])
  acc4=ets.forecast.1[[i]]%>%forecast(h=h1,PI=FALSE)%>%accuracy(test.ts.1[[i]])
  acc5=arima.forecast.1[[i]]%>%forecast(h=h1)%>%accuracy(test.ts.1[[i]])
  index=c(acc1[4],acc2[4],acc3[4],acc4[4],acc5[4])%>%as.numeric()%>%which.min()
  print(paste(paste("Best model for",regions[i],sep = " "),model.name[index],sep = " is "))
}


#We see that the best model changes for each data
#The drift method was best 4 times, arima 3 times and ets one time
#Now repeating the same thing for h=20

h2=20

mean.forecast.2=rep(NA,length(data.list))%>%as.list()
naive.forecast.2=rep(NA,length(data.list))%>%as.list()
drift.forecast.2=rep(NA,length(data.list))%>%as.list()
arima.forecast.2=rep(NA,length(data.list))%>%as.list()
ets.forecast.2=rep(NA,length(data.list))%>%as.list()


for (i in 1:length(data.list)) {
  mean.forecast.2[[i]]=meanf(train.ts.2[[i]], h=h2)
  naive.forecast.2[[i]]=rwf(train.ts.2[[i]], h=h2)
  drift.forecast.2[[i]]=rwf(train.ts.2[[i]], h=h2,drift = TRUE)
  ets.forecast.2[[i]]=ets(train.ts.2[[i]],restrict = FALSE)
  arima.forecast.2[[i]]=auto.arima(train.ts.2[[i]],seasonal = FALSE,stepwise = FALSE,
                                   approximation = FALSE)
}

#saving these forecasts for later use
saveRDS(mean.forecast.2, file = "mean-forecast-2.RDS")
saveRDS(naive.forecast.2, file = "naive-forecast-2.RDS")
saveRDS(drift.forecast.2, file = "drift-forecast-2.RDS")
saveRDS(ets.forecast.2, file = "ets-forecast-2.RDS")
saveRDS(arima.forecast.2, file = "arima-forecast-2.RDS")

#Plotting all models for all data
regions=levels(data1$Entity)
plots.forecast.2=rep(NA,length(data.list))%>%as.list()

for (i in 1:length(data.list)) {
  
  plots.forecast.2[[i]]=oil.ts[[i]]%>%autoplot(size=1)+
    autolayer(mean.forecast.2[[i]],series="Mean", PI=FALSE,size=1)+
    autolayer(naive.forecast.2[[i]],series="Naive", PI=FALSE,size=1)+
    autolayer(drift.forecast.2[[i]],series="Drift", PI=FALSE,size=1)+
    autolayer(ets.forecast.2[[i]]%>%forecast(h=h2,PI=FALSE),series="ETS",size=1)+
    autolayer(arima.forecast.2[[i]]%>%forecast(h=h2),series="Arima",PI=FALSE,size=1)+
    ggtitle(paste("Oil Production and Forecasts in",regions[i],sep=" "))+
    ylab("Production")
  print(plots.forecast.2[[i]])
  
}
saveRDS(plots.forecast.2, file = "plot-forecast-2.RDS")


#Comparing the test RMSE of all models to see which one performs better
model.name=c("mean","naive","drift","ets","arima")
acc=rep(NA,5)

for (i in 1:length(data.list)) {
  acc1=mean.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc2=naive.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc3=drift.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc4=ets.forecast.2[[i]]%>%forecast(h=h2,PI=FALSE)%>%accuracy(test.ts.2[[i]])
  acc5=arima.forecast.2[[i]]%>%forecast(h=h2)%>%accuracy(test.ts.2[[i]])
  index=c(acc1[4],acc2[4],acc3[4],acc4[4],acc5[4])%>%as.numeric()%>%which.min()
  print(paste(paste("Best model for",regions[i],sep = " "),model.name[index],sep = " is "))
}

#Here we obtained arima 3 times, drift 4 times and naive one time