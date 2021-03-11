library(tidyverse)
library(caret)
library(fpp2)

#In this part we will see if the ets() function could be further optimized
#We will use only the world data
#In our case a cross validation may be used 


#Loading data and plots
data.list=readRDS("datalist.RDS")
data1=readRDS("data1.RDS")
plots=readRDS("plotlist.RDS")


#Also loading the arima model when h=20
all.arima=readRDS(file = "arima-forecast-2.RDS")

#Taking world data alone
arima.world.1=all.arima[[8]]

#Optimizing the ets model
#First lets review the plot
plots[[8]]
#If we look at the automatic ets model 
arima.world.1

#The automatic arima function gave an ARIMA(5,2,0) model
#In this case d=2 and we have no constant thus 
#long-term forecasts will follow a straight line (add reference)


#In the case of arima we can't analyze as we did with ets
#However it is logical and expected not to obtain a seasonal part
#Here to optimize we need to test different models by adding or subtracting 1
#from each element thus we will test ARIMA(4,2,0), ARIMA(6,2,0) and ARIMA(5,2,1)
#we will keep d=2 for stationarity 

#To compare between models a cross validation is done then we compare 
#the CV errors and test errors for both models
all.train.data=readRDS(file = "train-ts-data-2.RDS")
all.test.data=readRDS(file = "test-ts-data-2.RDS")

train.data=all.train.data[[8]]
test.data=all.test.data[[8]]

arima.world.2=Arima(train.data,order = c(4,2,0),seasonal = c(0,0,0) )
arima.world.3=Arima(train.data,order = c(6,2,0),seasonal = c(0,0,0) )
arima.world.4=Arima(train.data,order = c(5,2,1),seasonal = c(0,0,0) )

#Performing CV 
#The function tsCV requires that we write the functions like that
farima <- function(x, h,p,q) {
  forecast(Arima(x,order = c(p,2,q),seasonal = c(0,0,0)), h=h)
}

e1=tsCV(train.data, farima, h=1,p=5,q=0) # Compute CV errors for ARIMA(5,2,0) as e1
e2=tsCV(train.data, farima, h=1,p=4,q=0) # Compute CV errors for ARIMA(4,2,0) as e2
e3=tsCV(train.data, farima, h=1,p=6,q=0) # Compute CV errors for ARIMA(6,2,0) as e3
e4=tsCV(train.data, farima, h=1,p=5,q=1) # Compute CV errors for ARIMA(5,2,1) as e4

#RMSE of each model 
rmse.1=mean(e1^2, na.rm=TRUE)%>%sqrt()
rmse.2=mean(e2^2, na.rm=TRUE)%>%sqrt()
rmse.3=mean(e3^2, na.rm=TRUE)%>%sqrt()
rmse.4=mean(e4^2, na.rm=TRUE)%>%sqrt()

rmse.1
rmse.2
rmse.3
rmse.4
#Thus the CV RMSE is the lowest for the third model which is ARIMA(6,2,0)

#MAE of each model
mae.1=e1%>%abs()%>%mean(na.rm=TRUE)
mae.2=e2%>%abs()%>%mean(na.rm=TRUE)
mae.3=e3%>%abs()%>%mean(na.rm=TRUE)
mae.4=e4%>%abs()%>%mean(na.rm=TRUE)

mae.1
mae.2
mae.3
mae.4

#The CV MAE is the lowest for the fourth model which is ARIMA(5,2,1)

#The first model gave the worst results for both measures so we are sure that 
#it isn't the best arima model

#However here each measure gave a different model 
#To make a decision we could either choose one the 2 either calculate a third measure
#We will calculate the MAPE
mape.3=(100*e3/train.data)%>%abs()%>%mean(na.rm=TRUE)
mape.4=(100*e4/train.data)%>%abs()%>%mean(na.rm=TRUE)

mape.3
mape.4

#Both models gave close results 4.35% and 4.85% 
#However we will choose the third one since it has a lower MAPE and RMSE

#Let's calculate the test error for this model and for the initial one
arima.world.1%>%forecast(h=20)%>%accuracy(test.data)
arima.world.3%>%forecast(h=20)%>%accuracy(test.data)

#We see that the third model ARIMA(6,2,0) has lower RMSE, MAE and MAPE for the test set
#Plotting it next to the ARIMA (5,2,0) chosen automatically
all.oil=readRDS(file="oil-ts-data.RDS")
world.oil=all.oil[[8]]
world.oil%>%autoplot(size=1)+
  autolayer(arima.world.3%>%forecast(h=20),series = "Optimal ARIMA",size=1,PI=FALSE)+
  autolayer(arima.world.1%>%forecast(h=20),series = "Auto ARIMA",size=1,PI=FALSE)

#By plotting we see that the improvement is very small