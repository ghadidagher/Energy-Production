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

#Also loading the arima and ets models when h=20
all.ets=readRDS(file = "ets-forecast-2.RDS")

#Taking world data alone
ets.world.1=all.ets[[8]]

#Optimizing the ets model
#First lets review the plot
plots[[8]]
#If we look at the automatic ets model 
ets.world.1
#We see that the chosen a MAN model. In fact by looking at the oil production
#curve we see no seasonality thus the N part is very logical and expected
#For the A part. We have 4 possibilities A, Ad, M, or N. We know it's not
#N because we have a trend
#It's not M because multiplicative trends are never a good option
#Also it's not Ad because the curve continue to rise
#So the A is also logical and expected 
#Finally for the M part we had 2 possibilities A or M. Both are possible
#The algorithm chose M because the model gave lower BIC

#Yet if we want to optimize the model we should try different combination
#But according to the explication given above the only possible model to test
#is the AAN model
#To compare between both models a cross validation is done then we compare 
#the CV errors and test errors for both models
all.train.data=readRDS(file = "train-ts-data-2.RDS")
all.test.data=readRDS(file = "test-ts-data-2.RDS")

train.data=all.train.data[[8]]
test.data=all.test.data[[8]]

ets.world.2=ets(train.data,model = "AAN")#the BIC is larger for this model

#Performing CV 
#The function tsCV requires that we write the functions like that
fets.1 <- function(x,h) {
  forecast(ets(x,model = "MAN"), h = h, PI=FALSE)
}

fets.2 <- function(x,h) {
  forecast(ets(x,model = "AAN"), h = h, PI=FALSE)
}

e1=tsCV(train.data, fets.1, h=1) # Compute CV errors for MAN as e1
e2=tsCV(train.data, fets.2, h=1) # Compute CV errors for AAN as e2
#the tsCV() function returns a residual and not an accuracy measure

#RMSE of each model 
rmse.1=mean(e1^2, na.rm=TRUE)%>%sqrt()
rmse.2=mean(e2^2, na.rm=TRUE)%>%sqrt()

rmse.1
rmse.2
#Thus the CV RMSE is lower for the MAN model

#MAE of each model
mae.1=e1%>%abs()%>%mean(na.rm=TRUE)
mae.2=e2%>%abs()%>%mean(na.rm=TRUE)

mae.1
mae.2

#also the CV MAE is lower for the MAN model
#Thus the MAN model is the best ets model possible

#Let's calculate the test error for this model
ets.world.1%>%forecast(h=20,PI=FALSE)%>%accuracy(test.data)

#we can see that the train RMSE is similar to the CV RMSE yet the test RMSE 
#is approximately 5 times bigger 

#Since we couldn't optimize the ets any further, the arima model remains the best
#for the world data when h=20

#Plotting the forecast
all.oil=readRDS(file="oil-ts-data.RDS")
world.oil=all.oil[[8]]
world.oil%>%autoplot(size=1)+
  autolayer(ets.world.1%>%forecast(h=20,PI=FALSE),series = "ETS forecast",size=1)