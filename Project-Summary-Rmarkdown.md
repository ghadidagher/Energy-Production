Energy Production Project Summary
================
Ghadi Dagher
2/24/2021

The original data can be found on **Our World in Data** github page
available
[here](https://github.com/owid/owid-datasets/tree/ac5d2bf39b1fbc66e47a3040f71aabe1fdc92bff/datasets/Energy%20production%20by%20source%201900-2014%20-%20SHIFT%20Project%20\(2014\)).
Also it is available as a csv file here.

This project consist of 5 main parts:

**1. Data Wrangling**  
**2. Data Visualization**  
**3. Time Series Analysis and Forecasting**  
**4. Exponential Smoothing Optimization**  
**5. Arima Optimization**

The packages needed for this project are

``` r
library(tidyverse)
library(fpp2) #for time series analysis and forecasting
library(readxl)
```

# Data Wrangling

In the first part, data was imported from a csv file and transformed
into a data frame. Then column names were changed and columns were
transformed to numeric. The code below shows how it was done.

``` r
filename="Energy-production-by-source-1900-2014.csv"
fullpath=file.path(getwd(), filename)
data1=read_csv(fullpath)%>%as.data.frame()
colnames(data1)=c("Entity","Year","Oil","Renewables")
data1=data1%>%mutate(Oil=as.numeric(Oil),Renewables=as.numeric(Renewables))
```

Now that we have the data frame we created a list and each element of
that list is the data corresponding to one of the 8 regions or entities.
This separation is helpful later for time series analysis.The code below
shows how it was done.

``` r
data1=data1%>%mutate(Entity=as.factor(Entity))
regions=levels(data1$Entity)
data.list=rep(NA,length(regions))%>%as.list()
for (i in 1:length(regions)) {
data.list[[i]]=data1%>%filter(Entity==regions[i])  
}
```

The object regions is a character vector containing all the levels of
the column Entity.

``` r
regions=levels(data1$Entity)
regions
```

    ## [1] "Africa"                    "Asia and Oceania"         
    ## [3] "Central and South America" "Eurasia"                  
    ## [5] "Europe"                    "Middle East"              
    ## [7] "North America"             "World"

It is important to note that we should have transformed the data into a
tidy format. Nonetheless, this was only required for data visualization
in the second part. Other than that data.list was enough for our
analysis.  
Also, there are a lot of missing values in this data.These NAs indicate
that there were no oil or renewable energy production that year. Nothing
was done to handle these NAs as they are automatically removed.

# Data Visualization

In this part, we created some plots in order to understand data. First,
we plot the oil production as follows.

``` r
p1=data1%>%select(Entity,Year,Oil)%>%group_by(Entity)%>%
  ggplot(aes(x=Year,y=Oil,col=Entity))+geom_line(size=1)+
  ggtitle("Oil Production by Region")+ylab("Oil Production (Mtoe)")
print(p1)
```

![](Project-Summary-Rmarkdown_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Then we plot renewable energy production for all regions.

``` r
p2=data1%>%select(Entity,Year,Renewables)%>%group_by(Entity)%>%
  ggplot(aes(x=Year,y=Renewables,col=Entity))+geom_line(size=1)+
  ggtitle("Renewable Energy Production by Region")+
  ylab("Renewable Energy Production (Mtoe)")+
  scale_x_continuous(limits = c(1960,2018))
p2
```

![](Project-Summary-Rmarkdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Finally we compare oil and renewable energy production for each
region.Here, we have created a list of plots. Each element of the list
corresponds to the plot of one region.

``` r
regions=levels(data1$Entity)
p=rep(NA,length(regions))%>%as.list()
for (i in 1:length(regions)) {
  p[[i]]=data1%>%filter(Entity==regions[i])%>%
    gather(Type, Production,"Oil":"Renewables")%>%
    group_by(Type)%>%
    ggplot(aes(x=Year,y=Production,col=Type))+
    geom_line(size=1)+
    ggtitle(paste("Oil vs Renewable Energy In",regions[i],sep=" "))+
    ylab("Production (Mtoe)")
}
```

For example the plot for europe is given by

``` r
p[[5]]
```

![](Project-Summary-Rmarkdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

We can clearly see that oil production is much higher than renewable
energy. This is the case for all other regions.

# Time Series Analysis and Forecasting

In this part we will work with oil production only. First, we need to
divide our data into a training set and a test set. Since this is a time
series data, we cannot divide at random. Thus, we will take the data of
the last 20 years as a test set. All previous data will serve for
training.  
Remember that we have 8 time series (one for each region). Thus, we need
8 different training and test sets. For this reason we will use lists as
before.  
Note that before dividing data into train and test sets, we should
transform the oil data from a numeric vector to a ts object. The ts
object needed to use different functions in forecast package. We use the
ts() function to create a ts object.

``` r
firstyear=data1$Year[1]
lastyear=data1$Year[length(data1$Year)/8]

oil.ts=rep(NA,length(data.list))%>%as.list()
train.ts.2=rep(NA,length(data.list))%>%as.list()
test.ts.2=rep(NA,length(data.list))%>%as.list()

for (i in 1:length(data.list)) {
  oil.ts[[i]]=ts(data.list[[i]]$Oil, start=firstyear, end=lastyear, frequency=1)
  train.ts.2[[i]]=window(oil.ts[[i]],end=1994)
  test.ts.2[[i]]=window(oil.ts[[i]],start=1995)
  
}
```

For each region, we will forecast oil production using 5 methods which
are

1.The mean forecast: the forecast of future values is the mean of the
training data  
2.The naive forecast: the forecast of future data is equal to the last
value in the training data  
3.The drift forecast: the forecast of future data is determined using a
line created using the first and last point of the data  
4.The automatic exponential smoothing (ets) forecast: we let the ets()
function in R choose the best ets model  
5.The automatic arima forecast: we let the auto.arima() function in R
choose the best arima model

The first 3 methods are used as benchmarks (baseline). The ets and arima
models should beat the benchmarks in order to be taken into
consideration.

The choice of ets models is based on the model having the lowest AICc
given by

![formula](https://render.githubusercontent.com/render/math?math=AICc=-2log\(L\)%2B2k%2B\\frac%7Bk\(k%2B1\)%7D%7BT-k-1%7D)

where L is the likelihood of the model, k is the total number of
parameters and initial states that have been estimated (including the
residual variance) and T is the number of observations.

The choice of the arima model is a little bit more complicated and can
be found in [**Forecasting: Principles and
Practices**](https://otexts.com/fpp2/arima-estimation.html) chapter 8.
Nonetheless, it also uses the AICc given by

![formula](https://render.githubusercontent.com/render/math?math=AICc=-2log\(L\)%2B2\(p%2Bq%2Bk%2B1\)%2B\\frac%7B2\(p%2Bq%2Bk%2B1\)\(p%2Bq%2Bk%2B2\)%7D%7BT-p-q-k-2%7D)

where p and q are the autoregressive and moving average orders
respectively.  
The following code will allow us to create the 5 methods described above
for every region.

``` r
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
  arima.forecast.2[[i]]=auto.arima(train.ts.2[[i]], seasonal=FALSE, stepwise=FALSE, approximation = FALSE)
}
```

We can also create list of the plots containing the forecasts as follows

``` r
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
    ylab("Production (Mtoe)")
}
```

For example, for the world data

``` r
plots.forecast.2[[8]]
```

![](Project-Summary-Rmarkdown_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

Now it is important to see which model performed the best for each
region. To do so, we will compare the test RMSE of the 5 methods. The
winning method for each region will be printed on screen. We can do this
as follows

``` r
model.name=c("mean","naive","drift","ets","arima")
acc=rep(NA,5)

for (i in 1:length(data.list)) {
  acc1=mean.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc2=naive.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc3=drift.forecast.2[[i]]%>%accuracy(test.ts.2[[i]])
  acc4=ets.forecast.2[[i]]%>%forecast(h=h2,PI=FALSE)%>% accuracy(test.ts.2[[i]])
  acc5=arima.forecast.2[[i]]%>%forecast(h=h2)%>%accuracy(test.ts.2[[i]])
  index=c(acc1[4],acc2[4],acc3[4],acc4[4],acc5[4])%>%as.numeric()%>% which.min()
  print(paste(paste("Best model for",regions[i],sep = " "), model.name[index],sep = " is "))
}
```

    ## [1] "Best model for Africa is arima"
    ## [1] "Best model for Asia and Oceania is drift"
    ## [1] "Best model for Central and South America is arima"
    ## [1] "Best model for Eurasia is drift"
    ## [1] "Best model for Europe is naive"
    ## [1] "Best model for Middle East is drift"
    ## [1] "Best model for North America is drift"
    ## [1] "Best model for World is arima"

We can see that we obtained arima 3 times, drift 4 times and naive one
time. Thus it appears that the baseline methods perform better than
arima and ets.

# Exponential smoothing optimization

The **ets()** and **auto.arima()** functions are both automatic
functions that choose the best model based on the AICc values. When
working with automatic functions, we should always work with caution and
verify if we have the best possible model. In this part, we will try to
optimize the ets forecast for the world data only.

First, let’s take a look at the plot of the world oil energy production

``` r
data1%>%filter(Entity=="World")%>%
    select(Year,Oil)%>%
    ggplot(aes(x=Year,y=Oil))+
    geom_line(size=1)+
    ggtitle("Oil Energy Production In The World")+
    ylab("Production (Mtoe)")
```

![](Project-Summary-Rmarkdown_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->

If we look at the automatic ets model

``` r
ets.forecast.2[[8]]
```

    ## ETS(M,A,N) 
    ## 
    ## Call:
    ##  ets(y = train.ts.2[[i]], restrict = FALSE) 
    ## 
    ##   Smoothing parameters:
    ##     alpha = 0.9999 
    ##     beta  = 0.207 
    ## 
    ##   Initial states:
    ##     l = 17.909 
    ##     b = 2.2282 
    ## 
    ##   sigma:  0.0562
    ## 
    ##      AIC     AICc      BIC 
    ## 1041.152 1041.826 1053.921

We see that the chosen model is a MAN model. The first letter is for the
error which mean that the error here is multiplicative “M”. The second
letter is for the trend which is an additive trend in our case. The last
letter is for seasonality. Here the “N” indicates a “none” which means
that there is no seasonality.  
In fact by looking at the oil production curve we see no seasonality
thus the N part is very logical and expected. For the A part. We have 4
possibilities A, Ad (additive damped), M (multiplicative), or N (none).
We know it’s not N because we have a trend. It’s not M because
multiplicative trends are never a good option. Also it’s not Ad because
the curve continue to rise (it’s not damped). So the A is also logical
and expected. Finally for the M part we have 2 possibilities A or M.
Both are possible. The algorithm chose M because the model gave lower
AICc.

Thus if we want to optimize the model we should try all the logical
combinations. According to the explication given above the only possible
model to test is the AAN model.

We will create this model and compare the cross-validation RMSE and MAE.
To perform a cross validation using the **forecast** package, we should
create the following functions first.

``` r
fets.1 <- function(x,h) {
  forecast(ets(x,model = "MAN"), h = h, PI=FALSE)
}

fets.2 <- function(x,h) {
  forecast(ets(x,model = "AAN"), h = h, PI=FALSE)
}
```

We create the second model as follows

``` r
ets.world.AAN=ets(train.ts.2[[8]],model = "AAN")
```

The cross-validation is done as follows

``` r
e1=tsCV(train.ts.2[[8]], fets.1, h=1) # Compute CV errors for MAN as e1
e2=tsCV(train.ts.2[[8]], fets.2, h=1) # Compute CV errors for AAN as e2
#the tsCV() function returns a residual and not an accuracy measure

#RMSE of each model 
rmse.1=mean(e1^2, na.rm=TRUE)%>%sqrt()
rmse.2=mean(e2^2, na.rm=TRUE)%>%sqrt()

rmse.1
```

    ## [1] 57.28456

``` r
rmse.2
```

    ## [1] 69.06671

``` r
#Thus the CV RMSE is lower for the MAN model

#MAE of each model
mae.1=e1%>%abs()%>%mean(na.rm=TRUE)
mae.2=e2%>%abs()%>%mean(na.rm=TRUE)

mae.1
```

    ## [1] 30.51336

``` r
mae.2
```

    ## [1] 33.50609

``` r
#also the CV MAE is lower for the MAN model
#Thus the MAN model is the best ets model possible
```

We can see that CV-RMSE and CV-MAE for the MAN model are both lower than
the AAN model. As a result, it is not possible to optimize any more.

Finally let’s calculate the test RMSE for the MAN model.

``` r
test.RMSE=ets.forecast.2[[8]]%>%forecast(h=h2,PI=FALSE)%>% accuracy(test.ts.2[[8]])
test.RMSE[4]
```

    ## [1] 248.6922

# Arima optimization

In this part, we will try to optimize the arima forecast for the world
data.  
If we look at the automatic model

``` r
arima.forecast.2[[8]]
```

    ## Series: train.ts.2[[i]] 
    ## ARIMA(5,2,0) 
    ## 
    ## Coefficients:
    ##           ar1      ar2      ar3      ar4      ar5
    ##       -0.5082  -0.5354  -0.2976  -0.1111  -0.3961
    ## s.e.   0.0947   0.1074   0.1169   0.1065   0.0923
    ## 
    ## sigma^2 estimated as 2710:  log likelihood=-497.76
    ## AIC=1007.51   AICc=1008.49   BIC=1022.71

The automatic arima function gave an ARIMA(5,2,0) model. In this case
d=2 and we have no constant thus long-term forecasts will follow a
straight line ([ref](https://otexts.com/fpp2/non-seasonal-arima.html)).

In the case of arima we can’t analyze as we did with ets. However it is
logical and expected not to obtain a seasonal part. Here to optimize we
need to test different models by adding or subtracting 1 from each
element thus we will test ARIMA(4,2,0), ARIMA(6,2,0) and ARIMA(5,2,1).
Note that we will keep d=2 for stationarity.

We will create the models as follows

``` r
arima.world.2=Arima(train.ts.2[[8]],order = c(4,2,0),seasonal = c(0,0,0) )
arima.world.3=Arima(train.ts.2[[8]],order = c(6,2,0),seasonal = c(0,0,0) )
arima.world.4=Arima(train.ts.2[[8]],order = c(5,2,1),seasonal = c(0,0,0) )
```

Cross-validation is done as before

``` r
#The function tsCV requires that we write the functions like that
farima <- function(x, h,p,q) {
  forecast(Arima(x,order = c(p,2,q),seasonal = c(0,0,0)), h=h)
}

e1=tsCV(train.ts.2[[8]], farima, h=1,p=5,q=0) # Compute CV errors for ARIMA(5,2,0) as e1
e2=tsCV(train.ts.2[[8]], farima, h=1,p=4,q=0) # Compute CV errors for ARIMA(4,2,0) as e2
e3=tsCV(train.ts.2[[8]], farima, h=1,p=6,q=0) # Compute CV errors for ARIMA(6,2,0) as e3
e4=tsCV(train.ts.2[[8]], farima, h=1,p=5,q=1) # Compute CV errors for ARIMA(5,2,1) as e4

#RMSE of each model 
rmse.1=mean(e1^2, na.rm=TRUE)%>%sqrt()
rmse.2=mean(e2^2, na.rm=TRUE)%>%sqrt()
rmse.3=mean(e3^2, na.rm=TRUE)%>%sqrt()
rmse.4=mean(e4^2, na.rm=TRUE)%>%sqrt()

rmse.1
```

    ## [1] 79.72128

``` r
rmse.2
```

    ## [1] 72.10685

``` r
rmse.3
```

    ## [1] 62.10222

``` r
rmse.4
```

    ## [1] 65.97491

``` r
#Thus the CV RMSE is the lowest for the third model which is ARIMA(6,2,0)

#MAE of each model
mae.1=e1%>%abs()%>%mean(na.rm=TRUE)
mae.2=e2%>%abs()%>%mean(na.rm=TRUE)
mae.3=e3%>%abs()%>%mean(na.rm=TRUE)
mae.4=e4%>%abs()%>%mean(na.rm=TRUE)

mae.1
```

    ## [1] 46.92801

``` r
mae.2
```

    ## [1] 40.97342

``` r
mae.3
```

    ## [1] 40.14339

``` r
mae.4
```

    ## [1] 37.12127

``` r
#The CV MAE is the lowest for the fourth model which is ARIMA(5,2,1)
```

The first model gave the worst results for both measures so we are sure
that it isn’t the best arima model. However here each measure gave a
different model. To make a decision we could either choose one the 2
either calculate a third measure We will calculate the MAPE.

``` r
mape.3=(100*e3/train.ts.2[[8]])%>%abs()%>%mean(na.rm=TRUE)
mape.4=(100*e4/train.ts.2[[8]])%>%abs()%>%mean(na.rm=TRUE)

mape.3
```

    ## [1] 4.359353

``` r
mape.4
```

    ## [1] 4.85844

``` r
#Both models gave close results 4.36% and 4.86% 
#However we will choose the third one since it has a lower MAPE and RMSE
```

Now, let’s calculate the test error for this model and for the initial
one

``` r
initial.model=arima.forecast.2[[8]]%>%forecast(h=20)%>%accuracy(test.ts.2[[8]])
initial.model[4]
```

    ## [1] 229.6736

``` r
optimized.model=arima.world.3%>%forecast(h=20)%>%accuracy(test.ts.2[[8]])
optimized.model[4]
```

    ## [1] 202.7681

We see that the third model ARIMA(6,2,0) has lower RMSE, MAE and MAPE
for the test set. Thus, the ARIMA(6,2,0) is the optimized model.
