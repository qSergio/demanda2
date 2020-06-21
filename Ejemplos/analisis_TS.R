library(tidyverse)
library(fpp2)
library(seasonal)
library(tseries) 
library(TTR) #SMA
library(TSA) #Fourier
library(forecast)
library(stats)
library(plotly)

acti <- read.csv("training_data_d.csv")
activ <- (acti$unicas)/10000
activ <- activ[-c(149)]

activ1 <- log(activ)

n<-dim(activ1)
act<-act[-n,]
n<-dim(act)[1]
plot(acti$fecha,acti$unicas,type="l")


plot(forecast(auto.arima(ts(train,frequency=52),D=0),h=26))

  taylor.fit <- tbats(activ1)
plot(forecast(taylor.fit))
        #Estacionaria?
#acti$unicas/10000
adf.test(train, alternative = "stationary")

#Diferencias

act_dif<-diff(train, differences=1)

plot(act_dif, type="l")

plot(forecast(auto.arima(ts(act_dif,frequency=52),D=1),h=26))

#Si después de una diferencia aun parece que no es estacionaria:

adf.test(act_dif, alternative = "stationary", k=18)

adf.test(act_dif, alternative = "explosive")

# No se usará una diferencia, el test es con p=0.01 
# por lo que tenemos que aceptar la hipótesis alternativa
# stationary


# auto-correlación
act_dif = train
acf(act_dif, lag.max=104)
pacf(act_dif, lag.max=104)

#Patrones no muy regulares muestran algo de estacionalidad:

act_suave<-SMA(act_dif, n=26) #specify averaging over 26
plot(act_suave,type="l")

# Dominio de frecuencias

PGram<-periodogram(act_dif)

PGram_df = data.frame(freq=PGram$freq, spec=PGram$spec)
order = PGram_df[order(-PGram_df$spec),]
top5 = head(order, 8)
top5  

TimePeriod<-1/top5[7,1] # buscar la frecuencia de la serie
TimePeriod 

# Graficar con la diferencia

act_Freq18 <- ts(act_dif, frequency=52)

RDecomp<-decompose(act_Freq18)
plot(RDecomp)

#con regresión local

descomp <- stl(act_Freq18, s.window="periodic")
plot(descomp)

act_decomp <- seasadj(descomp)
plot(act_decomp)

plot(forecast(auto.arima(ts(act_decomp,frequency=52)),h=26))
##FALTA EL RUIDO

acf(as.numeric(act_decomp), lag.max = 52) 
pacf(as.numeric(act_decomp), lag.max = 104)

## AUTO.ARIMA

auto.arima(act_decomp)

act_arima<-stats::arima(act_decomp, order=c(0,1,2))
act_forecast <- forecast(act_arima, h=26)
act_forecast


#Errores
act_forecasts <- forecast(act_arima, h=26, level=c(99.5))
plot(act_forecasts)

acf(as.numeric(act_forecasts$residuals), lag.max=26)

Box.test(act_forecasts$residuals, lag=26, type="Ljung-Box")

#Here, p=0.9882 is greater than .05, suggesting that there are NO significant autocorrelations between successive forecasting errors.

plot(act_forecasts$residuals)

mean(act_forecasts$residuals)

