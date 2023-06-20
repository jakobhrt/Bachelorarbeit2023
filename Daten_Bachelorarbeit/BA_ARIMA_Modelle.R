rm(list = ls())
#packages
library(readxl)
library(tseries)
library(rugarch)
library(FinTS)
library(e1071)
library(forecast)
data_Wechselkurs_EURUSD <- read_excel("BA_Wechselkurs_EUR_USD_ab_2013.xlsx")
data_Wechselkurs_EURCHF <- read_excel("BA_Wechselkurs_EUR_CHF_ab_2013_V2.xlsx")
data_Wechselkurs_EURGBP <- read_excel("BA_Wechselkurs_EUR_GBP_ab_2013.xlsx")
WechselkursUSD <- data_Wechselkurs_EURUSD$Wechselkurs
WechselkursCHF <- data_Wechselkurs_EURCHF$Wechselkurs
WechselkursGBP<- data_Wechselkurs_EURGBP$Wechselkurs
#Renditen die genutzt werden
#USD
rUSD <- diff(log(WechselkursUSD))
#CHF
rCHF <- diff(log(WechselkursCHF))
#GBP
rGBP <- diff(log(WechselkursGBP))
#ARMA-Modelle testen
auto.arima(rUSD)#ar1
auto.arima(rCHF)#ar1
auto.arima(rGBP)#wn
#weitere Modelle
#USD
ar1_modell_USD=arima(rUSD, order = c(1,0,0))
ma1_modell_USD=arima(rUSD, order = c(0,0,1))
arma11_modell_USD=arima(rUSD, order = c(1,0,1))
wn_modell_USD=arima(rUSD, order = c(0,0,0))
ar2_modell_USD=arima(rUSD, order = c(2,0,0))
ma2_modell_USD=arima(rUSD, order = c(0,0,2))
arma21_modell_USD=arima(rUSD, order = c(2,0,1))
arma12_modell_USD=arima(rUSD, order = c(1,0,2))
arma22_modell_USD=arima(rUSD, order = c(2,0,2))
ar1_modell_USD
ar2_modell_USD
ma1_modell_USD
ma2_modell_USD
arma11_modell_USD
arma21_modell_USD
arma12_modell_USD
arma22_modell_USD
wn_modell_USD
#CHF
ar1_modell_CHF=arima(rCHF, order = c(1,0,0))
ma1_modell_CHF=arima(rCHF, order = c(0,0,1))
arma11_modell_CHF=arima(rCHF, order = c(1,0,1))
wn_modell_CHF=arima(rCHF, order = c(0,0,0))
ar2_modell_CHF=arima(rCHF, order = c(2,0,0))
ma2_modell_CHF=arima(rCHF, order = c(0,0,2))
arma21_modell_CHF=arima(rCHF, order = c(2,0,1))
arma12_modell_CHF=arima(rCHF, order = c(1,0,2))
arma22_modell_CHF=arima(rCHF, order = c(2,0,2))
ar1_modell_CHF
ar2_modell_CHF
ma1_modell_CHF
ma2_modell_CHF
arma11_modell_CHF
arma21_modell_CHF
arma12_modell_CHF
arma22_modell_CHF
wn_modell_CHF
#GBP
ar1_modell_GBP=arima(rGBP, order = c(1,0,0))
ma1_modell_GBP=arima(rGBP, order = c(0,0,1))
arma11_modell_GBP=arima(rGBP, order = c(1,0,1))
wn_modell_GBP=arima(rGBP, order = c(0,0,0))
ar2_modell_GBP=arima(rGBP, order = c(2,0,0))
ma2_modell_GBP=arima(rGBP, order = c(0,0,2))
arma21_modell_GBP=arima(rGBP, order = c(2,0,1))
arma12_modell_GBP=arima(rGBP, order = c(1,0,2))
arma22_modell_GBP=arima(rGBP, order = c(2,0,2))
ar1_modell_GBP
ar2_modell_GBP
ma1_modell_GBP
ma2_modell_GBP
arma11_modell_GBP
arma21_modell_GBP
arma12_modell_GBP
arma22_modell_GBP
wn_modell_GBP