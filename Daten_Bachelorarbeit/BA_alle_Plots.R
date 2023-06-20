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
#PLOTS
#Wechselkurse plotten
plot.ts(WechselkursUSD,xlab="Beobachtungen", ylab="Kurs", main="EUR/USD")
plot.ts(WechselkursCHF,xlab="Beobachtungen", ylab="Kurs", main="EUR/CHF")
plot.ts(WechselkursGBP,xlab="Beobachtungen", ylab="Kurs", main="EUR/GBP")
#Renditen plotten
plot.ts(rUSD, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/USD")
plot.ts(rCHF, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/CHF")
plot.ts(rGBP, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/GBP")
#ACF
acf(rUSD, main="EUR/USD")
acf(rCHF, main="EUR/CHF")
acf(rGBP, main="EUR/GBP")
#so gut wie keine Auschläge über Konfidenzbänder--> White Noise
#NIC der USD-Modelle
garch11_wn <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
egarch_wn=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
gjrgarch11_wn=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
USD_garch11_wn=ugarchfit(garch11_wn, data = rUSD)
USD_egarch11_wn=ugarchfit(egarch_wn, data = rUSD)
USD_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rUSD)
plot(USD_garch11_wn)
plot(USD_egarch11_wn)
plot(USD_gjrgarch11_wn)
