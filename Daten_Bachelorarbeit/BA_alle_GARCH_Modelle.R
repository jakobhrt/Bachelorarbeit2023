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
#verschiedene Garch(1,1)-Modelle spezifizieren mit bedingter Normalverteilung
garch11_wn <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
garch11_ar1 <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)))
garch11_arma11 <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
#EGARCH-Modelle spezifizieren
egarch_wn=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
egarch11_ar1=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)))
egarch11_arma11=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
#GJR-GARCH-Modelle spezifizieren
gjrgarch11_wn=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)))
gjrgarch11_ar1=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,0)))
gjrgarch11_arma11=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(1,1)))
#GARCH(1,1)-Modelle spezifizieren mit bedingter Student-t-Verteilung
garch11_wn_std <- ugarchspec(variance.model = list(garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)), distribution.model = "std")
egarch_wn_std=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)), distribution.model = "std")
gjrgarch11_wn_std=ugarchspec(variance.model = list(model="gjrGARCH",garchOrder=c(1,1)),mean.model = list(armaOrder=c(0,0)), distribution.model = "std")
#Modelle fitten
#alle USD Modelle
#1. wenn ar1 als mean-Modell genutzt werden kann
USD_garch11_ar1=ugarchfit(garch11_ar1, data = rUSD)
USD_egarch11_ar1=ugarchfit(egarch11_ar1, data = rUSD)
USD_gjrgarch11_ar1=ugarchfit(gjrgarch11_ar1, data = rUSD)
#2. falls wn als mean-Modell
USD_garch11_wn=ugarchfit(garch11_wn, data = rUSD)
USD_egarch11_wn=ugarchfit(egarch_wn, data = rUSD)
USD_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rUSD)
#USD wn mit Student-t-Verteilung
USD_garch11_wn_std=ugarchfit(garch11_wn_std, data = rUSD)
USD_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rUSD)
USD_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rUSD)
#alle CHF Modell
#1. ar1 als mean-Modell
CHF_garch11_ar1=ugarchfit(garch11_ar1, data = rCHF)
CHF_egarch11_ar1=ugarchfit(egarch11_ar1, data = rCHF)
CHF_gjrgarch11_ar1=ugarchfit(gjrgarch11_ar1, data = rCHF)
#2. wn als mean-Modell
CHF_garch11_wn=ugarchfit(garch11_wn, data = rCHF)
CHF_egarch11_wn=ugarchfit(egarch_wn, data = rCHF)
CHF_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rCHF, solver = "hybrid")# solver hybrid muss hinzugefügt werden, da sonst convergence problem
#CHF wn mit Student-t-Verteilung
CHF_garch11_wn_std=ugarchfit(garch11_wn_std, data = rCHF)
CHF_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rCHF)
CHF_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rCHF)
#alle GBP Modelle, immer wn als mean-Modell
GBP_garch11_wn=ugarchfit(garch11_wn, data = rGBP)
GBP_egarch11_wn=ugarchfit(egarch_wn, data = rGBP)
GBP_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rGBP)
#GBP mit Student-t-Verteilung
GBP_garch11_wn_std=ugarchfit(garch11_wn_std, data = rGBP)
GBP_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rGBP)
GBP_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rGBP)
#USD mit ar1 wird nicht weiter verwednet, da ar1 nicht signifikant
USD_garch11_ar1
#USD mit wn
USD_garch11_wn
USD_egarch11_wn
USD_gjrgarch11_wn
#CHF mit ar1, wird nicht weiter verwendet, da ar1 nicht signifikant
CHF_garch11_ar1
#CHF mit wn
CHF_garch11_wn
CHF_egarch11_wn
CHF_gjrgarch11_wn
#GBP mit wn
GBP_garch11_wn
GBP_egarch11_wn
GBP_gjrgarch11_wn
#USD wn mit Student-t-Verteilung
USD_garch11_wn_std
USD_egarch11_wn_std
USD_gjrgarch11_wn_std
#CHF wn mit Student-t-Verteilung
CHF_garch11_wn_std
CHF_egarch11_wn_std
CHF_gjrgarch11_wn_std
#GBP wn mit Student-t-Verteilung
GBP_garch11_wn_std
GBP_egarch11_wn_std
GBP_gjrgarch11_wn_std

