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
#verschiedene Arten von Tests anwenden
#1. Testen auf Stationarität/Unit Root mit ADF Test H0: nicht stationär H1: stationär
adf.test(rUSD)#signifikant
adf.test(rCHF)#signifikant
adf.test(rGBP)#signifikant
#alle Renditereihen sind stationär, ADF auf 1% Level signifikant
#2. Testen auf ARCH Effekt mit LM-Test H0: keine ARCH-Effekte
ArchTest(rUSD)#signifikant
ArchTest(rCHF)#signifikant
ArchTest(rGBP)#signifikant
#alle Reihen weisen ARCH-Effekt auf, alle auf 1% Level signifikant
#3. Testen ob eine Normalverteilung vorliegt H0: Normlaverteilung liegt vor
jarque.bera.test(rUSD)#signifikant
jarque.bera.test(rCHF)#signifikant
jarque.bera.test(rGBP)#signifikant
#den Reihen liegen keine Normalverteilungen zugrunde