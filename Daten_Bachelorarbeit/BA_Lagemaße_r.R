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
#empirische Kennzahlen für die verschiedenen Reihen berechnen
#für USD
min(rUSD)
max(rUSD)
median(rUSD)
mean(rUSD)
var(rUSD)
sd(rUSD)
kurtosis(rUSD)
skewness(rUSD)
#für CHF
min(rCHF)
max(rCHF)
median(rCHF)
mean(rCHF)
var(rCHF)
sd(rCHF)
kurtosis(rCHF)
skewness(rCHF)
#für GBP
min(rGBP)
max(rGBP)
median(rGBP)
mean(rGBP)
var(rGBP)
sd(rGBP)
kurtosis(rGBP)
skewness(rGBP)
#wie viel Prozent der Beobachtungen liegen innerhalb von -1% und 1%
#USD
USD_Anzahl_innerhalb_desIntervalls <- sum(rUSD >= -0.01 & rUSD <= 0.01)
USD_Anzahl_innerhalb_desIntervalls/2657
#CHF
CHF_Anzahl_innerhalb_desIntervalls <- sum(rCHF >= -0.01 & rCHF <= 0.01)
CHF_Anzahl_innerhalb_desIntervalls/2136
#GBP
GBP_Anzahl_innerhalb_desIntervalls <- sum(rGBP >= -0.01 & rGBP <= 0.01)
GBP_Anzahl_innerhalb_desIntervalls/2657