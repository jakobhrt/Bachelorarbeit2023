rm(list = ls())
#packages
library(readxl)
library(tseries)
library(rugarch)
library(FinTS)
library(e1071)
library(forecast)
library(quantmod)
library(xts)
library(PerformanceAnalytics)
library(fGarch)
#Daten einlesen
data_Wechselkurs_EURUSD <- read_excel("BA_Wechselkurs_EUR_USD_ab_2013.xlsx")
data_Wechselkurs_EURCHF <- read_excel("BA_Wechselkurs_EUR_CHF_ab_2013_V2.xlsx")
data_Wechselkurs_EURGBP <- read_excel("BA_Wechselkurs_EUR_GBP_ab_2013.xlsx")
WechselkursUSD <- data_Wechselkurs_EURUSD$Wechselkurs
WechselkursCHF <- data_Wechselkurs_EURCHF$Wechselkurs
WechselkursGBP<- data_Wechselkurs_EURGBP$Wechselkurs
#Renditen, die genutzt werden
rUSD <- diff(log(WechselkursUSD))
rCHF <- diff(log(WechselkursCHF))
rGBP <- diff(log(WechselkursGBP))
#empirische Kennzahlen
#für USD
c(min(rUSD),max(rUSD),median(rUSD),mean(rUSD))
c(var(rUSD),sd(rUSD),kurtosis(rUSD),skewness(rUSD))
#für CHF
c(min(rCHF),max(rCHF),median(rCHF),mean(rCHF))
c(var(rCHF),sd(rCHF),kurtosis(rCHF),skewness(rCHF))
#für GBP
c(min(rGBP),max(rGBP),median(rGBP),mean(rGBP))
c(var(rGBP),sd(rGBP),kurtosis(rGBP),skewness(rGBP))
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
#verschiedene Arten von Tests anwenden
#1. Augumented-Dickey-Fuller-Test
adf.test(rUSD)
adf.test(rCHF)
adf.test(rGBP)
#2. Testen auf ARCH Effekt mit LM-Test
ArchTest(rUSD)
ArchTest(rCHF)
ArchTest(rGBP)
#3. Testen ob eine Normalverteilung vorliegt
jarque.bera.test(rUSD)
jarque.bera.test(rCHF)
jarque.bera.test(rGBP)
#Plots
#Wechselkurse
plot.ts(WechselkursUSD,xlab="Beobachtungen", ylab="Kurs", main="EUR/USD")
plot.ts(WechselkursCHF,xlab="Beobachtungen", ylab="Kurs", main="EUR/CHF")
plot.ts(WechselkursGBP,xlab="Beobachtungen", ylab="Kurs", main="EUR/GBP")
#Renditen
plot.ts(rUSD, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/USD")
plot.ts(rCHF, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/CHF")
plot.ts(rGBP, xlab="Beobachtungen", ylab="Log-Rendite", main="EUR/GBP")
#ACF
acf(rUSD, main="EUR/USD")
acf(rCHF, main="EUR/CHF")
acf(rGBP, main="EUR/GBP")
#Arima-Modelle vergleichen
auto.arima(rUSD)
auto.arima(rCHF)
auto.arima(rGBP)
#weitere Modelle testen
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
#GARCH-Modelle schätzen
#verschiedene Garch(1,1)-Modelle spezifizieren mit bedingter Normalverteilung
garch11_wn <- ugarchspec(variance.model = list(garchOrder=c(1,1)),
                         mean.model = list(armaOrder=c(0,0)))
garch11_ar1 <- ugarchspec(variance.model = list(garchOrder=c(1,1)),
                          mean.model = list(armaOrder=c(1,0)))
egarch_wn=ugarchspec(variance.model = list(model="eGARCH",garchOrder=c(1,1)),
                     mean.model = list(armaOrder=c(0,0)))
gjrgarch11_wn=ugarchspec(variance.model = list(model="gjrGARCH",
                        garchOrder=c(1,1)),
                        mean.model = list(armaOrder=c(0,0)))
#verschiedene GARCH(1,1)-Modelle spezifizieren mit bedingter t-Verteilung
garch11_wn_std <- ugarchspec(variance.model = list(garchOrder=c(1,1)),
                             mean.model = list(armaOrder=c(0,0)),
                             distribution.model = "std")
egarch_wn_std=ugarchspec(variance.model = list(model="eGARCH",
                         garchOrder=c(1,1)),
                         mean.model = list(armaOrder=c(0,0)),
                         distribution.model = "std")
gjrgarch11_wn_std=ugarchspec(variance.model = list(model="gjrGARCH",
                             garchOrder=c(1,1)),
                             mean.model = list(armaOrder=c(0,0)),
                             distribution.model = "std")
#Modelle anpassen
#USD Modelle mit bedingter Normalverteilung
#1. AR(1) als mean modell
USD_garch11_ar1=ugarchfit(garch11_ar1, data = rUSD)
#2. wn als mean modell
USD_garch11_wn=ugarchfit(garch11_wn, data = rUSD)
USD_egarch11_wn=ugarchfit(egarch_wn, data = rUSD)
USD_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rUSD)
#USD mit bedingter t-Verteilung
USD_garch11_wn_std=ugarchfit(garch11_wn_std, data = rUSD)
USD_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rUSD)
USD_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rUSD)
#CHF Modelle mit Normalverteilung
#1. ar1 als mean modell
CHF_garch11_ar1=ugarchfit(garch11_ar1, data = rCHF)
#2. wn als mean modell
CHF_garch11_wn=ugarchfit(garch11_wn, data = rCHF)
CHF_egarch11_wn=ugarchfit(egarch_wn, data = rCHF)
CHF_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rCHF, solver = "hybrid")
#CHF mit bedingter t-Verteilung
CHF_garch11_wn_std=ugarchfit(garch11_wn_std, data = rCHF)
CHF_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rCHF)
CHF_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rCHF)
#GBP Modelle mit Normalverteilung 
GBP_garch11_wn=ugarchfit(garch11_wn, data = rGBP)
GBP_egarch11_wn=ugarchfit(egarch_wn, data = rGBP)
GBP_gjrgarch11_wn=ugarchfit(gjrgarch11_wn, data = rGBP)
#GBP mit bedingter t-Verteilung
GBP_garch11_wn_std=ugarchfit(garch11_wn_std, data = rGBP)
GBP_egarch11_wn_std=ugarchfit(egarch_wn_std, data = rGBP)
GBP_gjrgarch11_wn_std=ugarchfit(gjrgarch11_wn_std, data = rGBP)
#News Impact Curves 
plot(USD_garch11_wn)
plot(USD_egarch11_wn)
plot(USD_gjrgarch11_wn)
#Simulationsstudie
#Modell spezifizieren auf dessden Basis simuliert wird 
garch_wn_set <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                           variance.model = list(model = "sGARCH"),
                           distribution.model = 'norm')
garch_wn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),
                       variance.model = list(model = "sGARCH"),
                       distribution.model = 'norm')
USD_garch_wn <- ugarchfit(data = rUSD, spec = garch_wn)
setfixed(garch_wn) <- as.list(coef(USD_garch_wn))
#Koefizienten als Vektor definieren
mu <- coef(USD_garch_wn)[[1]]
omega <- coef(USD_garch_wn)[[2]]
alpha <- coef(USD_garch_wn)[[3]]
beta <- coef(USD_garch_wn)[[4]]
true_values <- c(mu,omega,alpha,beta)
#Funktionen implementieren
#Funktion: verändert die Werte nicht für apply 
unveraenderteFunktion <- function(werte) {
      unveraenderteWerte <- werte
return(unveraenderteWerte)}
#Funktion: berechnet Konfidenzintervalle der Parameter nach der Schätzung
confidence_intervals <- function(data) {
  n_params <- 4
  m_sim <- ncol(data)
  estimated_parameters <- matrix(NA, nrow = m_sim, ncol = 4)
  parameter_intervals <- numeric(length = n_params * 2)
  for (i in 1:m_sim) {
    fit <- ugarchfit(spec = garch_wn_set, data = data[, i], solver = "hybrid")
    estimated_parameters[i, ] <- coef(fit)}
    for (j in 1:n_params){
      se<-sd(estimated_parameters[,j])
      Wert <- mean(estimated_parameters[,j])
      error<-qnorm(0.975)*se/sqrt(m_sim)
      parameter_intervals[j*2-1] <- Wert - error
      parameter_intervals[j*2] <- Wert + error}
return(parameter_intervals)}
#Funktion: berechnet für jeden einzelnen Parameter ein 95% KI und berechnet
#dann wie viel Prozent der KI die wahren Parameter überdecken
coverage <- function(data) {
  n_params <- 4
  m_sim <- ncol(data)
  estimated_parameters <- matrix(NA, nrow = m_sim, ncol = 4)
  parameter_intervals <- matrix(NA, nrow = m_sim, ncol = n_params * 2)
  coverage_percentages <- numeric(n_params)
  for (i in 1:m_sim) {
    fit <- ugarchfit(spec = garch_wn_set, data = data[, i], solver = "hybrid")
    estimated_parameters[i, ] <- coef(fit)}
  for (k in 1:m_sim) {
    for (l in 1:n_params){
      Wert <- estimated_parameters[k,l]
      parameter_intervals[k, l * 2 - 1] <- Wert - 
                                      qnorm(0.975)*sd(estimated_parameters[,l])
      parameter_intervals[k, l * 2] <- Wert +
                                      qnorm(0.975)*sd(estimated_parameters[,l])
      if (true_values[l] >= parameter_intervals[k, l * 2 - 1] &&
          true_values[l] <= parameter_intervals[k, l * 2]) {
        coverage_percentages[l] <- coverage_percentages[l] + 1}}}
  coverage_percentages <- coverage_percentages / m_sim
return(coverage_percentages)}
#Simualtionen durchführen und dazu passende Matrizen erstellen
#Simulation mit 2000 Beobachtungen
USD_garch_sim <- ugarchpath(spec = garch_wn,m.sim = 1000,
                            n.sim = 2000, n.start=200, rseed = 1)
USD_garch_simS <- USD_garch_sim@path$seriesSim
#Simulation mit 1000 Beobachtungen
USD_garch_sim2 <- ugarchpath(spec = garch_wn,m.sim = 1000,
                             n.sim = 1000, n.start=200, rseed = 2)
USD_garch_simS2 <- USD_garch_sim2@path$seriesSim
#Simulation mit 500 Beobachtungen
USD_garch_sim3 <- ugarchpath(spec = garch_wn,m.sim = 1000,
                             n.sim = 500, n.start=200,  rseed = 3)
USD_garch_simS3 <- USD_garch_sim3@path$seriesSim
#Simulation mit 100 Beobachtungen
USD_garch_sim4 <- ugarchpath(spec = garch_wn,m.sim = 1000,
                             n.sim = 100, n.start=200, rseed = 4)
USD_garch_simS4 <- USD_garch_sim4@path$seriesSim
#Konfidenzintervalle berechnen
#2000 Beobachtungen
intervall <- confidence_intervals(USD_garch_simS)
#1000 Beobachtungen
intervall2 <- confidence_intervals(USD_garch_simS2)
#500 Beobachtungen
intervall3 <- confidence_intervals(USD_garch_simS3)
#100 Beobachtungen
intervall4 <- confidence_intervals(USD_garch_simS4)
#Coverage berechnen
#2000 Beobachtungen
cover <- coverage(USD_garch_simS)
#1000 Beobachtungen
cover2 <- coverage(USD_garch_simS2)
#500 Beobachtungen
cover3 <- coverage(USD_garch_simS3)
#100 Beobachtungen
cover4 <- coverage(USD_garch_simS4)
#Renditen und Volatilitäten ploten
#2000 Beobachtungen
r <- apply(USD_garch_simS, 2, 'unveraenderteFunktion')
matplot(r, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite",
        main="Simulation GARCH(1,1)")
v <- apply(sigma(USD_garch_sim), 2, 'unveraenderteFunktion')
matplot(v, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität",
        main="Simulation GARCH(1,1)")
#1000 Beobachtungen
r2 <- apply(USD_garch_simS2, 2, 'unveraenderteFunktion')
matplot(r2, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite",
        main="Simulation GARCH(1,1)")
v2 <- apply(sigma(USD_garch_sim2), 2, 'unveraenderteFunktion')
matplot(v2, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität",
        main="Simulation GARCH(1,1)")
#500 Beobachtungen
r3 <- apply(USD_garch_simS3, 2, 'unveraenderteFunktion')
matplot(r3, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite",
        main="Simulation GARCH(1,1)")
v3 <- apply(sigma(USD_garch_sim3), 2, 'unveraenderteFunktion')
matplot(v3, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität",
        main="Simulation GARCH(1,1)")
#100 Beobachtungen
r4 <- apply(USD_garch_simS4, 2, 'unveraenderteFunktion')
matplot(r4, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite",
        main="Simulation GARCH(1,1)")
v4 <- apply(sigma(USD_garch_sim4), 2, 'unveraenderteFunktion')
matplot(v4, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität",
        main="Simulation GARCH(1,1)")