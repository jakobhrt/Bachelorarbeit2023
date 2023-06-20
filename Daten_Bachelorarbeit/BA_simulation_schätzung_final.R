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
#Simulationsstudie mit dem USD-GARCH(1,1)-Modell
#Modell spezifizieren
data_Wechselkurs_EURUSD <- read_excel("BA_Wechselkurs_EUR_USD_ab_2013.xlsx")
WechselkursUSD <- data_Wechselkurs_EURUSD$Wechselkurs
rUSD <- diff(log(WechselkursUSD))
garch_wn_set <- ugarchspec(mean.model = list(armaOrder = c(0,0)),variance.model = list(model = "sGARCH"),distribution.model = 'norm')
garch_wn <- ugarchspec(mean.model = list(armaOrder = c(0,0)),variance.model = list(model = "sGARCH"),distribution.model = 'norm')
USD_garch_wn <- ugarchfit(data = rUSD, spec = garch_wn)
setfixed(garch_wn) <- as.list(coef(USD_garch_wn))
#Koefizienten als Vektor festlegen
mu <- coef(USD_garch_wn)[[1]]
omega <- coef(USD_garch_wn)[[2]]
alpha <- coef(USD_garch_wn)[[3]]
beta <- coef(USD_garch_wn)[[4]]
true_values <- c(mu,omega,alpha,beta)
#Funktionen implementieren
#Funktion: verändert die Werte nicht für apply 
unveraenderteFunktion <- function(werte) {
  unveraenderteWerte <- werte
  return(unveraenderteWerte)
}
#Funktion: berechnet Konfidenzintervalle der Parameter nach der Schätzung
confidence_intervals <- function(data) {
  n_params <- 4
  m_sim <- ncol(data)
  estimated_parameters <- matrix(NA, nrow = m_sim, ncol = 4)
  parameter_intervals <- numeric(length = n_params * 2)
  for (i in 1:m_sim) {
    fit <- ugarchfit(spec = garch_wn_set, data = data[, i], solver = "hybrid")
    estimated_parameters[i, ] <- coef(fit)
  }
  for (j in 1:n_params){
    se<-sd(estimated_parameters[,j])
    Wert <- mean(estimated_parameters[,j])
    error<-qnorm(0.975)*se/sqrt(m_sim)
    parameter_intervals[j*2-1] <- Wert - error
    parameter_intervals[j*2] <- Wert + error
  }
  
  return(parameter_intervals)
}
#Funktion: berechnet für jeden einzelnen Parameter ein 95% KI und berechnet dann wie viel Prozent der KI die wahren Parameter überdecken
coverage <- function(data) {
  n_params <- 4
  m_sim <- ncol(data)
  estimated_parameters <- matrix(NA, nrow = m_sim, ncol = 4)
  parameter_intervals <- matrix(NA, nrow = m_sim, ncol = n_params * 2)
  coverage_percentages <- numeric(n_params)
  for (i in 1:m_sim) {
    fit <- ugarchfit(spec = garch_wn_set, data = data[, i], solver = "hybrid")
    estimated_parameters[i, ] <- coef(fit)
  }
  
  for (k in 1:m_sim) {
    for (l in 1:n_params){
      Wert <- estimated_parameters[k,l]
      parameter_intervals[k, l * 2 - 1] <- Wert - qnorm(0.975)*sd(estimated_parameters[,l])
      parameter_intervals[k, l * 2] <- Wert + qnorm(0.975)*sd(estimated_parameters[,l])
      if (true_values[l] >= parameter_intervals[k, l * 2 - 1] && true_values[l] <= parameter_intervals[k, l * 2]) {
        coverage_percentages[l] <- coverage_percentages[l] + 1
      }
    }
  }
  coverage_percentages <- coverage_percentages / m_sim
  return(coverage_percentages)
}
#Simualtionen durchführen und dazu passende Matrizen erstellen
#Simulation mit 2000 Beobachtungen
USD_garch_sim <- ugarchpath(spec = garch_wn,m.sim = 1000, n.sim = 2000, n.start=200, rseed = 1)
USD_garch_simS <- USD_garch_sim@path$seriesSim
#Simulation mit 1000 Beobachtungen
USD_garch_sim2 <- ugarchpath(spec = garch_wn,m.sim = 1000, n.sim = 1000, n.start=200, rseed = 2)
USD_garch_simS2 <- USD_garch_sim2@path$seriesSim
#Simulation mit 500 Beobachtungen
USD_garch_sim3 <- ugarchpath(spec = garch_wn,m.sim = 1000, n.sim = 500, n.start=200,  rseed = 3)
USD_garch_simS3 <- USD_garch_sim3@path$seriesSim
#Simulation mit 100 Beobachtungen
USD_garch_sim4 <- ugarchpath(spec = garch_wn,m.sim = 1000, n.sim = 100, n.start=200, rseed = 4)
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
#Coverage berchnen
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
matplot(r, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite", main="Simulation GARCH(1,1)")
v <- apply(sigma(USD_garch_sim), 2, 'unveraenderteFunktion')
matplot(v, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität", main="Simulation GARCH(1,1)")
#1000 Beobachtungen
r2 <- apply(USD_garch_simS2, 2, 'unveraenderteFunktion')
matplot(r2, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite", main="Simulation GARCH(1,1)")
v2 <- apply(sigma(USD_garch_sim2), 2, 'unveraenderteFunktion')
matplot(v2, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität", main="Simulation GARCH(1,1)")
#500 Beobachtungen
r3 <- apply(USD_garch_simS3, 2, 'unveraenderteFunktion')
matplot(r3, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite", main="Simulation GARCH(1,1)")
v3 <- apply(sigma(USD_garch_sim3), 2, 'unveraenderteFunktion')
matplot(v3, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität", main="Simulation GARCH(1,1)")
#100 Beobachtungen
r4 <- apply(USD_garch_simS4, 2, 'unveraenderteFunktion')
matplot(r4, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Rendite", main="Simulation GARCH(1,1)")
v4 <- apply(sigma(USD_garch_sim4), 2, 'unveraenderteFunktion')
matplot(v4, type = "l", lwd = 1, xlab="Beobachtungen", ylab="Volatilität", main="Simulation GARCH(1,1)")