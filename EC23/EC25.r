#Primero, leemos los datos
parking <- read.table("datos_EC25.txt", header = TRUE)

#Extraemos los tiempos entre llegadas al parqueadero vecino y evaluamos su distribución
arrival_parking <- parking$dt_min
#Al parecer, los tiempos entre llegadas distribuyen exponencialmente. Vamos a comprobarlo realizando una prueba de bondad de ajuste.
hist(arrival_parking, main='tiempos de llegada al parqueadero')
library(MASS)
fit1 <- fitdistr(arrival_parking, "exponential")
ks.test(arrival_parking, "pexp", fit1$estimate)

#Por lo anterior (valor p > 0.05), podemos concluir que estos tiempos entre llegadas sí distribuyen exponencialmente, pues no hay evidencia suficiente para rechazar la hipótesis nula.
