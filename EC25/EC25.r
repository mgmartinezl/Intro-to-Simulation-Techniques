parking <- read.table("datos_EC25.txt", header = TRUE) #Leemos los datos
arrival_parking <- parking$dt_min #Extraemos tiempos entre llegadas
hist(arrival_parking, main = "Tiempos entre llegadas al parqueadero") #Por el histograma, parece que estos tiempos distribuyen exponencialmente
library(MASS)
fit1 <- fitdistr(arrival_parking, "exponential")
ks.test(arrival_parking, "pexp", fit1$estimate)

#Por lo anterior (valor p > 0.05), podemos concluir que estos tiempos entre llegadas sí distribuyen exponencialmente, pues no hay evidencia suficiente para rechazar la hipótesis nula.

stay_times <- parking$ts_min #Extraemos tiempos de permanencia
hist(stay_times, main='tiempos de permanencia en el parqueadero') #Por el histograma, parece que estos tiempos distribuyen uniformemente
ks.test(stay_times, "punif", min(stay_times), max(stay_times))

#Lo anterior, nos confirma (pues el valor p es mayor a una significancia del 0.05) que los datos de permanencia en el parqueadero se ajustan a una distribución uniforme con mínimo 15.1047 y máximo 34.8989, pues no tenemos evidencia suficiente para rechazar la hipótesis nula.

#Definimos las variables a utilizar
p <- 0 #Número de celdas
serv <- 0 #El tiempo de servicio

#Vamos a generar celdas de parqueo y clientes mientras la atención sea inferior al 90%
nums <- rexp(10000, rate=0.27151919)
arrival_times <- cumsum(nums)
while (serv <= 0.9) {
  #Aumentamos el Número de celdas de parqueo en 1 y sabemos que no hay clientes que no pudieron ingresar
  p = p + 1
  w_entry <- 0
  cells <- rep(0, p)
  #Generemos llegadas aleatorias de autos e iteramos sobre ellas
  for (r in 1:length(arrival_times)) {
    t <- arrival_times[r] #Este es su tiempo de llegada
    d <- runif(1, min=15.1047, max=34.8989) # Esta es su duración en el parqueadero
    P_free_time <- min(cells) # en qué momento se desocupa la siguiente celda?
    if (P_free_time < t) { # está disponible en el momento que el cliente llega?
      space <- which(cells == P_free_time)[1] # la primera celda que está disponible
      cells[space] <- cells[space] + d # resetea el momento en que estará disponible
    } else {
      w_entry <- w_entry + 1 #clientes en cola que rechazaremos
    }   
  }
serv <- (length(arrival_times) - w_entry)/length(arrival_times) # % de clientes atendidos
cat(paste("Con", p, "celdas se rechazan", w_entry, "clientes, para un nivel de atenci�n de", percent(serv)), sep="\n")
}

print(p)
print(w_entry)
print(serv)

getwd()
setwd("E:/R")

parking <- read.table("datos_EC25.txt", header = TRUE) #Leemos los datos
arrival_parking <- parking$dt_min #Extraemos tiempos entre llegadas
hist(arrival_parking, main = "Tiempos entre llegadas al parqueadero") #Por el histograma, parece que estos tiempos distribuyen exponencialmente
library(MASS)
fit1 <- fitdistr(arrival_parking, "exponential")
ks.test(arrival_parking, "pexp", fit1$estimate)

stay_times <- parking$ts_min #Extraemos tiempos de permanencia
hist(stay_times, main='tiempos de permanencia en el parqueadero') #Por el histograma, parece que estos tiempos distribuyen uniformemente
ks.test(stay_times, "punif", min(stay_times), max(stay_times))
fitdistr(stay_times, "unif", method="mme")
min(stay_times)
max(stay_times)
