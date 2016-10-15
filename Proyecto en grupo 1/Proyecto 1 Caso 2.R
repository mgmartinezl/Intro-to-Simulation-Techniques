# Creamos la tasa de descuento          
rate <- 0.004074

# Creamos el tiempo de recuperación de la inversión inicial
Time <- 16.82

# Creamos el monto de inversión inicial
AC <- 370

# Creamos un dataframe de 1000 filas y 360 columnas (clientes por meses) llena de números aleatorios uniformemente distribuidos
A <- data.frame(replicate(360, sample(runif(1000), 1000, rep=TRUE)))

# La primera columna siempre será 1 porque el primer mes incluye a todos los clientes 
A[, 1] <- 1

# Ahora generamos para los demás meses, los abandonos (representados con 0), de acuerdo a los porcentajes de abandono considerados
A[, 2:3] <- ifelse(A[, 2:3]>=0.01,1,0)
A[, 4:15] <- ifelse(A[, 4:15]>=0.06,1,0)
A[, 16:ncol(A)] <- ifelse(A[, 16:ncol(A)]>=0.3,1,0)
A[1:10, 1:20]


# Eliminamos los usuarios que abandonaron, de los ingresos de Virgin
for (i in 2:ncol(A)) {
  A[, i] <- A[, i] * A[, i-1]
}

# Obtenemos los márgenes mensuales de cada cliente
B <- A * 22
B[1:10, 1:20]

# Obtenemos el vector de LVP, multiplicando el dataframe anterior por el vector de descuentos
Desc <- 1/(1+rate)^(1:360)
LTV <- as.matrix(B) %*% Desc

# Confirmamos que los LVP no distribuyen exponencialmente, como dice la teoría
hist(LTV,main="Histograma de los LTV de los clientes")
ks.test(LTV, "pnorm")
qqnorm(LTV)

# Obtenemos el histograma de los tiempos de recuperación asociados a clientes que se fueron antes de los 16.82 meses
t <- rowSums(A)
sum(t > Time)  # cuántos duraron más de el tiempo de recuperación
Recup <- ifelse(t - Time > 0, 0, t - Time)
hist(Recup, main="Histograma de los LTV de los clientes") #Aprox 80% de los clientes, no se recuperan

Ganancias <- LTV - AC
hist(Ganancias, main="Histograma de las ganancias obtenidas")
abline(v=0, col="red")
sum(Ganancias) # Este es el valor esperado de las ganancias de Virgin en 30 años (en valor presente)

