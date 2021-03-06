#Ejercicio EC23

#Lo primero que hacemos es leer los datos
read.table("datos_EC23.txt")

#Las llamadas serán almacenadas así:
calls <- read.table("datos_EC23.txt", header = TRUE)

#Observamos el histograma del tiempo entre llegadas, que a simple vista parece distribuir exponencialmente.
hist(test, main='Histograma tiempo entre llegadas')

#Ajustamos una distribución exponencial a estos datos, para luego aplicar test de bondad de ajuste.
library(MASS)
fit1 <- fitdistr(test, "exponential")
ks.test(test, "pexp", fit1$estimate)

#Los resultados obtenidos son:
One-sample Kolmogorov-Smirnov test

data:  test
D = 0.043631, p-value = 0.8388
alternative hypothesis: two-sided

#De lo anterior, podemos concluir que no hay evidencia suficiente para rechazar la hipótesis nula, por lo tanto, el tiempo entre llegadas distribuye exponencialmente.

#Ahora, vamos a calcular la media del tiempo entre llegadas (que por definición, es 1/lambda). Primero encontramos lambda.
 fitdistr(test, "exponential")
 rate   
  0.45360276 
 (0.03199467)
 
 #De esta manera, se tiene que el tiempo promedio entre eventos tiene una media de 2.2 (es decir, cada 2.2 minutos, llega una llamada)
#Este es un sistema M/M/s/k, es decir, un problema de teoría de colas con s=2 servidores y un tamaño de cola k, que en este caso puede considerarse infinito.

#Ahora, debemos evaluar qué distribución siguen los datos de tiempos de servicio de cada servidor y calcular sus respectivos parámetros. En general, se trata de tiempos de servicio que distribuyen normalmente (se trata de una normal truncada, que no toma valores negativos)
s1 <- calls$TA
hist(s1, main = 'histograma tiempos de servicio servidor A')
s2 <- calls$TB
hist(s2, main = 'histograma tiempos de servicio servidor B')
qqnorm(s1)
qqnorm(s2)

#Sus parámetros son:
#Para el servidor A:
fitdistr(s1, "normal")
      mean          sd    
  3.28464279   0.50983365 
 (0.03596089) (0.02542819)
 
#Para el servidor B:
 fitdistr(s2, "normal")
      mean          sd    
  4.23904378   0.52395200 
 (0.03695672) (0.02613235)

#A continuación simularemos el comportamiento del sistema para 100 llamadas, en términos de su tiempo total en el sistema, tiempo de espera de cada una y la ocupación de los servidores.

#Generamos 100 números aleatorios que distribuyen exponencialmente
nums <- rexp(100, rate = 0.4536028)
#Las llegadas serán la suma acumulada de dichos aleatorios
arrival_times <- cumsum(nums)
#Creamos un vector w donde almacenaremos los tiempos de espera
w = numeric(0)
#Aquí almacenaremos las duraciones de las llamadas según las atienda A o B
call_durations = numeric(0)
#Almacenamos el tiempo total de las llamadas en el sistema
total_time = numeric(0)
#Aquí guardaremos el próximo tiempo en el que el servidor A se desocupará
A_free_time <- 0
#Aquí guardaremos el próximo tiempo en el que el servidor B se desocupará
B_free_time <- 0
#Este será el tiempo de ocupación de A
occup_A <- 0
#Este será el tiempo de ocupación de B
occup_B <- 0

#Ahora iteramos sobre los números aleatorios exponenciales, que serán las llamadas.
for (r in 1:length(arrival_times)) {
#El tiempo de llegada será cada número aleatorio acumulado en arrival_times
  t <- arrival_times[r]
  #Comparamos si A se desocupará primero que B, pues de ser así, enviamos la llamada actual al servidor A.
  if (A_free_time <= B_free_time) {
  #Generamos a continuación la duración de esa llamada, que distribuye normalmente y tiene los parámetros hallados anteriormente.
    d <- rnorm(1, mean = 3.28464279, sd = 0.50983365)
    call_durations[r] = d
	#Guardamos el tiempo de espera de esta llamada, que será el máximo entre el tiempo en el que se desocupa A y la llegada de la llamada actual y cero.
    w[r] <- max(A_free_time - t, 0)
	#Guardamos el inicio del servicio
    start <- t + w[r]
	#Guardamos el próximo tiempo en el que A estará libre.
    A_free_time <- start + d
	#El tiempo total de la llamada en el sistema, será el tiempo de espera más la duración de la atención.
    total_time[r] <- w[r] + d
	#Finalmente, calculamos la ocupación de A para esta llamada.
    occup_A = occup_A + d
  } else {
  #Si A no se desocupa primero que B, enviamos la llamada a B. La lógica para este servidor se repite como en el caso de A.
    d <- rnorm(1, mean = 4.23904378, sd = 0.52395200)
    call_durations[r] = d
    w[r] <- max(B_free_time - t, 0)
    start <- t + w[r]
    B_free_time <- start + d
    total_time[r] <- w[r] + d
    occup_B = occup_B + d
  }
}
#FInalmente, calculamos el tiempo total del sistema y los porcentajes de ocupación de A y B.
total_time_system <- max(A_free_time, B_free_time)
occup_A_percentage <- occup_A/total_time_system
occup_B_percentage <- occup_B/total_time_system

#Los resultados obtenidos nos muestran que A estuvo ocupado el 76.37% del tiempo mientras que B lo estuvo el 79.01% (racional, porque es más lento)
 total_time_system
[1] 242.1193
occup_A_percentage
[1] 0.7637505
> occup_B_percentage
[1] 0.7901703

#Adicionalmente, se tiene que los tiempos de espera fueron racionales de acuerdo a las medias de duración de los servicios.
w
  [1] 0.000000000 0.000000000 0.000000000 2.353445716 0.000000000 0.000000000
  [7] 1.194553624 0.000000000 0.000000000 0.000000000 0.000000000 0.007927116
 [13] 0.061148598 2.720069259 4.095055139 0.489900066 2.266686401 0.326741379
 [19] 2.144886301 1.655704046 0.000000000 0.000000000 0.000000000 2.168131266
 [25] 3.185221246 3.989227377 0.530156304 1.922003005 1.436656352 1.917508783
 [31] 4.462775695 0.000000000 0.000000000 0.000000000 0.000000000 1.395026557
 [37] 0.675192306 3.935935113 4.422374957 7.629123736 8.191698967 6.481147547
 [43] 6.529557658 4.578456134 1.769834632 0.000000000 0.024283242 0.000000000
 [49] 0.000000000 0.000000000 0.096510660 2.885153240 4.323066748 1.799242371
 [55] 3.892418196 1.190716802 1.109938375 2.072685796 0.000000000 2.876401824
 [61] 3.438935021 6.381637224 6.255466229 9.372329070 7.549783459 4.775529195
 [67] 2.943405335 1.857835772 1.202193112 0.000000000 0.000000000 0.000000000
 [73] 0.493278220 0.000000000 0.000000000 0.000000000 0.000000000 2.058931954
 [79] 0.000000000 0.000000000 0.000000000 0.000000000 0.000000000 1.914132050
 [85] 2.707829454 4.644182563 0.000000000 0.000000000 2.833503021 2.838668156
 [91] 3.452979107 2.342742236 4.722423409 4.253009565 2.575214308 2.652941411
 [97] 0.491653498 2.061390057 1.415133791 3.778366630

