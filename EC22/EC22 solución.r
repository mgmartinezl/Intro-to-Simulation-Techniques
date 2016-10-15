#Primero, cargamos los datos de los nacimientos de los bebes
datos <- read.table("babyboomt.txt")
#Con el siguiente histograma y la gráfica de la densidad de la distribución de los datos se verifica visualmente que los pesos de los bebes en general, no distribuyen normalmente.
hist(datos$V3, main="Histograma peso de los bebés")
plot(density(datos$V3), main="densidad peso de los bebés")
#Con la siguiente línea, obtenemos la función de distribución acumulada del peso de los bebés.
plot(ecdf(datos$V3), main="función de distribución acumulada peso de los bebés")
#Normalizamos los datos de los pesos para posteriormente visualizar gráfico QQ de normalidad. Se evidencia nuevamente que los pesos no distribuyen normalmente.
znorm <- (datos$V3-mean(datos$V3)/sd(datos$V3))
qqnorm(znorm)

#Vamos a evaluar la normalidad de los datos para los niños y las niñas
girls <- datos[which(datos$V2 == "1"), ]
boys <- datos[which(datos$V2 == "2"), ]
plot(density(girls$V3), main="densidad peso niños")
plot(density(boys$V3), main="densidad peso niñas")
hist(girls$V3, main="Histograma peso niñas")
hist(boys$V3, main="Histograma peso niños")
#Las conclusiones siguen siendo las mismas. Los datos de los pesos no distribuyen normalmente para ninguno de los sexos por separado. Sin embargo, las niñas presentan pesos menos concentrados alrededor de un valor, lo que sí ocurre marcadamente con los niños.

#Ahora, vamos a intentar adaptar algunas distribuciones a los datos de los pesos de los bebés. Por ser de naturaleza continua, ensayaremos las distribuciones lognormal, weibull y gamma.
library(MASS)

# Notar que la distribución weibull no pudo ser ajustada. Esto se debe a que la función fitdistr utiliza técnicas de máxima verosimilitud para encontrar los parámetros buscados, que fallan con algunas distribuciones específicas como la t y la weibull. Para solucionar este problema, deberíamos ayudarle al optimizador con un punto de partida y un límite para que encuentre los valores óptimos. Por ejemplo: fitdistr(x, "t", start = list(m=mean(x),s=sd(x), df=3), lower=c(-1, 0.001,1))

fitdistr(datos$V3, "weibull")
Error in fitdistr(datos$V3, "weibull") : optimization failed

#Para la distribución gamma obtenemos:
fitdistr(datos$V3, "gamma")
      shape           rate    
  33.344055996    0.010178427 
 ( 5.924452634) ( 0.001813293)

#Para la distribución lognormal obtenemos:
fitdistr(datos$V3, "lognormal")
    meanlog       sdlog   
  8.07929395   0.18180473 
 (0.02740810) (0.01938045)
 
#El objetivo ahora, es aplicar la prueba de bondad de ajuste de Kolmogorov-Smirnov para determinar si dichas distribuciones realmente se ajustan a los datos. Para el caso de la distribución gamma tenemos que los datos de los pesos NO distribuyen gamma, pues no hay evidencia suficiente que permita aceptar la hipótesis nula.
 
ks.test(datos$V3, "pgamma", shape=33.3440, rate=0.01017)
 
One-sample Kolmogorov-Smirnov test

data:  datos$V3
D = 0.20439, p-value = 0.05064
alternative hypothesis: two-sided

#Para la distribución lognormal tenemos el siguiente test que también rechazó la hipótesis nula.

ks.test(datos$V3, "plnorm", meanlog=8.07, sdlog=0,1818)

One-sample Kolmogorov-Smirnov test

data:  datos$V3
D = 0.70455, p-value < 2.2e-16
alternative hypothesis: two-sided

#Como conclusión, lo más adecuado es que los datos de estos pesos sean ajustados con una distribución empírica.