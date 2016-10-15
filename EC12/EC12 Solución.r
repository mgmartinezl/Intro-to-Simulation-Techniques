Ejercicio EC12

Utilizamos R para simular la cantidad de dinero que un usuario (no la compañía) tendrá que pagar por daños a su carro.

Creamos una función llamada deducibles que tomará como argumento la cantidad deducible que determine la compañía de seguros. Se aplicó inicialmente para $1000 y posteriormente para deducibles de $500 a $2000 en múltiplos de 500.

La fórmula funciona de la siguiente manera:

deducibles <- function(x) {
	#Generamos 5000 números aleatorios uniformemente distribuidos que nos servirán para evaluar las probabilidades de accidente de un carro.
	accs <- runif(5000) 
	#Evaluamos cada número aleatorio generado. Si es menor a la probabilidad de accidente, reemplazamos dicho número con un 1 para indicar la ocurrencia de un accidente. Si el número aleatorio es mayor a dicha probabilidad, asumimos que no ocurrió ningún accidente. Así, simulamos la generación de datos producto de una distribución Bernoulli.
	accs2 <- ifelse(accs<0.025, 1, ifelse(accs>0.025, 0, accs))
	#A continuación, generamos otros 5000 números aleatorios que distribuyen normalmente con media 3000 y desviación estándar 750, para simular el pago de los daños.
	cost <- rnorm(5000, mean=3000, sd=750)
	#Multiplicamos los vectores de ocurrencia de accidentes y costo de los daños para obtener los valores totales a pagar.
	daños0 <- accs2 * cost
	#Ahora calculamos la cantidad que tendrá que pagar el usuario de acuerdo con el deducible utilizado.
	daños <- ifelse(daños0<x, daños0, ifelse(daños0>x, x, daños0))
	#Imprimimos el cálculo del valor medio que tendrá que pagar un usuario por sus daños (excluyendo los valores iguales a cero para que no afecten el promedio).
	print(mean(subset(daños, daños != 0)))
	#Imprimimos el cálculo de la desviación estándar para el vector de pagos por daños (también excluimos los valores iguales a cero).
	print(sd(subset(daños, daños != 0)))
	#Calculamos el intervalo de confianza para la media de dicho vector, al 95%. No puede calcularse cuando la desviación estándar sea cero, es decir, en caso de que el usuario tenga un valor promedio de pago de $1000.
	t.test(subset(daños, daños != 0))
	#Finalmente, graficamos el histograma del vector que analizamos para verificar visualmente si los datos son normales. 
	hist(subset(daños, daños != 0), main="Histograma del vector de pagos por daños")	
}

deducibles(2000)

¿Es razonable asumir que los daños se distribuyen normalmente? ¿qué le criticaría a dicha suposición? ¿sugeriría otra cosa? 

Evidentemente la respuesta es no. Estos datos sobre los valores a pagar por los daños están lejos de distribuir normalmente, pues la probabilidad de que un usuario sufra un accidente es baja, de manera que cuando ocurre, la mayoría de los datos sobre pagos estarán inclinados hacia un extremo de la distribución (porque resulta de la multiplicación entre el vector de ocurrencias de accidentes y los valores deducibles).

En la vida real, la ocurrencia de accidentes probablemente distribuye Poisson y no Bernoulli. Los valores a pagar por dichos accidentes probablemente distribuyan gamma o weibull, porque se esperaría que muchísimos accidentes cuesten poco (debidos a pormenores en las vías de tránsito), mientras que unos pocos serán los más costosos (pues normalmente involucran consecuencias considerables).
