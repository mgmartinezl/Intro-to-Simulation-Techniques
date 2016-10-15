Demanda <- c(100, 150, 200, 250, 300)
Probabilidad <- c(0.3, 0.2, 0.3, 0.15, 0.05)
ProbAcum <- c (0.3, 0.5, 0.8, 0.95, 1)
Almanaques <- data.frame(Demanda, Probabilidad)
hist(Almanaques$Probabilidad)
Almanaques <- cbind(Almanaques, ProbAcum)


Comp <- 50

x <- c()
r <- runif(5000)

for (i in seq(1:5)) {
  UFinal <- 0
  Comp = Comp + 50
    for (i in r) {
      count <- 1
      while (i>ProbAcum[count])
      count = count + 1
      end
      Dem <- Demanda[count]
      if (Dem > Comp) {
      U <- Comp * 2.5
      CO <- (Dem - Comp) * 2.5 
      } else {
      U <- Dem * 2.5
      CO <- (Comp - Dem) * 5 
      }
      UFinal = UFinal + (U - CO)
    }
UP <- UFinal/5000
x <- append(x, UP)
}

print(x)
