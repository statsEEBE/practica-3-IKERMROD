#Solucion Pregunta 1

#Ensayo de Bernoulli

x <- c(0, 1)
fx <- c(0.68, 0.32)

#tabla

cbind(x, fx)
plot(x, fx, ylim=c(0,1), type="h", 
     col="red")
points(x, fx, pch=16, col="red")

n <- 400000
muestra <- sample(x, n, fx, replace = TRUE)
fia <- table(muestra)/n #frecuencia absoluta
fi <- table(muestra)/n #frecuencia relativa
br <- barplot(fi, ylim = c(0,1)) #frecuencias relativas

lines(br, fx, ylim=c(0,1), type="h", 
     col="red")
points(br, fx, pch=16, col="red")

xbar <- mean(muestra) #promedio

mu <- sum(x*fx) #la media

fx[2]

#datos varianza muestral
ssq <- var(muestra)
ssq

#varianza de la funcion de masa de probabilidad
sigmasq <- sum((x-mu) ^2*fx)
sigmasq
fx[1]*fx[2] #en Bernoulli


n <- 43
set.seed(123)
muestra <- sample(x, n, fx, replace =TRUE)
muestra

y <- function(i){sum(sample(x, n, fx, replace = TRUE))}
y(4)

#bucle en R
set.seed(123)
m <- 400000 #encuestas de n=43
encuestas <- sapply(1:m, y)
fi <- table(encuestas)/m
data.frame(fi)

dbinom(13, 43, 0.32) #binomial

#tabla de probabilidad
resultados <- 1:43
fy <- dbinom(resultados, 43, 0.32)

tabladeprob <- cbind(resultados, fy)
tabladeprob

### n =44
resultados <- 1:44
fy <- dbinom(resultados, 44, 0.32)

tabladeprob <- cbind(resultados, fy)
plot(resultados,
     fy, type="h", col="red",
     ylim=c(0,0.2))

Fy <- cumsum(fy)
tabladeprob <- cbind(resultados,
                     fy, Fy)
tabladeprob

plot(Fy, type="s", col="red")

pbinom(17, 44, 0.32)


###
resultados <- 1:24
fy <- dbinom(resultados, 24, 0.68)
Fy <- cumsum(fy)
tabladeprob <- cbind(resultados,
                     fy, Fy)
tabladeprob
mu <- sum(resultados*fy) #media bernoulli, valor esperado
mu

sigmasq <- sum((resultados-mu)^2*fy) #la varianza
sigmasq



plot(Fy, type="s", col="red")

#Encontramos el q0.25 o primer quartil viendo en que sitio de la grafica de la tabla de prob, se ecnuentra Fy=0.25
#Puedes ver como en la tabla a partir de q numero, Fy supera el 0,24, pues ahí es


qbinom(0.25, 24, 0.68) #otra manera de sacar el Q0.25 o primer quartil
