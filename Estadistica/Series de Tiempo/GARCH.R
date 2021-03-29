##################################################
### GARCH
### Precio del petroleo
##################################################

### Paquetes
library(imputeTS)
#library(rugarch)
library(fGarch)
library(nortest)

### cambiar direccion para leer los datos
### setwd("AQUI")
setwd("/Users/EstadisticaCiencias/Dropbox/_FCiencias/NotasCiencias/SupervivenciaSeries/EjemplosSeriesTiempo/")

### leer datos
datos <- read.csv("PrecioDiario2.csv", header=T,dec=",",sep=";")
head(datos)

precio <- as.numeric(as.vector(Precio)) 
summary(precio)   ### tiene datos faltantes

### imputar los valores faltantes con el metodo de ma
precio <- na.ma(precio, k = 4, weighting = "exponential")

plot(precio,col="blue",type="l")
### los datos tienen tendencia creciente
### las datos presentan una variabilidad creciente

### la variabilidad se estabiliza con la transformacion logaritmo 
plot(log(precio),col="blue",type="l")

### lo que mas interesa es modelar los rendimientos
### rendi = log(precio[t]) - log(precio[t-1])
rendi <- diff(log(precio))
plot(rendi, col="red",type="l")

### este patron es caracteristico de los GARCH
### tambien para identificar a los GARCH es conveniente revisar ACF y PACF

par(mfrow=c(2,1))
acf(rendi) 
pacf(rendi) 
### parecen estar No correlacionados, porque los valores del acf y pacf quedan dentro de las bandas de confianza
### pero de acuerdo a los datos SI existe relacion de dependencia!

### revisar ACF y PACF de los cuadrados y valores absoluto
acf(rendi^2) 
pacf(rendi^2) 
acf(abs(rendi)) 
pacf(abs(rendi)) 
### si existe relacion de dependencia, ya que muchos valores del acf y pacf quedan fueran de las bandas de confianza

### OJO: si tuvieramos un ruido blanco, esto NO sucederia!
z <- rnorm(100)
### al ser No correlacionados, de hecho INDEPENDIENTES, los valores del acf y pacf quedan dentro de las bandas de confianza
acf(z) 
pacf(z) 
### y lo mismo sucede con los cuadrados y valores absoluto
acf(z^2) 
pacf(z^2) 
acf(abs(z)) 
pacf(abs(z)) 
### OJO; Si X y Y son v.a. independientes. Entonces X^2 y Y^2 son independientes, y tambien abs(X) y abs(Y) son independientes

### usando pruebas NO parametricas
help(Box.test) ### "Compute the Box–Pierce or Ljung–Box test statistic for examining the null hypothesis of independence in a given time series." 
### p-value > nivel de significancia alpha == Aceptar H0
### p-value < nivel de significancia alpha == Rechazar H0
Box.test(rendi,1)
for(k in 1:5){print(Box.test(rendi,k))}
Box.test(rendi^2)
Box.test(abs(rendi))

### estimador modelos GARCH(p,q)
help(garchFit)
### usualmente se eligen valores de p y q igual a 1 y 2
mod11 <- garchFit(formula = ~garch(1,1), data= rendi)
mod12 <- garchFit(formula = ~garch(1,2), data= rendi)
mod21 <- garchFit(formula = ~garch(2,1), data= rendi)
mod22 <- garchFit(formula = ~garch(2,2), data= rendi)
### usando metodos de bondad de ajuste, analisis de residuales, significancia de los parametros, entonces elegir el mejor modelo
summary(mod11)
summary(mod12)
summary(mod21)
summary(mod22)

### hacer el analisis con cada modelo estimado
mod <- mod11 
mod <- mod22 
summary(mod)
plot(mod)

### analisis de residuales
### comprobar ruido blanco gaussiano
summary(mod)
res <- residuals(mod,standardize=F)
plot(res)
res <- residuals(mod,standardize=T)
plot(res)
### Normalidad
qqnorm(res) 
qqline(res,col="red",lwd=2)
ad.test(res) 
### No correlacion
par(mfrow=c(2,1))
acf(res) 
pacf(res)
acf(res^2)
pacf(res^2)
acf(abs(res))
pacf(abs(res))

### en los modelos GARCH es usuar buscar otro tipo de distribucion para los errores, ya queden ser No simetricos, o con colas mas pesadas
mod <- garchFit(formula = ~garch(1,1), data=rendi, cond.dist="snorm")
mod <- garchFit(formula = ~garch(1,1), data=rendi, cond.dist="ged")
mod <- garchFit(formula = ~garch(1,1), data=rendi, cond.dist="sged")
mod <- garchFit(formula = ~garch(1,1), data=rendi, cond.dist="std")
mod <- garchFit(formula = ~garch(1,1), data=rendi, cond.dist="sstd")

### predicciones
predict(mod, n.ahead = 5, trace = FALSE, mse = c("cond"),
        plot=TRUE, nx=10, crit_val=NULL, conf=NULL)
##################################################
