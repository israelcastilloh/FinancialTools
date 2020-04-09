#Codigo para estimar un modelo GARCH a una serie de datos
ArchTest <- function (x, lags=20, demean = FALSE) 
{
  # Capture name of x for documentation in the output  
  xName <- deparse(substitute(x))
  # 
  x <- as.vector(x)
  if(demean) x <- scale(x, center = TRUE, scale = FALSE)
  #  
  lags <- lags + 1
  mat <- embed(x^2, lags)
  arch.lm <- summary(lm(mat[, 1] ~ mat[, -1]))
  STATISTIC <- arch.lm$r.squared * length(resid(arch.lm))
  names(STATISTIC) <- "Chi-squared"
  PARAMETER <- lags - 1
  names(PARAMETER) <- "df"
  PVAL <- 1 - pchisq(STATISTIC, df = PARAMETER)
  METHOD <- "ARCH LM-test;  Null hypothesis:  no ARCH effects"
  result <- list(statistic = STATISTIC, parameter = PARAMETER, 
                 p.value = PVAL, method = METHOD, data.name =
                   xName)
  class(result) <- "htest"
  return(result)
}

#Instalar Paqueterias
library(fBasics)
library(normtest)
library(quantmod)
library(PerformanceAnalytics)
library(readxl)
library(TTR)
library(fGarch)
library(car)
library(aTSA)
library(stats)
library(forecast)
library(TSA)
library(xtable)
library(tseries)

#Bajar datos históricos
sim<-"CEMEXCPO.MX"
inicio<- "2011-06-01"
fin <- "2019-04-01"
getSymbols(sim,from=inicio,to=fin)
chart_Series(CEMEXCPO.MX) # Grafica del precio
precio<-CEMEXCPO.MX$CEMEXCPO.MX.Adjusted

activo.ret<-na.omit(diff(log(precio)))
chart_Series(activo.ret) #Grafica de los rendimientos 
# Rendimiento promedio diario
ret_diario <- mean(activo.ret)
# Volatilidad Promedio Diaria
vol_diaria <- sqrt(var(activo.ret))

#PASO 1) Checar si existe independencia en los rendimientos 
#usaremos la prueba para la independencia donde 
#Ho: no existe independencia en los resagos del rendimiento 
Box.test(activo.ret, type="Ljung", lag=20)
#La prueba se distribuye como una j.cuadrada con el numero de rezago con 20 grado de libertad
qchisq(.95,20)
#Si el calculado es mayor que el resultado de chi la rechazamos y si es menor que .05 con el pvalue

#como hemos probado al 95% con 20 rezagos continuamos
#PASO 2) Probar si hay efectos ARCH/GARCH (hay relación entre varianzas en distintos puntos del tiempo)
ArchTest(activo.ret)
#rechazamos Ho porque el calculado es mayor al estadistico de tablas
# Si hay efectos GARCH

##PAS 3)Ajustar modelo para la media del rendimiento
par(mfrow=c(2,1))
acf(CEMEXCPO.MX$CEMEXCPO.MX.Adjusted)
pacf(CEMEXCPO.MX$CEMEXCPO.MX.Adjusted)

par(mfrow=c(2,1))
acf(activo.ret, main="FAC")
pacf(activo.ret, main="FAC PARCIAL")

#Probamos varios modelos con hasta 3 rezagos 
arima313<-arima(activo.ret, order=c(3,1,3))
arima212<-arima(activo.ret, order=c(2,1,2))
arima111<-arima(activo.ret, order=c(1,1,1))
arima013<-arima(activo.ret, order=c(0,1,3))
arima011<-arima(activo.ret, order=c(0,1,1))
arima012<-arima(activo.ret, order=c(0,1,2))
arima113<-arima(activo.ret, order=c(1,1,3))

#de los modelos construidos se selecciona aquel que tenga el menor valor del parámetro de Akaike y chuartz 
aic313<- arima313$aic
aic212<- arima212$aic
aic111<- arima111$aic
aic013<- arima013$aic
aic011<- arima011$aic
aic012<- arima012$aic
aic113<- arima113$aic

nombres<-c("aic313","aic212", "aic111", "aic013", "aic011", "aic012", 'aic113')
aic<-as.numeric(c(aic313,aic212, aic111, aic013, aic011, aic012, aic113))
tabla<-data.frame(nombres, aic)
min(aic)

#PASO 4) ajustar la varianza 
Res<- arima313$residuals
Res2<-arima313$residuals^2

par(mfrow=c(3,1))
plot(Res2, main="Residuos del modelo arima")
acf(Res2, main="ACF, Q")
pacf(Res2, main="FACP, P")


# consideremos un modelo p=1, q=1

garch11<-garch(Res, order=c(1,1))
summary(garch11)

#w=ao, alpha=a1, betha=b1
#Box- Ljung test mide la autocreelación de los residuos 
#Ho los errores se distribuyen de una manera independiente 
#No son normales pero si hay independencia
#Graficar el modelo seleccionado

par(mfcol=c(1,1))
ht.garch11=garch11$fit[,1]^2
estimaciones=fitted.values(garch11)
inf_garch11<-estimaciones-1.96*sqrt(ht.garch11)
sup_garch11<-estimaciones+1.96*sqrt(ht.garch11)
ret2<-as.ts(activo.ret)
plot(ret2, main="Grafica de los rendimientos de la serie con intervalos de confianza del
     modelo GARCH", type="l")
lines(inf_garch11[,2], col='red')
lines(inf_garch11[,1], col='red')
lines(sup_garch11[,2], col='blue')
lines(sup_garch11[,1], col='blue')
lines(estimaciones[,1], col='green')
lines(estimaciones[,2], col='green')
plot(inf_garch11[,2], col='red')
plot(sup_garch11)
plot(estimaciones) #estimaciones de varianza 

#------------demostrar las estimaciones tienden a la varianza a largo plazo
w<-garch11[['coef']][['a0']]
alpha<-garch11[['coef']][['a1']]
beta<-garch11[['coef']][['b1']]
vl<-w/(1-alpha-beta)
esperanza<-c()
for (i in 1:1000){
  vol=garch11$fit[i]
  esperanza[i]=((alpha+beta)^i)*((vol^2)-vl)+vl
}
plot(esperanza)
esperanza[1000]
vl
