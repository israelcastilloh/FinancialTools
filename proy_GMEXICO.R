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


### ----------- GRAFICAS PRECIO, REND Y DISTRIBUCIONES
#bajar la info o leer los datos
sim<-"GMEXICOB.MX"
inicio<-"2016-01-01"
fin<-"2019-03-05"

getSymbols(sim,from=inicio,to=fin)
chartSeries(GMEXICOB.MX,theme="white")

#asignar el precio a una variable
precio<-GMEXICOB.MX$GMEXICOB.MX.Adjusted
hist(precio,main = "Histograma del precio",xlab = "Precio",ylab = "Densidad",freq = FALSE)

#dibujar una normal ajustada a los datos
curve(dnorm(x,mean(precio),sd(precio)),add = TRUE,col="red")
#curva de densidad del precio
lines(density(precio),col="black")

##estimar el rendimiento
activo.rtn<-na.omit(diff(log(precio)))
hist(activo.rtn,main = "Histograma del rendimiento",xlab = "Rendimiento",ylab = "Densidad",freq = FALSE,ylim=c(0,30))
#dibujar una normal ajustada a los datos
curve(dnorm(x,mean(activo.rtn),sd(activo.rtn)),add = TRUE,col="red")
#curva de densidad del precio
lines(density(activo.rtn),col="black")

## ----------------------PRUEBA DE NORMALIDAD
jb.norm.test(activo.rtn) #prueba de normalidad, mayor a alpha si es normal.


##------------------------ SIMULACION MONTECARLO
#bajar la info o leer los datos
sim<-"GMEXICOB.MX"
inicio<-"2018-01-01"
fin<-"2019-03-05"
getSymbols(sim,from=inicio,to=fin)
precio<-GMEXICOB.MX$GMEXICOB.MX.Adjusted
activo.rtn<-na.omit(diff(log(precio)))
# n serán los numeros aleatorios para la estimación
n<- 1000
epsilon<-rnorm(n)
plot(epsilon,type='l') #La l es para que te los una con una linea
hist(epsilon)
#Definir los parámetros para la simulación Montecarlo
mu <-  mean(activo.rtn)
#mu <- mu[1]
sigma <- sqrt(var(activo.rtn))
#sigma <- sigma[1]
so<- tail(precio, 1)
epsilon<-as.matrix((rnorm(n)))
st<-matrix(0,n,4)
## Considerar diferentes valores de dt
dt<-c(1,10,30,252)
for (i in 1:4){
  for(j in 1:n){
    st[j,i]<- so*exp((mu-.5*sigma^2)*dt[i]+sigma*sqrt(dt[i])*epsilon[j])
    
  }
}

#dim(st) obtener el shape de la matriz

so_estimado_columnas<-colMeans(st) #Promedio por columnas
Linf<- colMins(st)
Lins<- colMaxs(st)


## ----------- ESTIMACION DE BETAS

getSymbols("^MXX", from="2016-01-1", to="2019-03-05")
IPC<-MXX$MXX.Adjusted
ipc_rn<- na.omit(diff(log(IPC)))
rendmiento <- mean(ipc_rn)*252
TasaCete <- read_excel("Desktop/TasaCete.csv")
mean(TasaCete$Tasa)
lr <- TasaCete[,1]

chartSeries(MXX,theme="white")

##Declara una serie de tiempo
lr <- ts(lr, start= c(2016), frequency=12)

#Para el modelo CAPM necesitamos obtener
rend_mer <- na.omit(monthlyReturn(MXX$MXX.Adjusted)*100)

#Obtener los precios del activo a estudiar
getSymbols("GMEXICOB.MX", from="2016-01-1", to="2019-03-05")

#Para el modelo CAPM necesitamos obtener el rendimiento mensual
rend_mer<- na.omit(monthlyReturn(MXX$MXX.Adjusted)*100)
rend_mer <- ts(rend_mer, start= c(2016), frequency=12)
rend_act<- na.omit(monthlyReturn(GMEXICOB.MX$GMEXICOB.MX.Adjusted)*100)
rend_act <- ts(rend_act, start= c(2016), frequency=12)
mu_mer<-mean(rend_mer)
mu_act<-mean(rend_act)
ganm<- rend_mer-lr #beta (ganancia de mercado)
#Regresión lineal
reg <- lm(rend_act~ganm) #lm = lineal model 
summary(reg)



## ----------- EWMA
#Codigo para generar el valor de lamda del modelo EWMA de volatilidad 

library(quantmod)
#Obtener los datos
getSymbols("GMEXICOB.MX", from="2016-01-01", to="2019-03-27")
sum(is.na(GMEXICOB.MX))
plot.xts(GMEXICOB.MX$GMEXICOB.MX.Adjusted)

#Calcular el rendimiento
GMEXICOB.MX.rtn<-na.omit(diff(log(GMEXICOB.MX$GMEXICOB.MX.Adjusted)))
plot.xts(GMEXICOB.MX.rtn)
n<-nrow(GMEXICOB.MX.rtn)

#Volatilidad del periodo
VAP=sqrt(var(GMEXICOB.MX.rtn)*252)


#Calcular el rendimiento al cuadrado o varianzas diarias
VarDiaria<-matrix(0,n,1)
VolDiaria<-matrix(0,n,1)
VarDiaria<-GMEXICOB.MX.rtn*GMEXICOB.MX.rtn
VolDiaria<-sqrt(VarDiaria)
plot.xts(VolDiaria)

#Estimacion Varianza
VarEst<-matrix(0,n-1,1)
Func<-matrix(0,n-1,1)
FMaxAct<-0
FMaxAnt<-0
Loptimo<-0

VarEst[2,1]=VarDiaria[2,1]
a<-seq(0.1,1,by=.01)
for (L in a){
  for (i in 3:(n-1)) {
    VarEst[i,1]=(1-L)*VarDiaria[i-1,1]+L*VarEst[i-1,1]
    Func[i,1]=-log(VarEst[i,1])-VarDiaria[i,1]/VarEst[i,1]
  } 
  FMaxAct=sum(Func)
  if (FMaxAct>FMaxAnt){
    FMaxAnt=FMaxAct
    FMax=FMaxAnt
    Loptimo=L
  } else {
    FMax=FMaxAnt
  }
}

"El Lambda optimo es"


#Nuevas estimaciones de varianza
VarEst2<-VarDiaria
n=nrow(VarEst2)
for (i in 2:n ) {
  VarEst2[i,1]=(1-Loptimo)*VarDiaria[i-1,1]+Loptimo*VarEst2[i-1,1]
}

plot.xts(sqrt(VarEst2))



## ========================= GARCH
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

#Bajar datos históricos
sim<-"GMEXICOB.MX"
getSymbols(sim,from='2016-01-01',to='2019-03-05')
chart_Series(GMEXICOB.MX) # Grafica del precio
precio<-GMEXICOB.MX$GMEXICOB.MX.Adjusted

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
acf(GMEXICOB.MX$GMEXICOB.MX.Adjusted)
pacf(GMEXICOB.MX$GMEXICOB.MX.Adjusted)

par(mfrow=c(2,1))
acf(activo.ret, main="FAC")
pacf(activo.ret, main="FAC PARCIAL")

# Probamos varios modelos con hasta 3 rezagos
arima111 <- arima(activo.ret,order = c(1,1,1))
arima011 <- arima(activo.ret,order = c(0,1,1))
arima010 <- arima(activo.ret,order = c(0,1,0))
arima001 <- arima(activo.ret,order = c(0,0,1))
arima100 <- arima(activo.ret,order = c(1,0,0))


## de los modelos construidos de selecciona aquiel que tenga el menos valor 
# del parametro akaike y chuartz
aic111 <- arima111$aic
aic011 <- arima011$aic
aic010 <- arima010$aic
aic001 <- arima001$aic
aic100 <- arima100$aic


nombres <- c("aic111","aic011","aic010","aic001","aic100")
aic <- as.numeric(c(aic111,aic011,aic010,aic001,aic100))

tabla <- data.frame(nombres,aic)
min(aic)

## 4) Ajustar Varianza
Res<- arima011$residuals
Res2 <- arima011$residuals^2
par(mfrow=c(3,1))
plot(Res2,main = "residuos del modelo ARIMA")
acf(Res2,main = "ACF , Q")
pacf(Res2,main = "FACP , P")

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
for (i in 1:500){
  vol=garch11$fit[i]
  esperanza[i]=((alpha+beta)^i)*((vol^2)-vl)+vl
}
plot(esperanza)
eesp<-esperanza[500]
eesp
vl



