#Codigo para estimar el VaR por métodos no paramétricos
rm(list=ls())
library(quantmod)

#Obtener datos del activo 1
getSymbols("GMEXICOB.MX", from="2016-01-01", to="2019-03-05")
sum(is.na(GMEXICOB.MX))
plot.xts(GMEXICOB.MX$GMEXICOB.MX.Adjusted)
na<-nrow(GMEXICOB.MX)

#Obtener datos del activo 2
getSymbols("GRUMAB.MX", from="2016-01-01", to="2019-03-05")
sum(is.na(GRUMAB.MX))
plot.xts(GRUMAB.MX$GRUMAB.MX.Adjusted)
nb<-nrow(GRUMAB.MX)

#Obtener datos del activo 3
getSymbols("ICHB.MX", from="2016-01-01", to="2019-03-05")
sum(is.na(ICHB.MX))
plot.xts(ICHB.MX$ICHB.MX.Adjusted)
nc<-nrow(ICHB.MX)

#Obtener datos del activo 4
getSymbols("IENOVA.MX", from="2016-01-01", to="2019-03-05")
sum(is.na(IENOVA.MX))
plot.xts(IENOVA.MX$IENOVA.MX.Adjusted)
nd<-nrow(IENOVA.MX)

#Obtener datos del activo 5
getSymbols("TLEVISACPO.MX", from="2016-01-01", to="2019-03-05")
sum(is.na(TLEVISACPO.MX))
plot.xts(TLEVISACPO.MX$TLEVISACPO.MX.Adjusted)
ne<-nrow(TLEVISACPO.MX)

#Calcular el rendimiento
GMEXICO.rtn<-na.omit(diff(log(GMEXICOB.MX$GMEXICOB.MX.Adjusted)))
plot.xts(GMEXICO.rtn)
RA_GMEXICO= as.numeric(mean(GMEXICO.rtn)*252)
VOLA_GMEXICO=as.numeric(sqrt(var(GMEXICO.rtn)*252))

GRUMAB.rtn<-na.omit(diff(log(GRUMAB.MX$GRUMAB.MX.Adjusted)))
plot.xts(GRUMAB.rtn)
RA_GRUMA= as.numeric(mean(GRUMAB.rtn)*252)
VOLA_GRUMA=as.numeric(sqrt(var(GRUMAB.rtn)*252))

ICHB.rtn<-na.omit(diff(log(ICHB.MX$ICHB.MX.Adjusted)))
plot.xts(ICHB.rtn)
RA_ICHB= as.numeric(mean(ICHB.rtn)*252)
VOLA_ICHB=as.numeric(sqrt(var(ICHB.rtn)*252))

IENOVA.rtn<-na.omit(diff(log(IENOVA.MX$IENOVA.MX.Adjusted)))
plot.xts(IENOVA.rtn)
RA_IENOVA= as.numeric(mean(IENOVA.rtn)*252)
VOLA_IENOVA=as.numeric(sqrt(var(IENOVA.rtn)*252))

TLEVISACPO.rtn<-na.omit(diff(log(TLEVISACPO.MX$TLEVISACPO.MX.Adjusted)))
plot.xts(TLEVISACPO.rtn)
RA_TLEVISA= as.numeric(mean(TLEVISACPO.rtn)*252)
VOLA_TLEVISA=as.numeric(sqrt(var(TLEVISACPO.rtn)*252))

#Generar un valor fictio del portafolio
A_GMEXICO = 100 #cantidad de acciones
A_GRUMAB = 100 #Cantidad de acciones 
A_ICHB = 100
A_IENOVA = 100
A_TLEVISA = 100


#Definir precios inciales del activo 
PI_GMEXICO<-as.numeric(GMEXICOB.MX$GMEXICOB.MX.Adjusted[na])
PI_GRUMAB<-as.numeric(GRUMAB.MX$GRUMAB.MX.Adjusted[nb])
PI_ICHB<-as.numeric(ICHB.MX$ICHB.MX.Adjusted[nc])
PI_IENOVA<-as.numeric(IENOVA.MX$IENOVA.MX.Adjusted[nd])
PI_TLEVISA<-as.numeric(TLEVISACPO.MX$TLEVISACPO.MX.Adjusted[ne])


#Valor inicial del portafolio
VI<-A_GMEXICO*PI_GMEXICO + A_GRUMAB*PI_GRUMAB + A_ICHB*PI_ICHB + A_IENOVA*PI_IENOVA + A_TLEVISA*PI_TLEVISA

#Para el VaR se deben estimar precios futuros 

#Met. 1: Estimar mediante cambios en precios 
#El precio estimado sera igual al precio inicial por uno de los primeros renidmientos observados 
PE_GMEXICO<-PI_GMEXICO*GMEXICO.rtn+PI_GMEXICO
PE_GRUMAB<-PI_GRUMAB*GRUMAB.rtn+PI_GRUMAB
PE_ICHB<-PI_ICHB*ICHB.rtn+PI_ICHB
PE_IENOVA<-PI_IENOVA*IENOVA.rtn+PI_IENOVA
PE_TLEVISA<-PI_TLEVISA*TLEVISACPO.rtn+PI_TLEVISA

#Esrimar valor de portafolio con precios estimados
VP_EST <- A_GMEXICO*PE_GMEXICO+A_GRUMAB*PE_GRUMAB+A_ICHB*PE_ICHB+A_IENOVA*PE_IENOVA+A_TLEVISA*PE_TLEVISA
plot.xts(VP_EST)
hist(VP_EST, nclass=30, main="Valor estimado de portafolio")

#Establecer una funcion de perdidas y ganancias 
#Esta funcion sera igual al valor estimado del portafolio menos el valor inicial
FPG<-VP_EST-VI
plot.xts(FPG)
hist(FPG, nclass=30, main="Perdidas y ganancias")

#Seleccionar el VaR 
#El VaR sera el valor numerico que se ubique en el percentil deseado 
var99=quantile(FPG, .01)*sqrt(1)#al 99% de confianza, esperariamos que por lo menos el 1% de las veces las perdidas hayan sido mayores al valor obtenido
var95=quantile(FPG, .05)*sqrt(1)
var90=quantile(FPG, .1)*sqrt(1)

print(var99)
print(var95)
print(var90)
#Var  diario si quisieras el var para mas de un dia lo multiplicas por la raiz del numero que quieres

#BackTesting para identificar los dias en que la perdida excedio el valor del var
#nos permite hacer una revision de tareas hacia atras 
#Usemos 99% 

#Estimamos el valor real del portafolio
VP_REAL <- (A_GMEXICO*GMEXICOB.MX$GMEXICOB.MX.Adjusted+A_GRUMAB*GRUMAB.MX$GRUMAB.MX.Adjusted+A_IENOVA*IENOVA.MX$IENOVA.MX.Adjusted+A_ICHB*ICHB.MX$ICHB.MX.Adjusted+A_TLEVISA*TLEVISACPO.MX$TLEVISACPO.MX.Adjusted)
plot.xts(VP_REAL)

#Identificamos la ganancia o perdida  real observada 
PER_OBS<-na.omit(diff(VP_REAL$GMEXICOB.MX.Adjusted))#QUEDA GUARDADO EL NOMBRE DEL PRIMER ACTIVO, PERO EN REALIDA TIENE EL VALOR COMPLETO






#Vemos que dias la perdida excedió el var estimado 
BTEST99<- ifelse(PER_OBS<var99,1,0)

#proporcionalizamos los dias que excedio la perdida respecto a los dias totales que tenemos la informacion
EV99<-(sum(BTEST99)/na)
print(EV99)

BTEST95<- ifelse(PER_OBS<var95,1,0)
EV95<-(sum(BTEST95)/na)
print(EV95)


BTEST90<- ifelse(PER_OBS<var90,1,0)
EV90<-(sum(BTEST90)/na)
print(EV90)


#Si cambiamos la metodologia de estimacion del precio 
#Usemos ahora simulacion de montecarlo con la ecuacion de precios 
#Generemos numeros aleatorios

e = rnorm(1000,0,1)

#Definamos valor inicial
#Ya los tenemos en Pi_Cemex y PI_AC
#Vol Anual VOLA_AC y VOLA_CEMEX
#Rendimiento anual RA_CEMEX RA_AC

dt=1/252 # para estimar un dia. El dia siguiente al valor inicial 

#Estimamos precios por montecarlo

SEST_GRUMA<-PI_GRUMAB*exp((RA_GRUMA-.5*VOLA_GRUMA^2)*dt+VOLA_GRUMA*e*sqrt(dt))

SEST_GMEXICO<-PI_GMEXICO*exp((RA_GMEXICO-.5*VOLA_GMEXICO^2)*dt+VOLA_GMEXICO*e*sqrt(dt))

SEST_ICHB<-PI_ICHB*exp((RA_ICHB-.5*VOLA_ICHB^2)*dt+VOLA_ICHB*e*sqrt(dt))

SEST_IENOVA<-PI_IENOVA*exp((RA_IENOVA-.5*VOLA_IENOVA^2)*dt+VOLA_IENOVA*e*sqrt(dt))

SEST_TLEVISA<-PI_TLEVISA*exp((RA_TLEVISA-.5*VOLA_TLEVISA^2)*dt+VOLA_TLEVISA*e*sqrt(dt))


#Estimar valor del portafolio con nuevos precios 
VP_EST2<-A_GMEXICO*SEST_GMEXICO+A_GRUMAB*SEST_GRUMA+A_ICHB*SEST_ICHB+A_IENOVA*SEST_IENOVA+A_TLEVISA*SEST_TLEVISA
FPG2<-VP_EST2-VI
var99_2=quantile(FPG2, .01)
var95_2=quantile(FPG2, .05)
var90_2=quantile(FPG2, .1)

print(var99_2)
print(var95_2)
print(var90_2)

#Nuevamente usamos bactesting 
BTEST90_2<- ifelse(PER_OBS<var90_2,1,0)
EV90_2<-(sum(BTEST90_2)/na)
print(EV90_2)

BTEST95_2<- ifelse(PER_OBS<var95_2,1,0)
EV95_2<-(sum(BTEST95_2)/na)
print(EV95_2)

BTEST99_2<- ifelse(PER_OBS<var99_2,1,0)
EV99_2<-(sum(BTEST99_2)/na)
print(EV99_2)

##Metodologia parametrica 

#Sacamos la correlacion de los activos______ NO SALE!!!! 
matriz<-matrix(0,796,5)
matriz[,1]<-GMEXICO.rtn
matriz[,2]<-GRUMAB.rtn
matriz[,3]<-IENOVA.rtn
matriz[,4]<-ICHB.rtn
matriz[,5]<-TLEVISACPO.rtn

covarianza= cov(matriz)
#Estimacion diaria de volatilidad
VD_GMEXICO<-VOLA_GMEXICO/sqrt(252)
VD_GRUMAB<-VOLA_GRUMA/sqrt(252)
VD_ICHB<-VOLA_ICHB/sqrt(252)
VD_IENOVA<-VOLA_IENOVA/sqrt(252)
VD_TLEVISA<-VOLA_TLEVISA/sqrt(252)

#PESOS DEL PORTAFOLIO
wGMEXICO<-(A_GMEXICO*PI_GMEXICO)/VI
wGRUMA<-(A_GRUMAB*PI_GRUMAB)/VI
wICHB<-(A_ICHB*PI_ICHB)/VI
wIENOVA<-(A_IENOVA*PI_IENOVA)/VI
wTLEVISA<-(A_TLEVISA*PI_TLEVISA)/VI
matriz_w <- matrix(0,1,5)
matriz_w[,1] <- wGMEXICO
matriz_w[,2] <- wGRUMA
matriz_w[,3] <- wICHB
matriz_w[,4] <- wIENOVA
matriz_w[,5] <- wTLEVISA

vz99<- -2.33
vz95<--1.64
vz90<--1.28

#Estimacion del var ------------ NO SUPE COMO

var3_99 = vz99*VI*sqrt(matriz_w%*%covarianza%*%(t(matriz_w)))*sqrt(1)
var3_95 = vz95*VI*sqrt(matriz_w%*%covarianza%*%(t(matriz_w)))*sqrt(1)
var3_90 = vz90*VI*sqrt(matriz_w%*%covarianza%*%(t(matriz_w)))*sqrt(1)

#Nuevamente usamos bactesting 
BTEST90_3<- ifelse(PER_OBS<var3_90[1,],1,0)
EV90_3<-(sum(BTEST90_3)/na)
print(EV90_3)

BTEST95_3<- ifelse(PER_OBS<var3_95[1,],1,0)
EV95_3<-(sum(BTEST95_3)/na)
print(EV95_3)

BTEST99_3<- ifelse(PER_OBS<var3_99[1,],1,0)
EV99_3<-(sum(BTEST99_3)/na)
print(EV99_3)

