library((fBasics))
library(quantmod)
library(readxl)

T28d <- read.csv("Desktop/T28d.csv")
T28d <- T28d$X28d

r <- na.omit(diff(log(T28d))) #se comienza calculando los rendimientos de las tasas historico
actual <- tail(T28d, n=1)
tasasest <- actual*(r)+actual #se coloca ese rendimiento sobre la tasas de hoy para estimar las posibles del futuro sobre las historicas
tc <- .05
vn <- 2500
cupon <- vn*tc 

x <- matrix(0,6,length(r))
valorinicial<-c()
suma<-c()
pnl<-c()

for (j in 1:length(r)){
  for (i in 1:6){
    if (i==6){
      x[i,j]<-(vn+cupon)/(1+tasasest[j])^(i/6) #eston son los flujos considerando las tasas estimadas
      valorinicial[i]<-(vn+cupon)/(1+actual)^(i/6) #este es el flujo con la tasa de hoy
    }
    if (i!=6){
      x[i,j]<-cupon/(1+tasasest[j])^(i/6) #eston son los flujos considerando las tasas estimadas
      valorinicial[i]<-cupon/(1+actual)^(i/6) #este es el flujo con la tasa de hoy
    }
  }
  suma[j]<-sum(x[,j]) #se suman para obtener precio de bono con cada caso
  pi<-sum(valorinicial) #se suma para obtener el precio de bono hoy (inicial)
  pnl[j]<-suma[j]-pi #funcion de PnL sobre los rend. de tasas al futuro con los de hoy, esto nos permite ver a mañana con cada caso del pasado en los rend. posibles sobre la tasa actual
}

VAR99=quantile(pnl,.01)#99%confianza
VAR95=quantile(pnl,.05)#95%confianza
VAR90=quantile(pnl,.10)#95%confianza
print (VAR99)
print (VAR95)
print (VAR90)

