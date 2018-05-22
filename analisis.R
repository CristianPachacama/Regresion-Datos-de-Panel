library(readxl)
library(forecast)
library(foreign)
library(car)
#library(easyGgplot2)
#Carga de Datos
getwd()
datos <- read_excel("datos.xlsx")
nombre <-names(datos)
names(datos) <- c("Com","Bom","P","Tr","API","Rg","Reservorio","Pozo")
View(datos)

#Quitar Limoncocha
datos=datos[datos$Pozo!="Limoncocha 28",]

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#Modelo Regresion ================================
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
regresion = function(datos,n = 3,pozo="Daimi 17"){
  
  #Formula de orden n
  formu = paste0("I(P^",1:n,")")
  formu = paste(formu,collapse = "+")
  formu = paste0("Bom ~ ",formu)
  formu = as.formula(formu)
  
  #Modelo de Regresion
  datos = datos[datos$Pozo==pozo,]
  modelo = lm(data=datos,formula = formu)
  summary(modelo)
  datos$Boe = predict(modelo,datos)
  
  #Calculo de Co
  betas = c()
  for (i in 1:n) {
    betas[i] = modelo$coefficients[i+1]
  }
  
  P_aux = datos$P
  numerador = 0
  for (j in 1:n) {
    numerador = numerador + j*betas[j]*(P_aux^(j-1))
  }
  datos$Coe = -numerador/(datos$Boe)
  
  #Grafico Conjunto de Bo y Co
  par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
  
  plot(datos$P,datos$Bom, xlab = "Presión",ylab = "Bom & Boe")
  lines(datos$P,datos$Boe,col='red')
  
  plot(datos$P,datos$Com, xlab = "Presión",ylab = "Com & Coe")
  lines(datos$P,datos$Coe,col='red')
  mtext(paste(pozo,", "," n=",n),outer = TRUE,cex = 1.5)
  
  return(list('datos'=datos,'modelo'=modelo))
}


#Generacion de Resumen Modelo ---------------
Pozos = unique(datos$Pozo)

p=length(Pozos)
for (k in 1:p) {
  mod = regresion(datos,n=4,pozo=Pozos[k])
  print(summary(mod$modelo))
}


#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# Modelo Regresion Datos de Panel ----------------
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#Grafico (Datos de Panel)  ---------------------
library(ggplot2)
library(plotly)
grafBom = ggplot(data=datos, aes(x=P, y=Bom, colour=factor(Pozo))) + 
  geom_line() + geom_point() + ggtitle("Gráfico Pozos")
ggplotly(grafBom)

grafCom =ggplot(data=datos, aes(x=P, y=Com, colour=factor(Pozo))) + 
  geom_line() + geom_point()+ ggtitle("Gráfico Pozos")
ggplotly(grafCom)


# Funcion que genera regresión ------------------
regresionPanel = function(datos,n = 3){
  
  #Formula de orden n
  formu = paste0("I(P^",1:n,")")
  formu = paste(formu,collapse = "+")
  formu = paste0("Bom ~ ", formu , " + factor(Pozo)*P")
  formu = as.formula(formu)
  
  #Modelo de Regresion
  modelo <- lm(data=datos,formula = formu)
  #summary(modelo)
  datos$Boe <- predict(modelo,datos)
  
  #Calculo de Co
  betas = c()
  for (i in 1:n) {
    betas[i] = modelo$coefficients[i+1]
  }
  #Coeficientes de Factor POZO
  alfas0 = modelo$coefficients[5:(length(Pozos)-1)]
  alfas0 = c(0,alfas0)
  
  ind_nomb = endsWith(x=names(modelo$coefficients),suffix = ":P")
  alfas = modelo$coefficients[ind_nomb]
  alfas = c(0,alfas)
  
  Pozos = unique(datos$Pozo)
  
  numerador= c()
  for(k in 1:length(Pozos)){
    
    numerador_aux = 0
    ind_pozo = datos$Pozo==Pozos[k]
    P_aux = datos$P[ind_pozo]
    
    for (j in 1:n) {
      if(j==1){
        numerador_aux = numerador_aux + j*(betas[j]+alfas[k])*(P_aux^(j-1))
      }else{
        numerador_aux = numerador_aux + j*betas[j]*(P_aux^(j-1))
      }
      
    }
    numerador=c(numerador,numerador_aux)
  }
  
  datos$Coe = -numerador/(datos$Boe)
  
  
  return(list('datos'=datos,'modelo'=modelo))
}

# remove(analisisPanel)
#Generacion de Resumen Modelo
analisisPanel = regresionPanel(datos,n=3)
datosPanel = analisisPanel$datos
summary(analisisPanel$modelo)


#Grafico
grafPanel = function(datosPanel,n=3){
  
  Pozos = unique(datosPanel$Pozo)
  
  for(pozo in Pozos){
    
    datos_aux = datosPanel[datosPanel$Pozo==pozo,]
    
    par(mfrow=c(1,2),oma = c(0, 0, 2, 0))
    
    plot(datos_aux$P,datos_aux$Bom, xlab = "Presión",ylab = "Bom & Boe")
    lines(datos_aux$P,datos_aux$Boe,col='red')
    
    plot(datos_aux$P,datos_aux$Com, xlab = "Presión",ylab = "Bom & Boe")
    lines(datos_aux$P,datos_aux$Coe,col='red')
    
    mtext(paste("Regresion Polinomial, n=",n),outer = TRUE,cex = 1.5)
    
  }
  
}

grafPanel(datosPanel)

#####

#Correccion
datos_aux = datosPanel[datosPanel$Pozo==Pozos[k],]
datos_aux




