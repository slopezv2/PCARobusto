#Librerias de donde se saca los datos antes utilizar install.packages() library(mosaicData)
library(dplyr)
library(ggplot2)
library(MASS)
#carga de datos de la libreria
data<-SaratogaHouses
#nombres de las variables
names(data)
# si se desea realizar con todas las variables la matriz de covarianzas hacer lo siguiente
#de las variablescategoricas se convierten en numericas summary(data$heating)
summary(data$fuel)
summary(data$sewer)
summary(data$waterfront)
summary(data$newConstruction)
summary(data$centralAir)
levels<-c("hot air","hot water/steam","electric") levels_fuel<-c("gas","electric","oil") levels_sewer<-c("septic","public/commercial","none") levels_waterfront<-c("Yes","No")
levels_new<-c("Yes","No")
levels_cair<-c("Yes","No")
data$heating <- match(data$heating, levels)
data$fuel <- match(data$fuel, levels_fuel)
data$centralAir <- match(data$centralAir, levels_cair)
data$sewer <- match(data$sewer, levels_sewer)
data$waterfront <- match(data$waterfront, levels_waterfront) data$newConstruction <- match(data$newConstruction, levels_new)
#### utilizando la funcion de mahalanobis
colMeans(data)
a<-cov(data)
data$mahal<-mahalanobis(data, colMeans(data), cov(data))
data$p <- pchisq(data$mahal, df=ncol(data), lower.tail=FALSE) outliers_tdata<-data[data$mahal > data$p,]
#solo con las variables numericas
# Finding distances
distances <- mahalanobis(x = data[,c(1:10)] , center = colMeans(data[,c(1:10)])
, cov =cov(data[,c(1:10)])) # Cutoff value for ditances from Chi-Sqaure Dist.
#cambiar el p a 0.99 para la prueba con esa probabilidad cutoff <- qchisq(p = 0.95 , df = ncol(data[,c(1:10)]))
## Display observation whose distance greater than cutoff value outliersp1<-data[distances > cutoff ,]
#cantidad de datos
nrow(outliersp1)
#Graficar 2 variables:
grafic<-data[,c("price","landValue")]
distances <- mahalanobis(x =grafic, center = colMeans(grafic) , cov =cov(grafic))
cutoff <- qchisq(p = 0.95 , df = ncol(grafic)) outliersp1<-grafic[distances > cutoff ,]
nrow(outliersp1)
rad = qchisq(p = 0.95 , df = ncol(grafic))
rad = sqrt(rad)
ellipse <- car::ellipse(center = colMeans(grafic) , shape = cov(grafic) , radius = rad ,
                        segments = 54 , draw = FALSE)
# Ellipse coordinates names should be same with air data set ellipse <- as.data.frame(ellipse)
colnames(ellipse) <- colnames(grafic)
figure <- ggplot(grafic , aes(x = landValue , y = price)) + geom_point(size = 2) +
geom_polygon(data = ellipse , fill = "orange" , color = "orange" ,
alpha = 0.5)+
geom_point(aes(colMeans(grafic)[2] , colMeans(grafic)[1]) , size = 5 ,
color = "blue") +
geom_text( aes(label = row.names(grafic)) , hjust = 1 , vjust = -1.5
,size = 2.5 ) +
ylab("precio de la vivienda") + xlab("valor del terreno")
figure
library(mosaicData)
library(dplyr)
library(ggplot2)
library(MASS)
######punto 2#####################################################
##boostrap method para todas las variables datos<-SaratogaHouses
levels<-c("hot air","hot water/steam","electric") levels_fuel<-c("gas","electric","oil") levels_sewer<-c("septic","public/commercial","none") levels_waterfront<-c("Yes","No") levels_new<-c("Yes","No") levels_cair<-c("Yes","No")
datos$heating <- match(datos$heating, levels)
datos$fuel <- match(datos$fuel, levels_fuel)
datos$centralAir <- match(datos$centralAir, levels_cair) datos$sewer <- match(datos$sewer, levels_sewer)
datos$waterfront <- match(datos$waterfront, levels_waterfront) datos$newConstruction <- match(datos$newConstruction, levels_new)
boostrap<- replicate(n=1000,sample(1:1728,872,replace = FALSE))#duda con true
minimo<-det(cov(datos))
for(i in 1:1000){
  a<-boostrap[,i]
  m<-datos[a,]

c<-cov(m)
  deter<-det(c)
  if(deter<minimo){
    minimo<-deter
    macv2<-c
    x<-m
} }
<- mahalanobis(x = x , center = colMeans(x) , cov =macv2) qchisq(p = 0.95 , df = ncol(x))
distancesmcd
cutoffmcd <-
outliersp2<-x[distancesmcd > cutoffmcd ,]
#Seleccion de las variables que entraran en la funcion, las 10 numericas
x<-datos[,1:10]
x2<-datos[,c("price","landValue")]
n=1728
p=10
a<-cov.rob(x, cor = FALSE, quantile.used = floor((n + p + 1)/2),
           method ="mcd",
           nsamp = "best", seed=1250)
#Cambiar p a 0.99
distancesmcd <- mahalanobis(x = x , center = a$center , cov =a$cov) cutoffmcd <- qchisq(p = 0.95 , df = ncol(x)) outliersp2<-x[distancesmcd > cutoffmcd ,]
#Graficar 2 variables:
#grafic<-data[,c("price","landValue")]
rad = qchisq(p = 0.99 , df = ncol(x2))
rad = sqrt(rad)
ellipse <- car::ellipse(center = a$center , shape = a$cov , radius = rad ,
                        segments = 362 , draw = FALSE)
# Ellipse coordinates names should be same with air data set ellipse <- as.data.frame(ellipse)
colnames(ellipse) <- colnames(x2)
figure <- ggplot(grafic , aes(x = landValue , y = price)) + geom_point(size = 2) +
geom_polygon(data = ellipse , fill = "orange" , color = "orange" ,
alpha = 0.5)+
geom_point(aes(a$center[2] , a$center[1]) , size = 5 , color = "blue")
+
geom_text( aes(label = row.names(x2)) , hjust = 1 , vjust = -1.5 ,size
= 2.5 ) +
ylab("precio de la vivienda") + xlab("valor del terreno")
figure
