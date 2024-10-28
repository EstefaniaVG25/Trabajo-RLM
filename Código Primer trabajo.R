#1: ANALISIS EXPLORATORIO DE LAS VARIABLES--------------------------------------
#Después de reorganizar las variables cuantitativas y cualitativas, hicimos el siguiente 
#Analisis exploratorio

#Llamamos a las librerías necesarias
library(GGally)
library(graphics)
library(car)

#Pasar las variables cualitativas a factor
Datos$Brand <- as.factor(Datos$Brand)
Datos$Model <- as.factor(Datos$Model)
Datos$Processor <- as.factor(Datos$Processor)
Datos$Graphics_Card <- as.factor(Datos$Graphics_Card)
Datos$Operating_System <- as.factor(Datos$Operating_System)
Datos$RAM <- as.factor(Datos$RAM)
Datos$Storage <- as.factor(Datos$Storage)
Datos$Screen_Size <- as.factor(Datos$Screen_Size)
Datos$Warranty <- as.factor(Datos$Warranty)

#Análizar la base de datos en general
summary(Datos)
##Interpretación: Como podemos observar en el siguiente summary, el modelo tiene 11 "variables
#explicativas" que nos podrían indican las caracteristicas 
#por las cuales es posible el cambio de precio en cada computador, 
#siendo el precio del computador la variable número 12 del modelo
#y la llamaremos "variable respuesta".

#Del summary o resumen de los datos podemos concluir que:

#Marca: Encuentra sus valores en 7 categorias (Dell, Asus, HP, Acer, Lenovo,
#MSI, y Other) siendo Dell y Others las categorias con mayor numero de pc

#Modelo: El significado de esta variable lo asimilamos a el número de cedula
#o identificación, por lo que todos sus valores son diferentes en cada pc
#y esta variable no la tendremos en cuenta para nuestro MRL 

#Procesador: Encuentra sus variables en 5 categorias diferentes (AMD Ryzen 5,
#AMD Ryzen 7, AMD Ryzen 9,Intel i5, Intel i7, Intel i9), siendo AMD Ryzen 7
#la categoria con mayor numero de pc y Intel i7 la categoria con menor 
#cantidad

#Ram: Encuentra sus variables en 4 categorias (8, 16 32 y 64) siendo
# 32 la que tiene mayor numero de pcs y 8 la de menor cantidad

#Almacenamiento:Se encuentra en 4 categorías: 256, 512, 1024, 2048 GB.
#512 GB es la categoría con mayor número de PCs (796), y 256 GB tiene la menor cantidad (727).
#Tamaño de Pantalla:

#Tamaño.Pantalla: Se encuentra en 4 categorías: 13.3", 14.0", 15.6", 17.0".
#13.3" es la categoría con mayor número de PCs (819), y 17.0" tiene la menor
#cantidad (713).

#Tarjeta Gráfica: Se encuentra en 5 categorías: AMD Radeon, Intel UHD,
#NVIDIA GTX 1650, NVIDIA RTX 3060 y NVIDIA RTX3070).AMD Radeon es la 
#categoría con mayor número de PCs (634), y NVIDIA RTX 3060 tiene la menor
#cantidad (568).


#Sistema Operativo: Se encuentra en 4 categorías: Linux, macOS, Windows 10, 
#windows 11. Linux es la categoría con mayor número de PCs (759), 
#y Windows 10 tiene la menor cantidad (727).


#Peso: Variable continua que varía entre 1.000 kg y 3.500 kg.

#Duración de Batería: Variable continua que varía entre 3 y 15 horas.

#Garantía:Se encuentra en 3 categorías: 1 año, 2 años, 3 años.
#1 año es la categoría con mayor número de PCs (1103), y 3 años tiene la 
#menor cantidad (969)

#Realizamos la gráfica para rectificar lo visto en el summary
pairs(Datos)
##Interpretación: De estos graficos podemos concluir que no se observa que haya relación o una tendencia
##entre las variables y toca hacer un análisis más profundo

#Aplicar Modelo lm
modelo<- lm(Price ~ ., data = Datos)
summary(modelo)
#Interpretación: Del summary podemos observar que el modelo lm no explica el 
#comportamiento de las variables,pues la variable modelo no se puede interpretar
#porque es como si fuera la cedula del computador portatil.Para rectificar esto 
##creamos la gráfica del precio vs el modelo del computador portatil
boxplot(Datos$Price ~ Datos$Model) #De este Este gráfico podemos conlcuir nuevamente que 
                                   #no representa nada

#Por tal motivo creamos un modelo sin la variable MOdel
modelo <- lm(Price~.-Model, data = Datos)
summary(modelo)

##Interpretación: Del summary observamos que la única variable que aparece como
##significativa es Warranty2 ya que tiene un valorp menor que el nivel de 
##significancia.

#El R cuadrado da un valor de -0.001294,
#lo que significa que el modelo no es bueno ni apropiado para explicar la
#variabilidad del precio según el Peso y la Duración de la batería.

#Como las preguntas que queremos responder es saber cuáles son las variables cualitativas
#o cuantitativas que influyan más en el precio, analizamos individualmente 
#las grafiacs del precio vs las cualitativas

#Análizar las variables cualitativas
boxplot(Datos$Price ~ Datos$Brand)
boxplot(Datos$Price ~ Datos$Processor)
boxplot(Datos$Price ~ Datos$Graphics_Card)
boxplot(Datos$Price ~ Datos$Operating_System)
boxplot(Datos$Price ~ Datos$RAM)
boxplot(Datos$Price ~ Datos$Storage)
boxplot(Datos$Price ~ Datos$Screen_Size)
boxplot(Datos$Price ~ Datos$Warranty)

##Interpretación: primero, que de acuerdo a los gráficos podemos afirmar que no 
##existe una variable cualitativa que sea significativa.pues sus medias están en un rango similar

##Ya analizadas las variables cualitatitas, pasamos al analisis de las variables cuantitativas

#Análizar las variables cuantitativas
var_cuanti <- subset(Datos, select = c("Weight", "Battery_Life", "Price"))
modelo_cuanti <- lm(Price ~ ., data = var_cuanti)
summary(modelo_cuanti)
##Interpretación: De acuerdo al summary podemos afirmar que ninguna de las 
##variables cuantitativas es significativa, pues sus valores p son mayores al
##nivel de significancia.

#en promedio, por cada kilogramo adicional de peso, el precio disminuye en 8.71 
#unidades monetarias. Sin embargo, este efecto no es significativo dado que
# (valor p = 0.629); similarmente pasa con la duración de la bateria, 
#por cada hora adicional de duración de la batería, el precio disminuiría en 
#1.91 unidades monetarias y no es significativo pues (valor p = 0.617).

#Analizar las variables cuantitativas a través de gráficos
library(GGally)
ggpairs(var_cuanti)
ggcorr(var_cuanti)

library(ggplot2)
ggpairs(var_cuanti,
        upper = list(continuous = wrap("cor", size = 4, colour = "black")),
        lower = list(continuous = wrap("points", colour = "lightgreen", alpha = 0.7)),
        diag = list(continuous = wrap("barDiag", fill = "lightblue", colour = "blue")))

##Interpretación: De acuerdo a las gráficas, podemos decir que las variables
##"Weight" y "Battery_Life" no tienen relación con el precio de las computadoras
##portatiles, confirmando así la conclusión anterior

#2: MULTICOLINEALIDAD-----------------------------------------------------------
#Analizar la multicolinealidad de las variables cuantitativas
modelo_cuanti <- lm(Price ~ ., data = var_cuanti)
vif(modelo_cuanti)
##Interpretación: Según los valores dados, no hay problemas de multicolinealidad
##pero tampoco hay correlación entre la variable respuesta y las regresoras.

#Analizar la multicolinealidad de las variables cualitativas con el anova
var_cuali <- subset(Datos, select=c("Brand","Processor","Graphics_Card",
                                    "Operating_System","RAM","Storage",
                                    "Screen_Size","Warranty","Price"))
#Nota: Agregamos la variable cuantitativa Price en var_cuali, porque la 
#necesitabamos ahí para el anova

#Crear anova
Anova <- aov(Price ~., data = var_cuali)
summary(Anova)
##Interpretación: Según los resultados del anova, ningún valor p es menor al
##alfa(0,05), 

#Crear modelos-----------------------------------------------------------------
modelo1 <- lm(Price ~ Storage + RAM + Processor+ Brand,
              data=Datos)
summary(modelo1)

#Validación estadistica del modelo
#1:Normalidad
Residuales <- rstandard(modelo1)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad


Datos$Residuales <- rstandard(modelo1)
library(car)
qqPlot(Datos$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo1)

Datos$Valores_Ajustados <- modelo1$fitted.values
ggplot(Datos, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo1)
bgtest(modelo1)

Datos$Id <- seq(1,nrow(Datos))
ggplot(Datos, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)
#Según la prueba y el gráfico el modelo sí tiene independencia

#Transformación del modelo1 


#Analizar outliers y puntos de influencia
#Detección de outliers y puntos de influencia----------------------------------
plot(Datos$Price,Residuales)

options(max.print=24000)
#2. Hacemos las pruebas estadisticas------------------------------------------
influence.measures(modelo1)

#Distancia de cook
#si D>1 el punto i es un punto de influencia
#si D>falpha,p,n-p el punto i es un outlier
f_alpha <- qf(1-0.05, df1 = 5, df2 = 3000 - 5)
f_alpha

2.217085

#Según el criterio de la distacia de cook, ninguno es mayor a el falpha:2.217085

# DFFITS
#si|DFFITS|>1 el punto i es un punto de influencia(muestras peuqeñas y medianas)
#si |DFFITS|>2sqrt(p/n) el punto i es un punto de influencia(muestras grandes
Criteriodf <- 2*(sqrt(5/3000))
Criteriodf

0.08164966
#Para los dfit                                                            
#punto 6 
#punto 11
#punto 16
#Punto 19
#Punto 27
#Punto 33
#Punto 35

#Para los dfbetas
criteriodfb<- 2/(sqrt(3000))
criteriodfb
0.03651484

#del dfbeta intercepto se eliminan 8, 27, 35

#codigo para quitarlos de la base de datos
Datos1<-Datos[-c(6,8,10,11,14,17,19,25,27,28,33,34,35,40,41,47,52,66,68,71,
                 72,73,75,83,84,86,103,106,118,121,125,129,131,133,137,139,
                 149,157,161,162,163,168,170,174,175,176,177,183,185,188,192,
                 197,203,204,206,210,211,213,214,216,217,220,225,226,229,231,
                 234,235,241,250,251,253,258,265,271,280,285,287,289,295,296,
                 299,300,303,314,316,318,326,331,332,352,356,357,369,362,365,
                 369,370,376,377,382,384,392,394,396,400,401,403,404,410,411,412,425,
                 428,430,431,433,442,449,462,465,467,468,472,473,474,477,480,487,489,494,496,501,
                 505,508,513,516,518,19,525,526,527,528,529,533,535,543,544,546,
                 549,556,559,567,568,574,577,581,595,596,604,609,610,611,612,622,
                 626,627,634,637,639,644,650,651,655,663,664,665,669,670,673,
                 676,679,684,686,692,696,704,708,709,710,716,722,724,731,738,
                 740,742,744,754,762,766,773,775,778,781,782,783,785,790,791,
                 797,798,799,801,818,822,826,828,831,833,836,838,840,841,847,849,
                 852,857,858,860,864,867,874,878,896,898,900,901,92,907,907,
                 911,913),]
Datos1

modelo1.1 <- lm(Price ~ Storage + RAM + Processor+ Brand,
              data=Datos1)
summary(modelo1.1)

#Nuevamente pruebas de validación eestadistica del modelo   
#1:Normalidad
Residuales <- rstandard(modelo1.1)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad

Datos$Residuales <- rstandard(modelo1.1)
library(car)
qqPlot(Datos1$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo1.1)
bptest(modelo1)

Datos1$Valores_Ajustados <- modelo1.1$fitted.values
ggplot(Datos1, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo1.1)
bgtest(modelo1.1)

Datos1$Id <- seq(1,nrow(Datos1))
ggplot(Datos1, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)

#Probando eliminando variables nos dimos cuenta que r cuadrado ajustado daba negativo, pero más pequeño
#Por ende, decidimos trabajar con menos variables y planteamos el siguiente modelo


modelo2 <- lm(Price~Warranty+Operating_System+Storage, data=Datos)
summary(modelo2)

#Prubas de validación estadistica
#1:Normalidad
Residuales <- rstandard(modelo2)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad

Datos$Residuales <- rstandard(modelo2)
library(car)
qqPlot(Datos$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo2)

Datos$Valores_Ajustados <- modelo2$fitted.values
ggplot(Datos, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo2)
bgtest(modelo2)

Datos$Id <- seq(1,nrow(Datos))
ggplot(Datos, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)

#Transformación para que de normalidad
plot(Datos$Price,Residuales)

options(max.print=11600)

#2. Hacemos las pruebas estadisticas------------------------------------------
influence.measures(modelo2)

#Distancia de cook
#si D>1 el punto i es un punto de influencia
#si D>falpha,p,n-p el punto i es un outlier
f_alpha <- qf(1-0.05, df1 = 4, df2 = 3000 - 4)
f_alpha

2.374898

#Según el criterio de la distacia de cook, ninguno es mayor a el falpha:2.217085

# DFFITS
#si|DFFITS|>1 el punto i es un punto de influencia(muestras peuqeñas y medianas)
#si |DFFITS|>2sqrt(p/n) el punto i es un punto de influencia(muestras grandes
Criteriodf <- 2*(sqrt(4/3000))
Criteriodf

0.07302967

#Para los dfbetas
criteriodfb<- 2/(sqrt(3000))
criteriodfb
0.03651484

#del dfbeta intercepto se eliminan 8, 27, 35

#codigo para quitarlos de la base de datos
Datos2<-Datos[-c(14, 17,19, 25,34, 35, 41, 52, 66, 71, 72, 73, 75, 84, 103,
                 106, 121, 125, 133, 137, 149, 157, 162, 174, 177, 188, 192,
                 204, 210, 213, 214, 217, 225, 226, 235, 241, 253, 258, 271,
                 280, 285, 296, 300, 316, 318, 326, 331, 332, 356, 365, 376, 
                 377, 382, 392, 394, 396, 400, 401, 410, 411, 430, 431, 449, 
                 468, 474, 480, 487, 489, 494, 496, 501, 505, 513, 518, 519,
                 525, 527, 533, 544, 546, 549, 556, 567, 581, 588, 595, 610,
                 612, 622, 637, 644, 655, 663, 669, 673, 710, 711, 722, 738,
                 742, 766, 773, 775, 781, 782, 783, 785, 790, 818, 822, 826,
                 828),]
Datos2

#Prubas de validación estadistica
#1:Normalidad

modelo2.1 <- lm(Price~Warranty+Operating_System+Storage, data=Datos2)
summary(modelo2.1)
summary(modelo2)

Residuales <- rstandard(modelo2.1)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad

Datos2$Residuales <- rstandard(modelo2.1)
library(car)
qqPlot(Datos2$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo2.1)

Datos2$Valores_Ajustados <- modelo2.1$fitted.values
ggplot(Datos2, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo2.1)
bgtest(modelo2.1)

Datos2$Id <- seq(1,nrow(Datos2))
ggplot(Datos2, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)

modelo3 <- lm(Price~Warranty+Operating_System, data=Datos)
summary(modelo3)

#Prubas de validación estadistica
#1:Normalidad
Residuales <- rstandard(modelo3)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad

Datos$Residuales <- rstandard(modelo3)
library(car)
qqPlot(Datos$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo3)

Datos$Valores_Ajustados <- modelo3$fitted.values
ggplot(Datos, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo3)
bgtest(modelo3)

Datos$Id <- seq(1,nrow(Datos))
ggplot(Datos, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)


#Transformación para que de normalidad
plot(Datos$Price,Residuales)

options(max.print=960)

#2. Hacemos las pruebas estadisticas------------------------------------------
influence.measures(modelo3)

#Distancia de cook
#si D>1 el punto i es un punto de influencia
#si D>falpha,p,n-p el punto i es un outlier
f_alpha <- qf(1-0.05, df1 = 3, df2 = 3000 - 3)
f_alpha

2.607873

#Según el criterio de la distacia de cook, ninguno es mayor a el falpha:2.217085

# DFFITS
#si|DFFITS|>1 el punto i es un punto de influencia(muestras peuqeñas y medianas)
#si |DFFITS|>2sqrt(p/n) el punto i es un punto de influencia(muestras grandes
Criteriodf <- 2*(sqrt(3/3000))
Criteriodf

0.06324555

#Para los dfbetas
criteriodfb<- 2/(sqrt(3000))
criteriodfb

0.03651484

Datos3<-Datos[-c(6, 14, 17, 19, 25, 34, 35, 41, 52, 66, 71, 72, 73, 75, 84, 86),]
Datos3

modelo3.1 <- lm(Price~Warranty+Operating_System, data=Datos3)
summary(modelo3.1)
summary(modelo3)

Residuales <- rstandard(modelo3.1)
shapiro.test(Residuales)
#Según la prueba shapiro no hay normalidad

Datos3$Residuales <- rstandard(modelo3.1)
library(car)
qqPlot(Datos3$Residuales, xlab= "Cuantiles de distribucion normal",
       ylab= "cuantiles de residules", pch= 16, col= "pink",
       col.lines = "orange")

#Según la gráfica se puede concluir que el modelo no tienen normalidad,
#pues el gráfico sigue una distribución con colas delgadas 

#2: Homocedasticidad: varianza constante
library(lmtest)
bptest(modelo3.1)

Datos3$Valores_Ajustados <- modelo3.1$fitted.values
ggplot(Datos3, aes(x= Valores_Ajustados, y=Residuales)) +
  geom_point(color= "brown", size= 2) +
  geom_hline(yintercept = c(-3.5,0,3.5), linetype= "dotted",
             color="black", size=1)

#3:Independencia
dwtest(modelo3.1)
bgtest(modelo3.1)

Datos3$Id <- seq(1,nrow(Datos3))
ggplot(Datos3, aes(x=Id, y=Residuales))+
  geom_point(color="black", size=2)+
  geom_hline(yintercept=c(-3.5,0,3.5), linetype="dotted",
             color="red", size=1)
