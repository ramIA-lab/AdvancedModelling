#### Script PROCESOS DE PUNTOS (GEOESTADISTICA TIPO II)
library("raster")
library("spatstat")

### La generación de la base de datos es totalmente artificial y refiere a un fenómeno de 
### Tipo II cualquiera en un espacio bidimensional en coordenadas X e Y
### Las variables SMI, NDVI,LST y CROP son variables adicionales al fenómeno que se estudia y que sirven
### para denotar procesos de puntos con "marcas"

set.seed(1019093771)
seqlat=seq(from=-73.3,to=-73.25,by=0.001)
seqlong=seq(from=5.54,to=5.58,by=0.001)
latitude=sample(seqlat,size = 100,replace=TRUE)
longitud=sample(seqlong,size = 100,replace=TRUE)
xy=data.frame(x=longitud,y=latitude)
plot(xy$x,xy$y,xlab="x",ylab="y",main = "Nube de puntos")
##xy vendría a ser vuestro data frame que contiene los
##puntos u observaciones del fenómeno PUNTUAL que se
##desea estudiar
SMI=sort.int(runif(100,0.7,0.95),partial=10)
NDVI=sort.int(rnorm(100,0.45,0.06),partial=10)
LST=sort.int(26*rbeta(100,shape1=0.87,shape2=0.91),partial=10)

###AGREGAMOS otra variable de marca al modelo del tipo binario (ejemplo: el fenomeno dividido en 
### categorías 0 y 1
crop=factor(ifelse(rgamma(n=100,rate=0.8,shape=0.5)<0.5,0,1))
df1=data.frame(xy,SMI,NDVI,LST,crop)

###Ejemplos de conversión de dataframe a datos espaciales
### para el análisis de PUNTOS

dataxy=ppp(xy$x,xy$y,xrange=c(min(xy$x),max(xy$x)),yrange=c(min(xy$y),max(xy$y)))
any(duplicated(dataxy))  #El conjunto presenta puntos duplicados.
y=unique(dataxy);
y  #se borran los puntos duplicados.

###Igual al anterior con un variable de marca
y1=unique(ppp(xy$x,xy$y,xrange=c(min(xy$x),max(xy$x)),yrange=c(min(xy$y),max(xy$y)),marks = df1$crop))            
any(duplicated(y1)) 
plot(y1,size=0.8,cols = "violet",main="Grafico de dispersión de puntos de y1")   #Gráfica de y1
y2=unique(ppp(xy$x,xy$y,xrange=c(min(xy$x),max(xy$x)),yrange=c(min(xy$y),max(xy$y)),marks = df1$NDVI))
plot(y2,size=0.0008,cols="blue",main="Grafico de dispersión de puntos de y2") #Grádica de y2
any(duplicated(y2))  
dataxy3=ppp(xy$x,xy$y,xrange=c(min(xy$x),max(xy$x)),yrange=c(min(xy$y),max(xy$y)),marks = df1[,3:6])
y3=unique(dataxy3)
#Gráfica con marca NDVI.
plot(y3,which.marks = "NDVI",size=0.0009,main = "Gráfica con marca NDVI")  
#Gráfica con la marca crop.
plot(y3,which.marks="crop",size=0.7,main = "Gráfica con marca crop")  
par(mfrow=c(1,1))
#Gráfica con todas las marcas.
plot(y3,size=0.002,main = "Gráfica con todas las marcas")
ventana1=owin(c(5.546,5.558),c(-73.289,-73.277));ventana1
coords(y3)
npoints(y3)  #Número de puntos.
marks(y3)   
as.owin(y3)  #enseña la ventana definida en la creación.
as.data.frame(y3) #enseña las coordenadas y las marcas creadas

##Análisis Gráfico HOT SPOTS -->Densidades e Intensidades
### Unicamente proceso de puntos sin marcas
plot(density(y))
contour(density(y))

hist(xy$x,xlab = "x",ylab="Frecuencia",main = "Histograma de frecuencia de x",
     col = "cadetblue3")   # Histograma de la longitud.

hist(xy$y,xlab = "y",ylab="Frecuencia",main = "Histograma de frecuencia de y",
     col = "burlywood1")  #Histograma de la latitud.
plot(density(y))
plot(y,add=TRUE)

#se usa la función split para dividir en dos tablas el df1, según si hay
#o no ausencia de la modalidad en la variable categórica
#agregada como marca (recuerden que se llama crop para este ejemplo)
dividir=split(df1,crop,drop = TRUE)
#Se toman los puntos donde hay "crop"
ausencia=dividir$"1";ausencia

##Analizamos el objeto espacial con crop=1
y4=unique(ppp(ausencia$x,ausencia$y,xrange=c(min(ausencia$x),max(ausencia$x)),yrange=c(min(ausencia$y),max(ausencia$y)),marks = ausencia[,6]))
any(duplicated(y4)) 
intensity(y4)
plot(y4,cols = "purple",main = "Patrón de puntos donde la variable crop=1")
plot(density(y4))
contour(density(y4))
#Gráfica de la marca que identifica los puntos con crop.
a=as.im(y4)     #Convertir en imagen el patrón
plot(a)         #
plot(y4,add = TRUE)
summary(y4)
summary(y3)

###Mayores detalles
summary(y)
unitname(y)<-"centimeters"
#La densidad de metros por centimetros cuadrados estimada
summary(y)$intensity
#calculando la densidad de puntos por metros cuadrados estimada.
lambda=summary(y)$intensity*(1000)^2;
lambda
intensity(y1)
#Construya una cuadricula de 4 filas y 4 columnas para ver la distribución del conteo de puntos en cada celda.
#cuenta los puntos en cada cuadrícula.
quadratcount(y, nx = 4, ny = 4) 
#guardar los conteos como una matriz en el objeto Q.
Q <- quadratcount(y, nx = 4, ny = 4)
#Gráfica de puntos superpuesta por las cuadrículas.
plot(y,size=0.7)  
plot(Q, add = TRUE)
plot(intensity (Q, image=T))
plot(Q, add=T)
plot(y, add=T)
#gráfica de la densidad sobrepuesta por el patrón de puntos.
densidad=density(y)
plot(y,add=TRUE)
persp(densidad)    #Gráfico 3D de la densidad.
persp(densidad, theta= 30, phi = 30, col = "lightblue")

###Prueba estadística sobre la densidad (conteo puntos) en el espacio
###Se seguirá la prueba de hipótesis χ2
###para comprobar aleatoriedad espacial

#H0:La intensidad es homogenea
#H1:La intensidad no es homogenea
#Si p<a entonces rechazamos H0.
M=quadrat.test(y, nx = 4, ny = 4)
M
plot(y,size=0.01)
plot(M, add = TRUE, cex = 0.9)

#Estimación usando kernel
#La función intensidad puede ser estimada usando una
#función kernel. Con spatstat utilzamos la función density,
#por defecto, utilizamos un kernel gaussiano, con el ancho de
#banda de suavisado determinado por el argumento sigma.
#Por defecto se utiliza la corrección uniforme, y la salida
#es una imagen im.
D05 = density(y, sigma=0.5)
D1 = density(y, sigma=1)
D15 = density(y, sigma=1.5)
plot(D05)


##Influencias de variables o marcas
##Con los datos y2 vemos la influencia de NDVI 
intensity(y2)
plot(density(y2))
plot(y2, add=T)
contour(density(y2), add=T, col="white")





