#### GEOESTADISTICA ######
#### PMAAD ###############

#####********** Cargar Librerias
library(sp)
library(geoR)
library(sm)
library(gstat)
library(npsp)
library(geohashTools) 
library(rgdal)
library(ggmap)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(tidyverse)
library(tmap)
library(car)
library(sf)
library(automap)

##### Preliminares (funciones útiles en cualquier momento)
# Convertir datos (data.frame a a objetos geoespaciales)
# Renombre latitud y longitud en su base de datos con "x" correspondiente
# a la longitud y "y" correspondiente a la latitud, tal como se vió en la teoría.
#---->  coordinates(dataframe) <- c("x", "y")

#Otra manera (útil para calcular distancias en el plano proyectado CRS)
# Creamos un Data Frame con los datos necesarios (aqui dos puntos, pero pueden ir vectores
datos <- data.frame(lat = c(-34.714656, 51.532068), long = c(-58.785999,0.177305))
# Lo convertimos a un objeto sf
puntosEspaciales <- st_as_sf(datos, coords = c("long", "lat"),crs = 4326) #crs=4326 es un estándar
st_distance(puntosEspaciales)  # En metros
st_distance(puntosEspaciales)/1000  # En kilómetros


####************Datos

data(meuse)
help(meuse)
backup<-meuse
str(meuse)

#### AED, para verificar tendencias y dependencia espacial

meuse.geoR <- geoR::as.geodata(meuse, coords.col = 1:2, data.col = 6)
plot(meuse.geoR, lowess=TRUE, scatter3d=FALSE, cex=0.8)
qqPlot(meuse$zinc, distribution="norm", lwd=2, pch="+", line="robust", las=0, cex=0.8, col.lines="red", main="Quantile-Comparison Plot")
hist(meuse$zinc, breaks = 16) #Distribución no simétrica, sesgada

#Al ser una distribución no simétrica, se aplica logaritmo para transformar
#los valores y obtener una distribución con un mejor shape. 
#Esto, además, reduce los posibles outliers.

meuse$logZn <- log10(meuse$zinc)
backup$logZn <- log10(backup$zinc)
hist(meuse$logZn, breaks = 16)

#Recordatorio
##Luego para transformar a los valores originales se utiliza antilog. antilog (x)=10^x

head(10^(meuse$logZn))

#Crear Spatial Data Frame

coordinates(meuse) <- c("x", "y") #or
#coordinates(meuse) <- ~ X + Y

#Renombre latitud y longitud en su base de datos con x correspondiente
# a la longitud y y correspondiente a la latitud, tal como se vió en la teoría.
class(meuse)
str(meuse)

####*************Explorar dependencia espacial
#Visualizar datos. Los puntos no están regularmente distribuidos, 
#sino que son mas densos en la cercanía del río, por tanto se aprecia una dependencia espacial factible 
#a ser modelada.

plot(meuse, asp = 1, pch = 1) 

#cargar otro conjunto de datos llamado meuse.riv que contiene lineas de las márgenes del río.

data(meuse.riv)
lines(meuse.riv)

#Otra visualización con símbolos proporcionales
plot(meuse, asp = 1, cex = 4 * meuse$zinc/max(meuse$zinc),pch = 1) 
#cex tamaño de circulos proporcionales al valor
lines(meuse.riv)

#La dependencia espacial local significa que cuanto mas cerca estén los puntos en el espacio geográfico,
#mas cerca también lo están en el espacio de atributos. Esto se llama autocorrelación.
#Los valores de un atributo pueden estar correlacionadon con si mismos, y la fuerza de esta correlación
#depende de la distancia de separación entre puntos.
#Cada par de puntos, estará separado por una distancia en el espacio geográfico y una semivarianza en el espacio
#de atributos.

#Calcule cuantos pares de puntos hay en el dataset meuse.
n <- length(meuse$logZn)
n * (n - 1)/2

#calcule la distancia y semivarianza entre los dos puntos primeros puntos del dataset
coordinates(meuse)[1, ]#punto 1
coordinates(meuse)[2, ]#punto 2
sep <- dist(coordinates(meuse)[1:2, ])
sep #distancia
gamma <- 0.5 * (meuse$logZn[1] - meuse$logZn[2])^2 # semivarianza
gamma

####*************Estudiar la estructura de la dependencia espacial --> SEMIVARIOGRAMA EMPIRICO y TEORICO
#ve <- variogram(logZn ~ 1, meuse) 
ve<- variogram(logZn~1, meuse, width=90, cutoff=1300)

## Note que logZn ~ 1 implica modelar sin T(s), es decir se asume que no existe componente T(s)
# variogram: genera la nube de variograma. logZn ~ 1: logZn es dependiente de si misma
#                            --> autocorrelación. 
#Por defecto (si no se especifica cutoff y with )el cutoff es 1/3 de la máxima distancia
#(diagonal del bbox). Esto es dividido en 15 clases igualmente espaciadas.

#¿Cuál es la evidencia de dependencia espacial local?
#np=número de pares de puntos para cada una de las 15 clases, dist=distancia media, 
#gamma=semivarianza media. A medida que la distancia aumenta, también lo hace la semivarianza,
#pero hasta una distancia determinada donde la semivarianza se estabiliza. 

ve  
plot(ve, plot.numbers = T, asp=1)

#MODELO DE VARIOGRAMA O VARIOGRAMA TEÓRICO  (Búsqueda del mejor fitting teórico para el variograma
#empirico del paso anterior

#Función que ajusta el variograma teórico. 
#Hay diferentes tipos de funciones que pueden ser útiles:
show.vgms()

#Métodos para seleccionar el modelo que mejor ajusta al variograma teórico.
#1- Ajuste visual
#2- Ajuste automático y comparar la bondad del ajuste

# Ajuste visual (usando el gráfico del variograma empirico calculamos rango, nugger y silo(meseta)

# Range o rango: separación o distancia entre pares de puntos en la cual ya no hay 
# dependencia espacial.
# Nugget o pepita: semivarianza a la separación de 0m.
# Total-sill o meseta: semivarianza a la distancia del rango.
# Partial-sill o meseta parcial= (total sill - nugget. aprox)   
# Ejemplo para un modelo teórico esférico
#vgm genera el modelo de variograma
vt <- vgm(psill = 0.12, model = "Sph", range = 850,nugget = 0.01) 
vt 
plot(ve, pl = T, model = vt) # Plot entre el variograma empirico y el variograma teórico

#Ajuste automático
#fit.variogram: ajuta el modelo de variograma a un variograma empírico.
va <- fit.variogram(ve, vt) 
plot(ve, pl = T, model = va)

#o
# # psill is "Partial Sill" (do not confound with the sill)
omnivgmodel <- fit.variogram(ve,model=vgm(psill=var(meuse$logZn), model="Sph", range=NA, nugget=NA),
                             fit.sills=T, 
                             fit.ranges=T, 
                             fit.method=7, 
                             warn.if.neg=T, 
                             fit.kappa=F)


####*************KRIGING ##################
# Como no hay T(s) usamos Kriging ORDINARIO

#Usualmente kriging se utiliza para predecir los píxeles (o nodos) de una malla regular
#que cubre la zona de estudio.
data(meuse.grid) #malla de 40m x 40m, disponible con el dataset meuse.
coordinates(meuse.grid) <- c("x", "y")
gridded(meuse.grid) <- T #indica que el conjunto de datos es un raster

#Predicción
ok <- krige(logZn ~ 1, locations = meuse, newdata = meuse.grid, model = va)   
#kriging ordinario significa que (1) la variable es modelada a partir de si misma;
#(2) la media espacial no es conocida a priori, sino estimada de los datos.
summary(ok)
ok$var1.pred
ok$var1.var
ok$pred <- 10^(ok$var1.pred)#volver a valores originales

## Superficie de predicción y desviación tipica de la predicción
par(mfrow=c(2,1))
pts.s <- list("sp.points", meuse, col="white",pch=1, cex=4*meuse$zinc/max(meuse$zinc))
print(spplot(ok, "var1.pred", asp=1, col.regions=rev(bpy.colors(64)),
             main="Predicción OK, log-ppm Zn",sp.layout = list(pts.s)), 
      split=c(1,1,2,1), more=TRUE)
pts.s <- list("sp.points", meuse, col="black", pch=20)
print(spplot(ok, zcol="var1.var",col.regions=cm.colors(64), asp=1,
             main="Varianza OK, log-ppm Zn^2",sp.layout = list(pts.s)), 
      split=c(2,1,2,1), more=FALSE)       
##VALIDACION
# five-fold cross validation (map of residuals)
xvalid <- gstat::krige.cv(logZn~1, meuse, va, nmax=32, nfold=5)
summary(xvalid)
#R2
r2_nfold <- cor(xvalid$observed, xvalid$var1.pred)^2
#RMSE
Exp_rmse_nfold <- sqrt(mean(xvalid$residual^2))

### EXTRA
# Kriging Plan View
ok.plot <- spplot(ok["var1.pred"], col.regions=terrain.colors(50), scales=list(draw=T), p.layout=list("var1.pred", meuse, pch="+"), main = "Local Ordinary Kriging Estimation")
ok.plot             
# Kriging Variance
kv.plot <- spplot(ok["var1.var"], col.regions=terrain.colors(50), scales=list(draw=T), p.layout=list("var1.var", meuse, pch="+"), main = "Ordinary Kriging Variance")
kv.plot


#####KRIGING UNIVERSAL
train_g_U <- gstat(formula = logZn ~1 + x + y, data = meuse)
vg_U <- variogram(train_g_U)
plot(vg_U)
#Procedemos a ajustar un modelo teórico para Vg_U como en los pasos anteriores y luego aplicamos kriging
#UK <- krige(formula = logZn ~ 1 + x + y,
#            meuse, meuse.grid , model = vt) #vt es el modelo téorico ajustado y se obtiene con fit.variogram


### AUTOMATIZACION### Probamos varios modelos
vemp <- autofitVariogram(formula = logZn~1,input_data = meuse, model="Sph")
plot(vemp)
sph.model <- vgm(psill=0.11, model="Sph", range=890, nugget=0.01)
sph.fit <- fit.variogram(ve, sph.model)
#Krige the data
ok <- krige(logZn ~ 1, locations = meuse, newdata = meuse.grid, model = sph.fit)   
#Calcule R2 y RMSE
xvalid <- gstat::krige.cv(logZn~1, meuse, sph.fit, nmax=32, nfold=5)
summary(xvalid)
#R2
r2_nfold <- cor(xvalid$observed, xvalid$var1.pred)^2
#RMSE
Exp_rmse_nfold <- sqrt(mean(xvalid$residual^2))
#Repita el bloque para otros modelos teóricos y decida el ganador según desempeño.


####OTRA FORMA
fit_vgm <- fit.variogram(ve,model = vgm(model = c("Sph", "Exp", "Gau", "Nug", "Bes", "Wav")), fit.sills = TRUE, fit.ranges = TRUE, fit.kappa = TRUE, fit.method = 7)
fit_vgm
attr(fit_vgm, "SSErr") 
plot(ve, fit_vgm, lwd = 2, col = "2", pch = "*", cex = 3)