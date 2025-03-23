# ==============================================================================
# [IDEAI] -  Advanced clustering algorithms - DBSCAN & OPTICS 
# 
# Author(s):    Dante Conti and Sergi Ramírez, IDEAI  (c)
# Date:         10th March 2023
# Description: 
#             Este script trata los algoritmos de clustering por densidad 
#             DBSCAN y OPTICS
# ==============================================================================
# Cargamos las librerias necesarias
library(cluster)
library(fpc)
library(pracma)
library(factoextra)
library(dbscan)

# =============================================================================
### Generamos una semilla para poder ejecutar los datos
set.seed(04102022)

# ==============================================================================
### Creamos la base de datos que vamos a utilizar para detectar los grupos
data("multishapes"); help("multishapes")
datos <- multishapes[, 1:2]

### Printamos la imagen que hemos obtenido de los datos a clasificar
ggplot2::ggplot(datos, aes(x = x, y = y)) + 
  ggplot2::geom_point(color='#3333FF')

# ==============================================================================
# KMEANS: 
### Graficamos los datos a traves de un k-means para visualizar como quedarian los 
### grupos cuando utilizamos unos algoritmos de agrupacion a partir de la inercia
km_clusters <- kmeans(x = datos, centers = 5, nstart = 50)
fviz_cluster(object = km_clusters, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
             theme_bw() + theme(legend.position = "none")

### Como podemos ver, Kmeans ha hecho una muy mala clusterizacion, puesto que
###  - No ha conseguido clusterizar segun las formas complejas del modelo.
###  - No ha tenido en cuenta que existen outliers, incluyendolos en los distintos clusters

# ==============================================================================
# DBSCAN: 

### DBSCAN parte de dos parametros que son: 
### - eps: distancia maxima a la que debe haber otra observacion para ser considerar 
###        que cumple con el criterio de "estar cerca"
### - minPts: parametro que controla la densidad minima requerida para que un punto 
###           sea considerado un nucleo y se incluya en un grupo/cluster.

### Para un punto p, si existen al menos minPts puntos dentro del radio eps alrededor de p, 
### entonces p se considera un nucleo (core point) y se incluye en el mismo grupo/cluster 
### que los demas puntos dentro del radio eps. 
### Si no hay suficientes puntos dentro del radio eps, p se considera un punto frontera (border point) 
### y se incluye en el mismo grupo/cluster que su punto nucleo mas cercano. 
### Si no hay ningun punto dentro del radio eps, p se considera un punto de ruido (noise point) 
### y no se incluye en ningun grupo/cluster.

### Selección del valor óptimo de epsilon. Como valor de minPts se emplea 5.
dbscan::kNNdistplot(datos, k = 5)

### Aplicamos el algoritmo de dbscan para classificar los datos
dbscan_res <- dbscan::dbscan(datos, eps = 0.15, minPts = 5)

### Graficamos el dbscan obtenido 
fviz_cluster(object = dbscan_res, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
             theme_bw() +  theme(legend.position = "none")

### Para escoger los valores de eps i minPts, necesitaremos optimizar el proceso. Para ello, 
### realizaremos la siguiente tecnica de optimizacion. 

# ---------------------------------------------------------------------------------------------------
# Calculo de min_pts

### El parametro min_pts establece el numero de puntos minimo que, dado un radio eps, tiene
### que haber para que se considere que dichos puntos forman un cluster.
### Un valor bajo de min_pts asegurara que mas puntos son agrupados, pero se corre el riesgo de 
### agrupar outliers. Por el contrario, un valor muy alto de min_pts puede descartar valores que 
### no son anomalos.

### En la literatura hablan de usar un valor entre 3 y 5 ya que funcionan bastante 
### bien en la mayoria de los casos. min Pts igual 2 cuando tenemos una distribucion 
### normal y otra nube de outliers

### Para calcularlo de manera empirica, diremos que el minimo de puntos sea igual 
### al 0.2% - 0.25% del total de los datos teniendo en cuenta que: 

### - El minimo sera de 2 para datos que sean muy pequeños
### - El maximo sera de 10 para datos con mucha información o quizás
### un poco más dependiendo del tamaño de la base de datos

#### Calculo de min_pts
porcentaje <- 0.0025 

# Calculo de min_pts. 
(min_pts <- round(nrow(datos) * porcentaje))

### Realizamos los cortes de 2 y 10 que se mencionan anteriormente como validación
min_pts <- ifelse(min_pts <= 1, 2, min_pts)
min_pts <- ifelse(min_pts >= 10, 10, min_pts);min_pts


# ---------------------------------------------------------------------------------------------------
# Normalizacion de los datos (SIEMPRE HAY QUE HACERLO)
### Cuando trabajamos con distancias es aconsejable normalizar los datos para que
### ninguno tenga un peso que no le corresponde
datos_norm <- data.frame(lapply(datos, scales::rescale)); head(datos_norm)

### Como podemos ver, ahora tendremos los valores entre el intervalo [0, 1]

# -----------------------------------------------------------------------------
# Calculo de la Epsilon (eps)
### Realizamos el calculo de las distancias mas cercanas en una matriz de puntos
#### distanciasVecinas <- dbscan::kNNdist(datos, k = min_pts)

### Ordenamos los puntos de menos a mayor y lo guardamos en un vector.
### Cuando realicemos el grafico elbow, sera nuestro eje de las Y
#### Y <- distanciasVecinas[order(distanciasVecinas)]

### Calculamos el Indice del eje de la X
#### X <- c(0:(length(Y) - 1))

### A continuacion calculamos las pendientes
#### pendientes <- c()
#### for (i in 1:length(X)) {
####	pendientes[i] <- (Y[i + 1] - Y[i])/(X[i+1] - X[i])
#### }

#### m <- which.max(pendientes)
#### primer <- gdata::first(which(pendientes >= m))
#### epsilon <- Y[primer]
### NoTa, ejecutar lineas antecedentes para decidir el corte en el máximo
### cambio de la pendiente, como podeís apreciar ocurre alrededor de 0.15
### Esto se hace trazando una recta horizontal desde el mayor cambio y viendo
### su valor en el eje Y
### Graficamos los epsilons ordenados
kNNdistplot(datos, k = 5, minPts = min_pts)
  abline(h = 0.15, lty = 2, col = "red")

### Mirando el grafico elbow vemos que el epsilon es 0.15
epsilon <- 0.15

# -----------------------------------------------------------------------------
### Volvemos a ejecutar el DBSCAN con los parametros optimos
res <- dbscan(datos, eps = epsilon, minPts = min_pts) 

### Aqui podeis graficar los resultados como se hizo en lineas precedentes
### Añado la columna cluster a mis datos.
datos[, "cluster"] <- res[["cluster"]]

### Guardo datos limpios.
datos_limpios <- dplyr::filter(datos, cluster != 0)

### Guardo outliers.
outliers <- dplyr::filter(datos, cluster == 0) 

### Graficamos el dbscan obtenido. Es el mismo gráfico anterior pero en PCA 
fviz_cluster(object = res, data = datos, geom = "point", ellipse = FALSE,
             show.clust.cent = FALSE, pallete = "jco") +
             theme_bw() +  theme(legend.position = "none")

### Otra manera de visualizar los clusters obtenidos
hullplot(datos, res$cluster, main = paste0("Convex cluster Hulls, eps = ", epsilon))

# =============================================================================
# OPTICS

### Ejecutamos el algoritmo OPTICS con un radio de vecindad de 0.5 y un numero 
### minimo de puntos de 5
optics <- dbscan::optics(datos, eps = 0.5, minPts = 5)
#### optics <- optics::optics(data, eps = 0.5, minPts = 5)

# -----------------------------------------------------------------------------
### Creamos un grafico que muestra la distancia alcanzable de cada punto
plot(optics, reachability = TRUE)

# -----------------------------------------------------------------------------
### Optimizamos la busqueda de parametros para epsilon y minPts en Optics
library(doParallel)
library(foreach)

### Definimos los valores que se van a probar para eps y minPts para conformar un
### Grid Search
eps_values <- seq(0.1, 1.0, by = 0.1)
eps_values
minPts_values <- seq(5, 20, by = 5)
minPts_values

### Crear una cuadricula de busqueda de los valores de eps y minPts
grid <- expand.grid(eps = eps_values, minPts = minPts_values)
grid
### Establecemos el numero de nucleos que se van a usar para realizar la optimizacion en paralelo
cores <- detectCores()
cores
registerDoParallel(cores = cores)

### Creamos una funcion para ejecutar OPTICS con una combinacion de parametros y 
### calcular el coeficiente de silueta. 
### Esta función puede adaptarse por si se desea aplicar una grid search similar en DBSCAN
run_optics <- function(data, eps, minPts) {
  optics <- dbscan::optics(data, eps = eps, minPts = minPts)
  res <- dbscan::extractDBSCAN(optics, eps_cl = eps)
  sil <- cluster::silhouette(res$cluster, dist(data))
  return(ifelse(is.na(sil), sil, mean(sil[, 3])))
}
### Con esta funcion nos permitira luego paralelizar el proceso

### Ejecutar la cuadricula de busqueda en paralelo para la funcion dada
results <- foreach(i = 1:nrow(grid), .combine = rbind) %dopar% {
  eps <- grid$eps[i]
  minPts <- grid$minPts[i]
  score <- run_optics(datos[, -3], eps, minPts)
  c(eps, minPts, score)
}

results <- results[, c(1:3)]
results
### Seleccionamos la combinacion de parametros que produjo el mejor resultado
(best_params <- grid[which.max(results[, 3]), ])

### Creamos el modelo con los mejores parametros
optics <- dbscan::optics(datos, eps = best_params$eps, minPts = best_params$minPts)
best_params$eps
best_params$minPts
## Aqui debeis rescatar los resultados del cluster pero debias decidir donde cortar
## el reachability plot
res <- dbscan::extractDBSCAN(optics, eps_cl = 0.3)
plot(res)
(res$cluster)
table(res$cluster)
# -----------------------------------------------------------------------------
### Metodo de la silueta

#### Ejecutar OPTICS para diferentes valores de eps
eps_values <- seq(0.1, 1, by = 0.1)
optics_results <- lapply(eps_values, function(e) optics(datos[, -3], eps = e, minPts = 5))

#### Obtener los agrupamientos para cada valor de eps
clusters <- lapply(optics_results, function(x) extractDBSCAN(x, eps = x$eps))

#### Calcular la medida de silhouette promedio para cada valor de eps
silhouette_avg <- sapply(clusters, function(x) mean(cluster::silhouette(x$cluster, dist(datos[, -3]))))

## Obtener los índices de los valores menores o iguales a 1
indices <- which(silhouette_avg <= 1)

# Graficar la medida de silhouette promedio en funcion de eps
plot(eps_values[indices], silhouette_avg[indices], type = "b", pch = 20, main = "Silhouette Plot")

# Agregar una linea vertical en el valor optimo de eps, el que maximiza la silhoutte
## o podeis escoger un valor arbitrario para cortar el reachability plot y probar
## diferentes configuraciones que os daran diferentes cantidades de clusters.

# Obtener el índice del máximo dentro de esos valores
opt_eps <- eps_values[indices[which.max(silhouette_avg[indices])]]

# Fijaros que del gráfico anterior se aconseja cortar en 0.40 (ya que maximiza)
# la silhoutte
abline(v = opt_eps, lty = 2, col = "red")

# -----------------------------------------------------------------------------------
### extract a DBSCAN clustering by cutting the reachability plot at eps_cl
### La elección del eps_cl puede obtenerse segun lo explicado anteriormente
### o podeis "jugar con el obtener diferentes configuraciones de cluster como
### se hace con los dendogramas de clustering jerarquico.
opt_eps
res <- dbscan::extractDBSCAN(optics, eps_cl = 0.4)

### black is noise
plot(res)  

### Visualizamos el grÃ¡fico con los grupos creados
dbscan::hullplot(datos, res)
res$cluster
table(res$cluster)
## No olvideis guardar los resultados del clustering como se hizo con DBSCAN
### Aqui se han formado 5 clusters, uno de ellos es NOISE o posibles OUTLIERS.
### Conviene cortar el reachability plot con otros valores diferentes de 0.4
### para obtener más clustering. 
# ==============================================================================