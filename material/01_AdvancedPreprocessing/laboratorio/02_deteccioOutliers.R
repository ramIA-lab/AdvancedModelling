# ==============================================================================
#                      PREPROCESSING: Detecció d'Outliers
# Author(s):     Sergi Ramírez i Dante Conti
#                        IDEAI (c)
# Date:               08 Febrer 2025
# Description:   Aquest script permet la detecció de outliers de la nostra 
#                base de dades
#
# ==============================================================================
# Carreguem les llibreries =====================================================
list.of.packages = c("EnvStats", "ggplot2", "outliers", "remotes", "scatterplot3d", 
                     "readr", "rgl", "plotly", "mvoutlier", "MVN", "chemometrics", 
                     "adamethods", "DMwR2", "dplyr", "Rlof", "R.matlab", "solitude", 
                     "tidyverse", "MLmetrics") 

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# Carreguem les bases de dades =================================================
dades <- read.csv("valentine_dataset.csv")

# Seleccio variables numériques ------------------------------------------------
tipus <- sapply(dades, class)
varNum <- names(tipus)[which(tipus %in% c("integer", "numeric"))]
varNum <- varNum[which(!varNum %in% c("Valentine_Date"))]
varCat <- names(tipus)[which(tipus %in% c("factor", "character"))]

# 1. Detecció Univariant =======================================================
## 1.1 Minims i màxims ---------------------------------------------------------
mapply(function(x, name) {
  cat("var. ", name, ": \n\t min: ", min(x), "\n\t max: ", max(x), "\n")
  invisible(NULL)  # Evita la salida de valores NULL
}, dades[, varNum], colnames(dades[, varNum]))

## 1.2 IQR ---------------------------------------------------------------------
## Es defineixen outliers els punts fora de [Q1 - 1.5xIQR, Q3 + 1.5xIQR]
library(EnvStats)

IQROutlier <- function(variable, rmnas = TRUE) {
  IQ <- iqr(variable, na.rm = rmnas)
  intInf <- quantile(variable, probs = c(0.25, 0.75))[[1]] - 1.5*IQ
  intSup <- quantile(variable, probs = c(0.25, 0.75))[[2]] + 1.5*IQ
  posicions <- which(variable >= intSup | variable <= intInf)
  if (length(posicions) > 0) {
    cat("Existeixen outliers en les posicions:", paste0(posicions, collapse = ", "))
  } else {
    cat("No existeixen outliers")
  }
  return(posicions)
}

IQROutlier(dades[, "Age"])

## 1.3 Boxplot -----------------------------------------------------------------
## Visualització basada en IQR per detectar outliers
library(ggplot2)

variable <- "Age"

boxplot(dades[, variable])
boxplot.stats(dades[, variable])$out

# Crear un boxplot
ggplot(dades, aes(y = get(variable))) +
  geom_boxplot(fill = "skyblue", color = "black") +
  labs(title = paste0("Boxplot de ", variable)) +
  theme_minimal()

## 1.4 Z-Score -----------------------------------------------------------------
## Un outlier es un valor amb |z| > 3 deviació estándar

variable <- "Age"
valorEscalado <- scale(dades[, variable])
hist(valorEscalado)

ggplot(data.frame(valor = valorEscalado), aes(x = valor)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +  # Histograma
  geom_vline(xintercept = c(3, -3), linetype = "dashed", color = "red", size = 1) + # Líneas horizontales
  theme_minimal()

## 1.5 Hampel Identifier -------------------------------------------------------
## Utilitza la mediana i la desviació absoluta mediana (MAD) en lloc de la mitjana
variable <- "Age"

lower_bound <- median(dades[, variable]) - 3 * mad(dades[, variable], constant = 1)
upper_bound <- median(dades[, variable]) + 3 * mad(dades[, variable], constant = 1)
outlier_ind <- which((dades[, variable] < lower_bound) | (dades[, variable] > upper_bound))
outlier_ind

## 1.6 Tests Estadístics -------------------------------------------------------
## 1.6.1 Grubbs'Test ...........................................................
## Detecta valors extrems en una distribució normal
library(outliers)

variable <- "Age"
test <- outliers::grubbs.test(dades[, variable], opposite = TRUE)
# amb el paràmetre opposite controles quina de les dues cues están buscant
test

## 1.6.2 Dixon's Test ...........................................................
## Només utilitzar per a bbdd petites (entre 3 - 30) observacions

variable <- "Age"
test <- outliers::dixon.test(dades[, variable], opposite = FALSE)
test

## 1.6.3 Rosner's Test ...........................................................
## La prueba de Rosner para valores atípicos tiene las ventajas de que:
##   1. se utiliza para detectar varios valores atípicos a la vez (a diferencia 
##   de la prueba de Grubbs y Dixon, que debe realizarse de forma iterativa para 
##   detectar múltiples valores atípicos), 
##   2. Está diseñado para evitar el problema del enmascaramiento, donde un valor 
##   atípico cercano en valor a otro valor atípico puede pasar desapercibido.
##
## A diferencia de la prueba de Dixon, tenga en cuenta que la prueba de Rosner 
## es más apropiada cuando el tamaño de la muestra es grande (n ≥ 20).
## 
## Esta función requiere al menos dos argumentos: 
## - los datos 
## - la cantidad de valores atípicos sospechosos k (k = 3 como cantidad predeterminada)

## Asumeix normalitat de les dades

library(EnvStats)

variable <- "Age"
test <- EnvStats::rosnerTest(dades[, variable], k = 1)
test
test$all.stats

# 2. Detecció Multivariant =====================================================
library(scatterplot3d)
library(readr)
dades <- readr::read_csv("https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv")
dades <- data.frame(dades[, c("DC", "temp", "RH")])
scatterplot3d(dades[,"DC"], dades[, "temp"], 
              dades[, "RH"])

# ..............................................................................
# library
library(rgl)

# Plot
plot3d(x = dades[, "DC"], y = dades[, "temp"], z = dades[, "RH"], 
  col = "black", type = 'p', radius = .1)

# ..............................................................................
library(plotly)

fig <- plotly::plot_ly(dades, x = ~DC, y = ~temp, z = ~RH, size = 1) %>% 
          add_markers()
fig

## 2.1 Cas general -------------------------------------------------------------
library(mvoutlier)
dades2 <- dades; Y <- as.matrix(dades2)
res <- aq.plot(Y)
#windows()
par(mfrow=c(1, 1))
library(MVN)
mvnoutliers <- mvn(dades, multivariateOutlierMethod = "adj", showOutliers = TRUE, 
                   showNewData = TRUE)
## Visualitzem tots els outliers detectats com a true
mvnoutliers$multivariateOutliers

## Visualitzem les variables originals quines han donat els que no son outliers
mvnoutliers$newData

## 2.1 ACP ---------------------------------------------------------------------
## Métodes basats en correlacions ens permeten detectar outliers
## 2.2 Distancia de Mahalanobis ------------------------------------------------
## Medeix la distancia de un punt respecte a la mitjana considerant la covariança

distancia_mahalanobis <- mahalanobis(dades, colMeans(dades), cov(dades))

## Grafiquem el plot de la densitat de les distancies
plot(density(distancia_mahalanobis))

## Es mostren els valors de la bbdd que queden per sobre de el 99% de la distribució
## chi-cuadrat
cutoff <- qchisq(p = 0.99, df = ncol(dades))
dades[distancia_mahalanobis>cutoff, ]

# Ordenamos de forma decreciente, según el score de Mahalanobis
dades <- dades[order(distancia_mahalanobis, decreasing = TRUE),]

# Visualitzem l'histograma de les distancies per veure on tallem els outliers
par(mfrow=c(1,1))
hist(distancia_mahalanobis)

# Descartamos los outliers según un umbral
umbral <- 8
dades[, "outlier"] <- (distancia_mahalanobis > umbral)

dades[, "color"] <- ifelse(dades[, "outlier"], "red", "black")
scatterplot3d(dades[, "DC"], dades[, "temp"], dades[, "RH"], 
              color = dades[, "color"])

fig <- plotly::plot_ly(dades, x = ~DC, y = ~temp, z = ~RH, 
                       color = ~color, colors = c('#0C4B8E', '#BF382A')) %>% 
add_markers()
fig

quienes <- which(dades[, "outlier"] == TRUE)
quienes

# Mahalanobis robust ...........................................................

library(chemometrics)

  dis <- chemometrics::Moutlier(dades[, c("DC", "temp", "RH")], quantile = 0.99, plot = TRUE)

par(mfrow = c(1, 1))
plot(dis$md, dis$rd, type = "n")
text(dis$md, dis$rd, labels = rownames(dades))
a <- which(dis$rd > 7)

## 2.3 Regresió Lineal i residus -----------------------------------------------
## Un punt amb un residu gran pot considerar-se un outlier
## 2.4 Distancia de Cook -------------------------------------------------------
## Identifica punts amb gran influència en la regresió. Un valor de Cook D_i > 1 és
## un outlier.
## 2.5 K-Nearest Neighbors (KNN) Outlier Score ---------------------------------
## Basats en la densitat local de les dades
library(adamethods)

do_knno(dades[, c("DC", "temp", "RH")], k=1, top_n = 30)

## 2.6 Local Outlier Factor (LOF) ----------------------------------------------
## Compara la densidat de un punt amb la densidat dels seus veïns. Un valor LOF alt
library(DMwR2)
library(dplyr)

outlier.scores <- lofactor(dades[, c("DC", "temp", "RH")], k = 5)
par(mfrow=c(1,1))
plot(density(outlier.scores))

outliers <- order(outlier.scores, decreasing=T)[1:5]
print(outliers)

### Aprofitarem el ACP per poder visualizar els outliers
n <- nrow(dades[, c("DC", "temp", "RH")]); labels <- 1:n; labels[-outliers] <- "."
biplot(prcomp(dades[, c("DC", "temp", "RH")]), cex = .8, xlabs = labels)

### Grafiquem les correlacions per veure els gráfics
pch <- rep(".", n)
pch[outliers] <- "+"
col <- rep("black", n)
col[outliers] <- "red"
pairs(dades[, c("DC", "temp", "RH")], pch = pch, col = col)

### Ho visualitzem en 3D
plot3d(dades[, "DC"], dades[, "temp"], dades[, "RH"], type = "s", col = col, size = 1)

# ..............................................................................
library(Rlof)
outliers.scores <- Rlof::lof(dades[, c("DC", "temp", "RH")], k = 5)
plot(density(outliers.scores))

## 2.7 Isolation Forest --------------------------------------------------------
### Cargamos las librerias necesarias
library(R.matlab)   # Lectura de archivos .mat
library(solitude)   # Modelo isolation forest
library(tidyverse)  # Preparación de datos y gráficos
library(MLmetrics)

# Carreguem les dades
cardio_mat  <- readMat("https://www.dropbox.com/s/galg3ihvxklf0qi/cardio.mat?dl=1")
df_cardio   <- as.data.frame(cardio_mat$X)
df_cardio$y <- as.character(cardio_mat$y)
datos <- df_cardio

# Modelo isolation forest
isoforest <- isolationForest$new(
  sample_size = as.integer(nrow(datos)/2),
  num_trees   = 500, 
  replace     = TRUE,
  seed        = 123
)
isoforest$fit(dataset = datos %>% select(-y))

# Predicción
predicciones <- isoforest$predict(
  data = datos %>% select(-y)
)
head(predicciones)

ggplot(data = predicciones, aes(x = average_depth)) +
  geom_histogram(color = "gray40") +
  geom_vline(
    xintercept = quantile(predicciones$average_depth, seq(0, 1, 0.1)),
    color      = "red",
    linetype   = "dashed") +
  labs(
    title = "Distribución de las distancias medias del Isolation Forest",
    subtitle = "Cuantiles marcados en rojo"  ) +
  theme_bw() +
  theme(plot.title = element_text(size = 11))

cuantiles <- quantile(x = predicciones$average_depth, probs = seq(0, 1, 0.05))
cuantiles

## Detección de anomalías
#### Una vez que la distancia de separación ha sido calculado, se puede emplear 
#### como criterio para identificar anomalías. Asumiendo que las observaciones 
#### con valores atípicos en una o más de sus variables se separan del resto 
#### con mayor facilidad, aquellas observaciones con menor distancia promedio 
#### deberían ser las más atípicas.

#### En la práctica, si se está empleando esta estrategia de detección es 
#### porque no se dispone de datos etiquetados, es decir, no se conoce qué 
#### observaciones son realmente anomalías. Sin embargo, como en este ejemplo 
#### se dispone de la clasificación real, se puede verificar si realmente los 
#### datos anómalos tienen menores distancias.

datos <- datos %>%
  bind_cols(predicciones)

ggplot(data = datos,
       aes(x = y, y = average_depth)) +
  geom_jitter(aes(color = y), width = 0.03, alpha = 0.3) + 
  geom_violin(alpha = 0) +
  geom_boxplot(width = 0.2, outlier.shape = NA, alpha = 0) +
  stat_summary(fun = "mean", colour = "orangered2", size = 3, geom = "point") +
  labs(title = "Distancia promedio en el modelo Isolation Forest",
       x = "clasificación (0 = normal, 1 = anomalía)",
       y = "Distancia promedio") +
  theme_bw() + 
  theme(legend.position = "none",
        plot.title = element_text(size = 11)
  )

#### La distancia promedio en el grupo de las anomalías (1) es claramente inferior. 
#### Sin embargo, al existir solapamiento, si se clasifican las n observaciones 
#### con menor distancia como anomalías, se incurriría en errores de falsos positivos.

#### Acorde a la documentación, el set de datos Cardiotocogrpahy contiene 176 
#### anomalías. Véase la matriz de confusión resultante si se clasifican como 
#### anomalías las 176 observaciones con menor distancia predicha.

resultados <- datos %>%
  select(y, average_depth) %>%
  arrange(average_depth) %>%
  mutate(clasificacion = if_else(average_depth <= 8.5, "1", "0"))

mat_confusion <- MLmetrics::ConfusionMatrix(
  y_pred = resultados$clasificacion,
  y_true = resultados$y)

mat_confusion

# Bibliografia =================================================================
## https://statsandr.com/blog/outliers-detection-in-r/#z-scores
## https://m-mburu.github.io/datacamp/anomaly_detection_R/anomaly_detection.html
## https://github.com/pridiltal/ctv-AnomalyDetection
