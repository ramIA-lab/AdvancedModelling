# ==============================================================================
# [PMAAD] - TimesSeriesClutering.R
# 
# Author(s):    Sergi Ramírez, IDEAI  (c)
# Date:         12th March 2024
# Description: 
# ==============================================================================
# Carreguem les llibreries =====================================================
list.of.packages = c("dtw","tidyverse", "dendextend", "dtwclust")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

library(dtw)
library(tidyverse)
library(dendextend)
library(dtwclust)

# ==============================================================================
# Carreguem la base de dades
dad <- read.csv2("https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/2013.csv?nocab=1")
dad <- dad[which(dad$Establecimientos.y.personal.empleado..plazas. == "Número de habitaciones estimadas"), ]
dad[, 2] <- NULL

# ==============================================================================
# Transformem la bbdd a la taula necessaria
dad[, "Total"] <- as.numeric(dad[, "Total"])
dad[, "Zonas.Turísticas"] <- gsub(":", "_", dad[, "Zonas.Turísticas"])
dad[, "Zonas.Turísticas"] <- gsub(" ", "", dad[, "Zonas.Turísticas"])
datos <- reshape2::dcast(dad, Zonas.Turísticas ~ Periodo, fun.aggregate = sum, na.rm = TRUE)
rownames(datos) <- datos[, "Zonas.Turísticas"]
datos[, "Zonas.Turísticas"] <- NULL

rm(dad);gc()

# Detectamos que existen muchas columnas en 0, vamos a buscar aquellas columnas para
# poderlas eliminar 
colSums(datos) == 0

## Detectamos que tenemos todo 0 hasta 2014M12. Por lo tanto, vamos a eliminar todas
## las columnas anteriores
quienes <- 1:which(colnames(datos) == "2014M12")
datos <- datos[, -quienes]
dim(datos)
head(datos)

# ==============================================================================
# Calculamos la distancia DTW
# Visualiza las distintas similitudes y distancias que maneja el paquete
summary(pr_DB)
distMatrix <- proxy::dist(datos, method = "DTW")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")
# Put the labels at the same height: hang = -1
plot(hcc, hang = -1, cex = 0.6) 
hc1 <- hclust(distMatrix, method = "complete")
hc2 <- hclust(distMatrix, method = "single")
hc3 <- hclust(distMatrix, method = "average")

## Try the several options for method and select the highest
help(hclust) 
# cor(distMatrix,cophenetic(hc))
cor(distMatrix, cophenetic(hcc))
cor(distMatrix, cophenetic(hc1))
cor(distMatrix, cophenetic(hc2))
cor(distMatrix, cophenetic(hc3))

# Realizamos el corte en 3 clases # Vea que aunque el método "average" es mejor
# que Ward, hacemos el ejercicio académico con Ward para ver la visualización final de
# los clusters con un mapa previamente realizado.
k <- 3
clusters <- cutree(hcc, k = k)

# ..............................................................................
# Clusterisation using k variables
datos %>% 
  proxy::dist(method = "DTW") %>% 
  hclust(method = "ward.D2") %>% 
  as.dendrogram() -> dend

dend %>% set("branches_k_color", k = k) %>% plot()

# ------------------------------------------------------------------------------
# Calculamos la distancia del coseno
distMatrix <- 1 - proxy::simil(datos, method = "cosine")

# Generamos el clustering
hcc <- hclust(distMatrix, method = "ward.D2")

# Put the labels at the same height: hang = -1
plot(hcc, hang = -1, cex = 0.6) 
hc1 <- hclust(distMatrix, method = "average")
cor(distMatrix, cophenetic(hcc))
cor(distMatrix, cophenetic(hc1))

# Realizamos el corte en 3 clases
k <- 3
clusters <- cutree(hcc, k = k)

# ==============================================================================
## Partitional
pc <- tsclust(datos, type = "partitional", k = 4, 
              distance = "dtw_basic", centroid = "pam", 
              seed = 3247L, trace = TRUE,
              args = tsclust_args(dist = list(window.size = 20L)))
plot(pc)
pc@clusinfo

# ------------------------------------------------------------------------------
## Hierarchical
hc <- tsclust(datos, type = "hierarchical", k = 3L, 
              distance = "dtw", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc)
hc@clusinfo
hc@cluster
cutree(hc, k = 3)
plot(hc, type = "sc")

# ..............................................................................
# probemos con K=4
hc <- tsclust(datos, type = "hierarchical", k = 4L, 
              distance = "dtw", trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
plot(hc)
hc@clusinfo
hc@cluster
plot(hc, type = "sc")

# ..............................................................................
# probemos con K=3
hc2 <- tsclust(datos, type = "hierarchical", k = 3L, 
              distmat = distMatrix, trace = TRUE,
              control = hierarchical_control(method = "ward.D2"))
hc2@clusinfo
hc2@cluster
plot(hc2,type="sc")

# ..............................................................................
# probemos con K=4
hc3 <- tsclust(datos, type = "hierarchical", k = 4L, 
               distmat = distMatrix, trace = TRUE,
               control = hierarchical_control(method = "ward.D2"))
plot(hc3)
hc3@clusinfo
plot(hc3,type = "sc")
# ==============================================================================
# Bibliografia: 
## https://github.com/asardaes/dtwclust
## https://rpubs.com/Edison-D/615477
## https://rpubs.com/sebas_Alf/684217
## http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
## https://plotly.com/ggplot2/dendrogram/
## https://cran.r-project.org/web/packages/dendextend/vignettes/dendextend.html

# ==============================================================================