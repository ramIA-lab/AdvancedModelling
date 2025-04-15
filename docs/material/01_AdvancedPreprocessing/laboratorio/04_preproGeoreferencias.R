# ==============================================================================
# [IDEAI]          preproGeoreferencias.R
# 
# Author/s:        Sergi Ramirez Mitjans (c)
# Date:            07 de Febrero de 2023
# Description:     Este script permite realizar preprocesamiento de aquellas vars
#                  que son georeferenciadas
# ==============================================================================
# Load the packages
list.of.packages = c("ggmap", "sf", "osmdata", "tidyverse", "Hmisc", "tmap", 
                     "tmaptools", "cli") 
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages) > 0) {
  install.packages(new.packages)
}
lapply(list.of.packages, require, character.only = T)
rm(list.of.packages, new.packages)

# ==============================================================================
# Method 1: Detect gym and hospitals ubication in Mataró
ubication <- "Mataró"

##  Lookt the five first features avaible
head(available_features())

## In this link you can search for the characteristics you want to find
### https://wiki.openstreetmap.org/wiki/ES:Objetos_del_mapa

## Create the query
q <- getbb(ubication) %>% 
  opq() %>%
  add_osm_feature("leisure", "fitness_centre")

q2 <- getbb(ubication) %>% 
  opq() %>%
  add_osm_feature("amenity","hospital")
    
## Look the query structure
str(q) 

## Download the data with query
fitness <- osmdata_sf(q)
hospital <- osmdata_sf(q2)

# Convertir el bbox a un objeto sf (Simple Features)
bbox <- getbb(ubication)
bbox_sf <- st_as_sfc(st_bbox(c(xmin = bbox[1,1], ymin = bbox[2,1], 
                               xmax = bbox[1,2], ymax = bbox[2,2]), 
                             crs = st_crs(4326)))

# Visualizar el mapa con OpenStreetMap y añadir puntos de interés
tmap_mode("view")

# Cargar el mapa base fuera de la cadena de capas
tm_basemap("OpenStreetMap") +
  
  # Mostrar el bounding box
  tm_shape(bbox_sf) +
  tm_borders() +
  
  # Visualizar puntos de fitness
  tm_shape(fitness$osm_points) +
  tm_dots(col = "#238443", size = 0.4, alpha = 0.7, shape = 21, border.col = "#004529") +
  
  # Visualizar puntos de hospitales
  tm_shape(hospital$osm_points) +
  tm_dots(col = "#FF0000", size = 0.4, alpha = 0.7, shape = 21, border.col = "#FF0000") +
  
  # Personalización del mapa
  tm_layout(title = paste("Mapa de", ubication),
            legend.outside = TRUE)

# Eliminamos los objetos
rm(q2, q, hospital, fitness, bbox_sf, bbox)

# ==============================================================================
# Validate coordinates
library(leaflet)

# Extraemos todos los gimnasios de Barcelona de la companyia DIR
q <- getbb("Barcelona") %>% 
  opq() %>%
  add_osm_feature("amenity", "driving_school") 
  
autoescuelas <- osmdata_sf(q)$osm_points

# Convertir geometría a coordenadas
coords <- st_coordinates(autoescuelas)

# Añadir las coordenadas al data frame
autoescuelas$longitude <- coords[, 1] # Longitud (X)
autoescuelas$latitude <- coords[,2]  # Latitud (Y)

# Validamos las coordenadas de nuestros datos
leaflet::validateCoords(lng = coords[,1], lat = coords[,2])

# Comprobar que la longitud esté entre -180 y 180 grados
all(coords[,1] >= -180 & coords[,1] <= 180) # Debe devolver TRUE

# Comprobar que la latitud esté entre -90 y 90 grados
all(coords[,2] >= -90 & coords[,2] <= 90) # Debe devolver TRUE

# Detectamos si hay na's
sum(is.na(coords)) # Debe ser 0

# Convertir el bbox a un objeto sf (Simple Features)
bbox <- getbb("Barcelona")
bbox_sf <- st_as_sfc(st_bbox(c(xmin = bbox[1,1], ymin = bbox[2,1], 
                               xmax = bbox[1,2], ymax = bbox[2,2]), 
                             crs = st_crs(4326)))

# Visualizar el mapa con OpenStreetMap y añadir puntos de interés
tmap_mode("view")

# Cargar el mapa base fuera de la cadena de capas
tm_basemap("OpenStreetMap") +
  
  # Mostrar el bounding box
  tm_shape(bbox_sf) +
  tm_borders() +
  
  # Visualizar puntos de fitness
  tm_shape(autoescuelas) +
  tm_dots(col = "#5B88FF", size = 0.4, alpha = 0.7, shape = 21, border.col = "#0046FF") +
  
  # Personalización del mapa
  tm_layout(title = paste("Mapa de", ubication),
            legend.outside = TRUE)

# ==============================================================================
# Method 2: Extract coordinates to adresses and print the information in maps
#### Madrid buildings (https://dominicroye.github.io/es/2019/visualizar-el-crecimiento-urbano/)
# Instalar los paquetes necesarios si no los tienes aún
install.packages(c("httr", "jsonlite"))

# Cargar las librerías
library(httr)
library(tmaptools)
library(jsonlite)

# Definir la URL de la API
url <- "https://opendata-ajuntament.barcelona.cat/data/api/action/datastore_search?resource_id=66f5e7e3-045b-4d19-b649-2eaea622ae93&limit=5570"

# Realizar la solicitud a la API
respuesta <- GET(url)

# Comprobar si la solicitud fue exitosa (código de estado 200)
if (status_code(respuesta) == 200) {
  # Convertir la respuesta a formato JSON
  datos <- content(respuesta, as = "text", encoding = "UTF-8")
  datos_json <- fromJSON(datos)
  
  # Mostrar los datos obtenidos
  print(datos_json)
} else {
  cat("Error en la solicitud. Código de estado:", status_code(respuesta))
}

### Nos quedamos con el nombre de las columnas 
dades <- datos_json$result$records

##### we are left with the necessary columns
cols <- c("Nom_barri", "Nom_districte", "Num_postal", "Nom_carrer")
accBCN <- dades[, cols]

#### Preprocessing
#### Creamos las direcciones correspondientes 
qui <- which(accBCN$Num_postal != "")
accBCN[qui, "direccion"] <- paste0(accBCN[qui, "Nom_carrer"], " nº ", 
                                   accBCN[qui, "Num_postal"], ", ", 
                                   accBCN[qui, "Nom_barri"], ", ",  
                                   accBCN[qui, "Nom_districte"], ", Barcelona")

qui <- which(accBCN$Num_postal == "")
accBCN[qui, "direccion"] <- paste0(accBCN[qui, "Nom_carrer"], " nº ", 
                                   accBCN[qui, "Nom_barri"], ", ",  
                                   accBCN[qui, "Nom_districte"], ", Barcelona")

### Para hacer el trabajo, realizaremos un sample de la base de datos 
set.seed(1240)
accBCN <- accBCN[sample(1:nrow(accBCN), size = 100), ]

#### Calculate the coordinates
## Mediante la api de OpenStreetMap
### Creamos la función que nos permite geolocalizar los datos
pb = txtProgressBar(min = 1, max = length(1:nrow(accBCN)), 
                    initial = 1, style = 3) 

for (i in 1:nrow(accBCN)) {
  osm_reply <- suppressMessages(geocode_OSM(accBCN[i, "direccion"], 
                                projection = 4326, as.data.frame = TRUE, 
                           server = "http://nominatim.openstreetmap.org"))
  if (length(osm_reply) > 0) {
    accBCN[i, "latitude"] <- osm_reply$x
    accBCN[i, "longitude"] <- osm_reply$y  
  } else {
    accBCN[i, "latitude"] <- ''
    accBCN[i, "longitude"] <- ''
  }
  setTxtProgressBar(pb, i)
}
close(pb)

# ==============================================================================
