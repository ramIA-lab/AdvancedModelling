# ==============================================================================
#
#
#
#
# ==============================================================================
# Cargamos las librerias necesarias 
library(geniusr)
library(dplyr)
library(tidytext)

install.packages("devtools")
devtools::install_github("JosiahParry/geniusr")

# Cargamos los paquetes necesarios
install.packages("remotes")
remotes::install_github('AlbertoAlmuinha/rgenius')
library(rgenius)

library(geniusr)
library(dplyr)

# Tu token de Genius API
library(httr)
library(jsonlite)
library(rvest)
library(dplyr)
library(stringr)

source("material/06_TextualAnalysis/laboratorio/funcionesScrapping.R")

# ==============================================================================
# PASO 1: Haceros el perfil de GENIUS
# Tu Genius API token
token <- "S4KNAxQXIWFbXpskNeH1pLE7TkfOqYVcqqA5kEtgJPtrotcppBkGYMoLxpZal5ZT"

# Buscamos el id del artista
artista <- "Lola Indigo"
dfCantantes <- search_artist(search_term = "Lola Índigo", n_results = 20)
id <- as.numeric(dfCantantes[grep(artista, dfCantantes$artist_name), "artist_id"])

# BUscamos todas las canciones de la artista
canciones <- get_genius_artist_songs(as.character(id)) %>% as.data.frame()
dfFinal <- buscar_letras(canciones)

# 🔍 Ejemplo: múltiples artistas
View(dfFinal)  # o usa head(df_total)
