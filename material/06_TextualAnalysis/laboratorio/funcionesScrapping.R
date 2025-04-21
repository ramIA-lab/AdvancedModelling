genius_get <- function(endpoint, query = list(), token = NULL) {
  url <- modify_url("https://api.genius.com", path = paste0("search"), query = query)
  res <- GET(url, add_headers(Authorization = paste("Bearer", token)))
  stop_for_status(res)
  fromJSON(content(res, as = "text"), flatten = TRUE)
}

# ==============================================================================
# Función que permite el scrapping de la letra desde el HTML
get_lyrics <- function(song_url) {
  page <- try(read_html(song_url), silent = TRUE)
  if (inherits(page, "try-error")) return(NA)
  
  lyrics <- page %>%
    html_elements("div.lyrics, div[class^='Lyrics__Container']") %>%
    html_text2()
  
  lyrics <- paste(lyrics, collapse = "\n")
  return(lyrics)
}

# ==============================================================================
get_genius_artist_songs <- function(artist_id, access_token = Sys.getenv('GENIUS_API_TOKEN')){
  
  page <- 1
  info <- list()
  
  while(page > 0){
    
    res <- RETRY('GET', url = str_glue('https://api.genius.com/artists/{artist_id}/songs'),
                 query = list(per_page = 50,
                              page = page,
                              access_token = access_token),
                 quiet = TRUE) %>% content
    
    if(res$meta$status != 200){stop(str_glue('Information not found for artist_id: {artist_id}'))}
    
    if (length(res$response$songs) != 0) {
      
      songs <- purrr::map_df(seq(length(res$response$songs)), function(this_song){
        info <- res$response$songs[[this_song]]
        artist <- info$primary_artist
        
        list(id = info$id, title = info$title,
             url = info$url, full_title = info$full_title,
             lyrics_state = info$lyrics_state,
             annotation_count = info$annotation_count,
             artist_id = artist$id,
             artist_name = artist$name,
             artist_url = artist$url,
             artist_image_url = artist$image_url)
      })
      
      info[[page]] <- songs
      
      if(!is.null(res$response$next_page)){
        page <- res$response$next_page
      } else{ break() }
    } else {
      break()  
    }
  }
  
  max_art <- purrr::compose(names, which.max, table)
  info <- purrr::map_df(info, bind_rows) %>% filter(artist_name == max_art(artist_name))
  return(info)
  
}

# ==============================================================================
# Función del preprocessing 
preproLyrics <- function(letra) {
  
  # Dividimos por el salto de linia
  letra <- strsplit(letra, split = "\n")[[1]]
  
  # Buscamos el inicio de la letra. Para ello empezamos con 
  ## Intro 
  existeIntro <- grep("[Intro", letra, fixed = T)
  
  if (length(existeIntro) > 0) {
    letra <- letra[(existeIntro + 1):length(letra)]
  } else {
    ## si no existe Intro entonces "[Verse 1"
    # letra <- letra[(grep("[Verse 1", letra, fixed = T) + 1):length(letra)]
    letra <- letra[(grep("[", letra, fixed = T)[1] + 1):length(letra)]
  }
  
  # Preprocesamos los datos 
  letra <- letra[-grep("[", letra, fixed = T)]
  
  # Eliminamos las que sean vacias 
  letraF <- c()
  for (let in letra){
    if (let != "") {
      # texto_modificado <- gsub("([a-z])(?=([A-Z]|[[:punct:]]))", "\\1\n", let, perl = TRUE)
      # texto_modificado <- gsub("(?=([a-z]|[[:punct:]]))(?=([A-Z]|[[:punct:]]))", "\\1\n", let, perl = TRUE)
      texto_modificado <- gsub("([a-z)])(?=([A-Z[:punct:]]))", "\\1\n", let, perl = TRUE)
      # print(texto_modificado)
    }
    letraF <- c(letraF, texto_modificado)
  }
  
  # Creamos el DF correspondiente
  # df <- rbind(df, data.frame(artista = artista, titulo = titulo, letra = paste0(letra, collapse = "\n"), stringsAsFactors = FALSE))
  return(paste0(letraF, collapse = "\n"))
}

# ==============================================================================
# Función principal para buscar canciones y letras
buscar_letras <- function(dfCanciones) {
  for (i in seq_len(nrow(dfCanciones))) {
    hit <- dfCanciones[i, ]
    
    titulo <- hit[1, "title"]
    url <- hit[1, "url"]
    cat("Buscando:", titulo, "\n")
    if (hit$lyrics_state[1] == "complete") {
      letra <- get_lyrics(url)
      letra <- preproLyrics(letra)
    } else {
      letra <- ""
    }
    dfCanciones[i, "Letra"] <- letra  
  }
  return(dfCanciones)
}
